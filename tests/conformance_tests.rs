use easl::compiler::core::load_easl_program_from_file_with_lookup_function;
use easl::compiler::program::CompilerTarget;
use easl::interpreter::run_program_with_capture;
use std::fs;
use std::path::Path;
use std::process::Command;

const BOILERPLATE: &str = "
@[storage-write 0 0]
(var internal-return-value: f32)

@cpu
(defn main []
  (print (f))
  (dispatch-compute-shader (fn [] (= internal-return-value (f))) (vec3u 1u))
  (print internal-return-value))
";

fn run_conformance_test(name: &str, tolerance: f64) {
  let source_path_str = format!("./data/conformance/{name}.easl");
  let source_path = Path::new(&source_path_str);

  let user_source = fs::read_to_string(source_path)
    .unwrap_or_else(|e| panic!("IO error, couldn't read file {name}: {e:?}"));
  let combined_source = user_source.clone() + BOILERPLATE;

  fs::create_dir_all("./out/conformance/")
    .expect("Unable to create out directory");

  // Run every backend independently, collecting `Result<f64, String>` for
  // each. Each backend keeps going on its own — none of them can panic the
  // test before the others have run. We compare them only at the end so
  // failure messages show all four results side-by-side, making it
  // obvious whether (e.g.) the interpreter is the odd one out vs the
  // VM/C/GPU.
  let interpreter_result = run_interpreter(name, source_path, &combined_source);
  let gpu_result = match &interpreter_result {
    // The interpreter run *also* produces the GPU result (the boilerplate
    // dispatches a compute shader as part of the same evaluation), so we
    // can't run GPU independently — they share the same evaluation.
    BackendResult::Both { gpu, .. } => Ok(*gpu),
    BackendResult::Failed { .. } => {
      Err("(skipped — interpreter evaluation failed)".to_string())
    }
  };
  let interpreter_result = match interpreter_result {
    BackendResult::Both { cpu, .. } => Ok(cpu),
    BackendResult::Failed { reason } => Err(reason),
  };
  let c_result = run_c_backend(name, source_path, &user_source);
  let vm_result = run_vm_backend(name, source_path, &user_source);

  let labels_and_results: [(&str, &Result<f64, String>); 4] = [
    ("interpreter", &interpreter_result),
    ("WGSL (GPU)", &gpu_result),
    ("C", &c_result),
    ("VM", &vm_result),
  ];

  let format_results = || -> String {
    labels_and_results
      .iter()
      .map(|(label, r)| match r {
        Ok(v) => format!("  {label:12} = {v}"),
        Err(e) => format!("  {label:12} = ERROR: {e}"),
      })
      .collect::<Vec<_>>()
      .join("\n")
  };

  // Any backend that errored out is a hard failure.
  let any_errored = labels_and_results.iter().any(|(_, r)| r.is_err());
  // Otherwise, check that all four agree within tolerance.
  let all_agree = !any_errored && {
    let values: Vec<f64> = labels_and_results
      .iter()
      .map(|(_, r)| *r.as_ref().unwrap())
      .collect();
    let lo = values.iter().cloned().fold(f64::INFINITY, f64::min);
    let hi = values.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    if tolerance == 0.0 {
      lo == hi
    } else {
      (hi - lo).abs() <= tolerance
    }
  };

  if !all_agree {
    panic!(
      "{name}: backends disagree (tolerance = {tolerance}):\n{}",
      format_results()
    );
  }
}

/// Result of running the interpreter+WGSL stage. Both run together —
/// the boilerplate evaluates `f` on the CPU and dispatches it as a
/// compute shader in the same program.
enum BackendResult {
  Both { cpu: f64, gpu: f64 },
  Failed { reason: String },
}

fn parse_f64(label: &str, s: &str) -> Result<f64, String> {
  s.parse()
    .map_err(|_| format!("{label} output {s:?} is not a valid float"))
}

fn run_interpreter(
  name: &str,
  source_path: &Path,
  combined_source: &str,
) -> BackendResult {
  let owned_source = combined_source.to_string();
  match load_easl_program_from_file_with_lookup_function(source_path, |_| {
    Ok(owned_source.clone())
  }) {
    Ok(Ok((_, Ok(mut program)))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      if !errors.is_empty() {
        return BackendResult::Failed {
          reason: format!("compile errors: {errors:#?}"),
        };
      }
      let prints = match run_program_with_capture(program) {
        Ok(p) => p,
        Err(e) => {
          return BackendResult::Failed {
            reason: format!("evaluation error: {e:#?}"),
          };
        }
      };
      if prints.len() != 2 {
        return BackendResult::Failed {
          reason: format!("expected exactly 2 print lines, got: {prints:?}"),
        };
      }
      let cpu = match parse_f64("CPU", &prints[0]) {
        Ok(v) => v,
        Err(e) => return BackendResult::Failed { reason: e },
      };
      let gpu = match parse_f64("GPU", &prints[1]) {
        Ok(v) => v,
        Err(e) => return BackendResult::Failed { reason: e },
      };
      BackendResult::Both { cpu, gpu }
    }
    Ok(Ok((document, Err(errors)))) => {
      let description = errors.describe(&document);
      fs::write(
        format!("./out/conformance/{name}.wgsl"),
        description.clone(),
      )
      .ok();
      BackendResult::Failed {
        reason: description,
      }
    }
    Ok(Err(mut failed_documents)) => {
      let mut errs = vec![];
      std::mem::swap(
        &mut errs,
        &mut failed_documents
          .sources
          .last_mut()
          .unwrap()
          .0
          .parsing_failures,
      );
      let description = errs
        .into_iter()
        .map(|err| failed_documents.describe_parse_error(err))
        .collect::<Vec<String>>()
        .join("\n\n");
      fs::write(format!("./out/conformance/{name}.wgsl"), &description).ok();
      BackendResult::Failed {
        reason: format!("parse error: {description}"),
      }
    }
    Err(e) => BackendResult::Failed {
      reason: format!("IO error: {e:?}"),
    },
  }
}

fn run_c_backend(
  name: &str,
  source_path: &Path,
  user_source: &str,
) -> Result<f64, String> {
  let owned_source = user_source.to_string();
  let c_code =
    match load_easl_program_from_file_with_lookup_function(source_path, |_| {
      Ok(owned_source.clone())
    }) {
      Ok(Ok((_, Ok(mut program)))) => {
        let errors = program.validate_raw_program(CompilerTarget::C);
        if !errors.is_empty() {
          return Err(format!("C backend compile errors: {errors:#?}"));
        }
        program
          .compile_to_target(CompilerTarget::C)
          .map_err(|e| format!("C codegen error: {e:#?}"))?
      }
      Ok(Ok((doc, Err(errors)))) => {
        return Err(format!(
          "C backend compile errors:\n{}",
          errors.describe(&doc)
        ));
      }
      Ok(Err(mut failed_documents)) => {
        let mut errs = vec![];
        std::mem::swap(
          &mut errs,
          &mut failed_documents
            .sources
            .last_mut()
            .unwrap()
            .0
            .parsing_failures,
        );
        let description = errs
          .into_iter()
          .map(|err| failed_documents.describe_parse_error(err))
          .collect::<Vec<String>>()
          .join("\n\n");
        return Err(format!("parse error: {description}"));
      }
      Err(e) => return Err(format!("IO error: {e:?}")),
    };

  let c_code_with_main = format!(
    "{c_code}\nint main() {{\n    printf(\"%.9g\\n\", f());\n    return 0;\n}}\n"
  );

  fs::create_dir_all("./out/conformance/c/")
    .map_err(|e| format!("create out dir: {e}"))?;
  let c_path = format!("./out/conformance/c/{name}.c");
  let exe_path = format!("./out/conformance/c/{name}");
  fs::write(&c_path, &c_code_with_main)
    .map_err(|e| format!("write C file: {e}"))?;

  let compile_output = Command::new("clang")
    .args(["-std=c11", "-lm", "-o", &exe_path, &c_path])
    .output()
    .map_err(|e| {
      format!(
        "Failed to run clang (is it installed?): {e}\n\
         Install with: xcode-select --install (macOS) or apt install clang"
      )
    })?;
  if !compile_output.status.success() {
    return Err(format!(
      "clang compilation failed:\n{}\nSee {c_path}",
      String::from_utf8_lossy(&compile_output.stderr)
    ));
  }
  let run_output = Command::new(&exe_path)
    .output()
    .map_err(|e| format!("run: {e}"))?;
  if !run_output.status.success() {
    return Err("C executable returned non-zero exit code".to_string());
  }
  let raw = String::from_utf8(run_output.stdout)
    .map_err(|e| format!("stdout not utf-8: {e}"))?
    .trim()
    .to_string();
  parse_f64("C", &raw)
}

fn run_vm_backend(
  _name: &str,
  source_path: &Path,
  user_source: &str,
) -> Result<f64, String> {
  let owned_source = user_source.to_string();
  match load_easl_program_from_file_with_lookup_function(source_path, |_| {
    Ok(owned_source.clone())
  }) {
    Ok(Ok((_, Ok(mut program)))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      if !errors.is_empty() {
        return Err(format!("VM compile errors: {errors:#?}"));
      }
      let (mut bytecode_program, function_names) =
        program.compile_to_bytecode_program();
      let function_index = function_names
        .iter()
        .position(|n| &**n == "f")
        .ok_or_else(|| {
          "no function named `f` in bytecode program".to_string()
        })?;
      bytecode_program.prepare_to_run_function(function_index);
      bytecode_program.execute();
      let return_position =
        bytecode_program.get_function_return_position(function_index);
      let result =
        f32::from_bits(bytecode_program.stack[return_position as usize]);
      Ok(result as f64)
    }
    Ok(Ok((doc, Err(errors)))) => {
      Err(format!("VM compile errors:\n{}", errors.describe(&doc)))
    }
    Ok(Err(mut failed_documents)) => {
      let mut errs = vec![];
      std::mem::swap(
        &mut errs,
        &mut failed_documents
          .sources
          .last_mut()
          .unwrap()
          .0
          .parsing_failures,
      );
      let description = errs
        .into_iter()
        .map(|err| failed_documents.describe_parse_error(err))
        .collect::<Vec<String>>()
        .join("\n\n");
      Err(format!("parse error: {description}"))
    }
    Err(e) => Err(format!("IO error: {e:?}")),
  }
}

macro_rules! conformance_test {
  ($name:ident) => {
    conformance_test!($name, 0.0);
  };
  ($name:ident, $tolerance:expr) => {
    #[test]
    fn $name() {
      run_conformance_test(stringify!($name), $tolerance);
    }
  };
}

// defs and vars
conformance_test!(read_def, 0.001);
conformance_test!(read_var, 0.001);

// arithmetic
conformance_test!(addition);
conformance_test!(subtraction);
conformance_test!(multiplication);
conformance_test!(division);

// rounding / step
conformance_test!(floor);
conformance_test!(ceil);
conformance_test!(round);
conformance_test!(trunc);
conformance_test!(fract);

// sign / abs / clamp
conformance_test!(abs_positive);
conformance_test!(abs_negative);
conformance_test!(sign_positive);
conformance_test!(sign_negative);
conformance_test!(sign_zero);
conformance_test!(saturate_above);
conformance_test!(saturate_below);
conformance_test!(clamp_high);
conformance_test!(clamp_low);
conformance_test!(clamp_mid);

// sqrt
conformance_test!(sqrt_exact);
conformance_test!(sqrt_two, 0.001);

// exponentials / logarithms
conformance_test!(exp_zero);
conformance_test!(exp_one, 0.001);
conformance_test!(exp2_three);
conformance_test!(log_one);
conformance_test!(log2_eight, 0.001);
conformance_test!(pow_exact);

// trigonometry — exact-zero inputs
conformance_test!(sin_zero, 0.001);
conformance_test!(cos_zero);
conformance_test!(tan_zero, 0.001);
conformance_test!(asin_zero, 0.001);
conformance_test!(acos_one, 0.001);
conformance_test!(atan_zero, 0.001);
conformance_test!(atan2_zero, 0.001);

// trigonometry — irrational results
conformance_test!(sin_half_pi, 0.001);
conformance_test!(cos_pi, 0.001);
conformance_test!(tan_pi_over_4, 0.001);
conformance_test!(asin_one, 0.001);
conformance_test!(acos_zero, 0.001);
conformance_test!(atan_one, 0.001);
conformance_test!(sin_vec4f, 0.001);

// min / max / mix
conformance_test!(min_f32);
conformance_test!(max_f32);
conformance_test!(mix_half);
conformance_test!(mix_half_vec4f);

// interpolation
conformance_test!(smoothstep_zero);
conformance_test!(smoothstep_one);
conformance_test!(smoothstep_half, 0.001);

// fused multiply-add
conformance_test!(fma_basic);

// more trigonometry
conformance_test!(atan2_one_zero, 0.001);
conformance_test!(atan2_one_one, 0.001);
conformance_test!(sin_pi, 0.001);
conformance_test!(cos_half_pi, 0.001);

// hyperbolic trig
conformance_test!(sinh_zero, 0.001);
conformance_test!(cosh_zero);
conformance_test!(tanh_zero, 0.001);
conformance_test!(sinh_one, 0.001);
conformance_test!(cosh_one, 0.001);
conformance_test!(tanh_one, 0.001);
conformance_test!(asinh_zero, 0.001);
conformance_test!(acosh_one, 0.001);
conformance_test!(atanh_zero, 0.001);

// more rounding (negative inputs)
conformance_test!(floor_negative);
conformance_test!(ceil_negative);
conformance_test!(trunc_negative);
conformance_test!(fract_negative);

// more sqrt / pow / log
conformance_test!(sqrt_zero);
conformance_test!(sqrt_large);
conformance_test!(pow_zero_exp);
conformance_test!(pow_one_exp);
conformance_test!(pow_half_exp);
conformance_test!(log2_one);
conformance_test!(log_e, 0.001);

// ldexp
conformance_test!(ldexp_basic);
conformance_test!(ldexp_negative_exp);

// more mix / interpolation
conformance_test!(mix_zero);
conformance_test!(mix_one);

// more clamp / min / max edge cases
conformance_test!(min_equal);
conformance_test!(max_equal);
conformance_test!(clamp_in_range);

// abs / sign edge cases
conformance_test!(abs_zero);
conformance_test!(sign_positive_f32);
conformance_test!(sign_negative_f32);

// vector operations
conformance_test!(dot_vec2f);
conformance_test!(dot_vec3f);
conformance_test!(length_vec2f);
conformance_test!(length_vec3f);
conformance_test!(normalize_length, 0.001);
conformance_test!(distance_vec2f);
conformance_test!(cross_z);
conformance_test!(cross_x);

// integer (i32 / u32) operations
conformance_test!(i32_abs);
conformance_test!(i32_min);
conformance_test!(i32_max);
conformance_test!(i32_clamp);
conformance_test!(u32_min);
conformance_test!(vec4u_min);
conformance_test!(u32_max);
conformance_test!(u32_clamp);

// type conversions
conformance_test!(i32_to_f32);
conformance_test!(u32_to_f32);
conformance_test!(f32_truncate);

// bitcast
conformance_test!(bitcast_roundtrip);
conformance_test!(vec_bitcast_roundtrip);

// bit manipulation
conformance_test!(count_one_bits);
conformance_test!(count_leading_zeros);
conformance_test!(count_trailing_zeros);
conformance_test!(reverse_bits);
conformance_test!(first_leading_bit);
conformance_test!(first_trailing_bit);
conformance_test!(extract_bits);

// degrees / radians
conformance_test!(degrees_zero);
conformance_test!(degrees_90, 0.01);
conformance_test!(radians_zero);
conformance_test!(radians_180, 0.001);

// inverse sqrt
conformance_test!(inverse_sqrt_four, 0.001);
conformance_test!(inverse_sqrt_four_vec4f, 0.001);
conformance_test!(inverse_sqrt_one);

// step
conformance_test!(step_above);
conformance_test!(step_below);
conformance_test!(step_equal);

// face-forward
conformance_test!(face_forward_toward);
conformance_test!(face_forward_away);

// reflect / refract
conformance_test!(reflect_basic);
conformance_test!(refract_basic, 0.001);

// matrix functions
conformance_test!(determinant_2x2);
conformance_test!(determinant_3x3);
conformance_test!(determinant_4x4);
conformance_test!(transpose_2x2);
conformance_test!(vec2f_arithmetic);

// matrix arithmetic
conformance_test!(mat_mat_add);
conformance_test!(mat_mat_sub);
conformance_test!(mat_scalar_mul);
conformance_test!(scalar_mat_mul);
conformance_test!(vec_mat_mul);
conformance_test!(mat3x2_vec_mul);
conformance_test!(mat_mat_mul_nonsquare);

// conditionals and pattern matching
conformance_test!(if_true_branch, 0.001);
conformance_test!(if_false_branch, 0.001);
conformance_test!(if_nested);
conformance_test!(if_statement);
conformance_test!(match_int);
conformance_test!(match_int_default);
conformance_test!(match_bool);
conformance_test!(match_enum_unit);
conformance_test!(match_enum_data);
conformance_test!(match_in_expression);
conformance_test!(match_statement);

// mutable references
conformance_test!(mut_ref_scalar);
conformance_test!(mut_ref_scalar_multiple_calls);
conformance_test!(mut_ref_indirect);
conformance_test!(mut_ref_vec);
conformance_test!(mut_ref_struct);
conformance_test!(mut_ref_struct_field);
conformance_test!(mut_ref_indirect_struct_field);
conformance_test!(mut_ref_swizzle);
conformance_test!(mut_ref_struct_multiple_calls);

// vec / mat indexing via call-style syntax
conformance_test!(vec_index);
conformance_test!(vec_index_u32);
conformance_test!(mat_column_index);
conformance_test!(mat_nested_index);
conformance_test!(mat_inline_index);
conformance_test!(mat_double_index);
conformance_test!(mat_index_var);
conformance_test!(mat_diagonal_sum);
conformance_test!(struct_field_assignment);
conformance_test!(array_index_assignment);
conformance_test!(array_argument);
conformance_test!(for_loop);
conformance_test!(for_loop_init);
