use easl::compiler::core::load_easl_program_from_file_with_lookup_function;
use easl::compiler::program::CompilerTarget;
use easl::interpreter::run_program_with_capture;
use std::fs;
use std::path::Path;

const BOILERPLATE: &str = "
@[storage-write 0 0]
(var x: f32)

@cpu
(defn main []
  (print (f))
  (dispatch-compute-shader (fn [] (= x (f))) (vec3u 1u))
  (print x))
";

fn run_conformance_test(name: &str, tolerance: f64) {
  let source_path_str = format!("./data/conformance/{name}.easl");
  let source_path = Path::new(&source_path_str);

  let user_source = fs::read_to_string(source_path)
    .unwrap_or_else(|e| panic!("IO error, couldn't read file {name}: {e:?}"));
  let combined_source = user_source + BOILERPLATE;

  fs::create_dir_all("./out/conformance/")
    .expect("Unable to create out directory");
  match load_easl_program_from_file_with_lookup_function(source_path, |_| {
    Ok(combined_source.clone())
  }) {
    Ok(Ok((_, Ok(mut program)))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

      let prints = run_program_with_capture(program).unwrap_or_else(|e| {
        panic!("{name}: evaluation error: {e:#?}");
      });
      assert_eq!(
        prints.len(),
        2,
        "{name}: expected exactly 2 print lines, got: {prints:?}"
      );
      if tolerance == 0.0 {
        assert_eq!(
          prints[0], prints[1],
          "{name}: CPU result {:?} != GPU result {:?}",
          prints[0], prints[1]
        );
      } else {
        let cpu: f64 = prints[0].parse().unwrap_or_else(|_| {
          panic!("{name}: CPU output {:?} is not a valid float", prints[0])
        });
        let gpu: f64 = prints[1].parse().unwrap_or_else(|_| {
          panic!("{name}: GPU output {:?} is not a valid float", prints[1])
        });
        assert!(
          (cpu - gpu).abs() <= tolerance,
          "{name}: CPU result {cpu} and GPU result {gpu} differ by more than tolerance {tolerance}"
        );
      }
    }
    Ok(Ok((document, Err(errors)))) => {
      let description = errors.describe(&document);
      fs::write(
        format!("./out/conformance/{name}.wgsl"),
        description.clone(),
      )
      .expect("Unable to write output file");
      panic!("{description}");
    }
    Ok(Err(mut failed_documents)) => {
      let mut errors = vec![];
      std::mem::swap(
        &mut errors,
        &mut failed_documents
          .sources
          .last_mut()
          .unwrap()
          .0
          .parsing_failures,
      );
      let description = errors
        .into_iter()
        .map(|err| failed_documents.describe_parse_error(err))
        .collect::<Vec<String>>()
        .join("\n\n");
      fs::write(format!("./out/conformance/{name}.wgsl"), &description)
        .expect("Unable to write output file");
      panic!("Unexpected parse error in {name}:\n{description}");
    }
    Err(e) => panic!("IO error, couldn't load file {name}: \n{e:?}"),
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

// min / max / mix
conformance_test!(min_f32);
conformance_test!(max_f32);
conformance_test!(mix_half);

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
conformance_test!(u32_max);
conformance_test!(u32_clamp);

// type conversions
conformance_test!(i32_to_f32);
conformance_test!(u32_to_f32);
conformance_test!(f32_truncate);

// bitcast
conformance_test!(bitcast_roundtrip);

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
conformance_test!(transpose_2x2);
