use easl::compiler::core::load_easl_program_from_file;
use easl::compiler::program::CompilerTarget;
use easl::interpreter::{
  CpuRuntime, run_program_capturing_output_with_runtime,
};
use std::fs;
use std::path::Path;

fn run_cpu_test(name: &str) {
  let expected = fs::read_to_string(format!("./data/cpu/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/cpu/{name}.txt"));
  let x =
    load_easl_program_from_file(Path::new(&format!("./data/cpu/{name}.easl")));
  match x {
    Ok(Ok((_, Ok(mut program)))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

      // Every test runs on both CPU runtimes and must produce identical
      // output on each.
      let output = run_program_capturing_output_with_runtime(
        program.clone(),
        CpuRuntime::TreeWalking,
      )
      .unwrap_or_else(|e| {
        panic!("{name}: evaluation error (tree-walking): {e:#?}");
      });
      assert_eq!(output, expected, "{name}: output mismatch (tree-walking)");

      let vm_output = run_program_capturing_output_with_runtime(
        program,
        CpuRuntime::BytecodeVm,
      )
      .unwrap_or_else(|e| {
        panic!("{name}: evaluation error (bytecode VM): {e:#?}");
      });
      assert_eq!(vm_output, expected, "{name}: output mismatch (bytecode VM)");
    }
    Ok(Ok((document, Err(errors)))) => {
      let description = errors.describe(&document);
      fs::write(format!("./out/cpu/{name}.wgsl"), description.clone())
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
      fs::write(format!("./out/cpu/{name}.wgsl"), &description)
        .expect("Unable to write output file");
      panic!("Unexpected parse error in {name}:\n{description}");
    }
    Err(e) => panic!("IO error, couldn't load file {name}: \n{e:?}"),
  }
}

macro_rules! cpu_test {
  ($name:ident) => {
    #[test]
    fn $name() {
      run_cpu_test(stringify!($name));
    }
  };
}

cpu_test!(print);
cpu_test!(def);
cpu_test!(assignment);
cpu_test!(field_assignment);
cpu_test!(for_loop);
cpu_test!(while_loop);
cpu_test!(defn);
cpu_test!(struct_type);
cpu_test!(enum_type);
cpu_test!(print_vec);
cpu_test!(break_for);
cpu_test!(break_while);
cpu_test!(continue_for);
cpu_test!(continue_while);
cpu_test!(nested_break);
cpu_test!(nested_continue);
cpu_test!(nested_break_while);
cpu_test!(early_return);
cpu_test!(return_from_loop);
cpu_test!(bitwise_ops);
cpu_test!(reflect);
cpu_test!(refract);
cpu_test!(bitcast);
cpu_test!(array_length);
cpu_test!(mat_construct);
cpu_test!(mat_add_sub);
cpu_test!(mat_scalar_mul);
cpu_test!(mat_vec_mul);
cpu_test!(mat_mat_mul);
cpu_test!(vec_index_cpu);
cpu_test!(mat_index_cpu);
cpu_test!(mat_index_assign);
cpu_test!(vec_index_assign);
cpu_test!(bit_manip);
cpu_test!(data_packing);
cpu_test!(array_assignment);
cpu_test!(array_element_compound_assignment);
cpu_test!(array_element_dynamic_index_assignment);
cpu_test!(array_element_compound_dynamic_index_assignment);
cpu_test!(dynamic_array_compound_assignment);
cpu_test!(dynamic_array_dynamic_index_assignment);
cpu_test!(dynamic_array_assignment);
cpu_test!(dynamic_array_from_function);
cpu_test!(dynamic_array_local_scratch);
cpu_test!(dynamic_array_copy_semantics);
cpu_test!(dynamic_array_two_results);
cpu_test!(dynamic_array_hof_no_clobber);
cpu_test!(dynamic_array_closure_capture);
cpu_test!(dynamic_array_struct_field);
cpu_test!(dynamic_array_enum_payload);
cpu_test!(nested_dynamic_array);
cpu_test!(nested_dynamic_array_element_store);
cpu_test!(nested_dynamic_array_copy_on_write);
cpu_test!(nested_dynamic_array_global);
cpu_test!(string_array_element_store);
cpu_test!(dynamic_array_generic_return);
cpu_test!(print_dynamic_array_local);
cpu_test!(print_dynamic_array_struct_field);
cpu_test!(print_dynamic_array_enum_payload);
cpu_test!(print_nested_dynamic_array);
cpu_test!(string_conversion);
cpu_test!(string_concat);
cpu_test!(string_length);
cpu_test!(string_substr);
cpu_test!(string_equality);
cpu_test!(string_user_fn);
cpu_test!(string_assignment);
cpu_test!(let_binding_copy_semantics);
cpu_test!(into_operator);
cpu_test!(into_builtin_conversions);
cpu_test!(into_inference_contexts);
cpu_test!(dynamic_zeroed_array);
cpu_test!(static_zeroed_array);
cpu_test!(hof_avoids_skipping_calls);
cpu_test!(hof_calls_not_skipped);
cpu_test!(nested_associatives);
cpu_test!(any_all);
cpu_test!(early_return_unit);
cpu_test!(disambiguated_overload);
cpu_test!(disambiguated_into_overload);
cpu_test!(audio_closure_entry);
cpu_test!(audio_closure_entry_hofs);
cpu_test!(cpu_only_bool_var);
// The embedded-heap-id promotion pins: aggregates (structs, closure
// scopes) carrying runtime-sized fields across constructions and call
// boundaries, where the ids must be owned shares rather than borrows of
// the allocation site (see `HeapCopyPlan`).
cpu_test!(dyn_field_struct_across_calls);
cpu_test!(closure_dyn_capture_across_calls);
// Whole-enum copies of heap payloads: payload offsets depend on the
// runtime discriminant, so the release/promote fixups are emitted as a
// per-variant compare-and-skip dispatch (release side keyed on the
// destination's old discriminant, promote side on the copied value's —
// `emit_heap_fixups` in vm/compile.rs). Each pin below covers one face
// of that machinery — see the .easl headers.
cpu_test!(enum_dyn_payload_across_calls);
cpu_test!(enum_dyn_payload_transitions);
cpu_test!(enum_multi_dyn_variants_across_calls);
cpu_test!(dyn_enum_in_struct_across_calls);
cpu_test!(dyn_struct_in_enum_variant_across_calls);
cpu_test!(dyn_enum_in_enum_across_calls);
cpu_test!(dyn_enum_ref_fn_arg);
cpu_test!(dyn_enum_payload_extraction_outlives);
cpu_test!(closure_seeded_capture_read);
cpu_test!(ref_dyn_array_arg);
cpu_test!(dyn_array_arg_scalar_return);
cpu_test!(hof_shared_specialization);
cpu_test!(sibling_same_name_locals);
cpu_test!(const_generic_chain);
cpu_test!(const_generic_chain_three);
cpu_test!(audio_time_through_hof_chain);
cpu_test!(load_wav_local_binding);
cpu_test!(assign_field_in_dyn_array_element);
cpu_test!(const_generic_zeroed_array);
cpu_test!(const_generic_zeroed_array_map);
// KNOWN-FAILING: containers whose ELEMENT type involves heap values —
// two distinct holes, one per container kind. Runtime-sized containers
// with elements that *embed* heap ids without being one
// (`[(Option [f32])]`, `[Packet]`-with-dyn-field) store flat words
// (`DynMemory::Words`), so construction/store/COW-clone traffic copies
// embedded ids as silent borrows. FIXED-size arrays with heap-value
// elements (`[2: [f32]]`, `[2: (Option [f32])]`, `[2: Packet]`)
// currently panic at VM compile — `vm_type_size`'s sized-array case
// delegates to GPU sizing, which can't size heap elements — and once
// sized, `ArrayLookup`/`ArrayStore` raw-copy at `base + i*stride` with
// no release/promote (whole-array copies should be sound via
// `collect_heap_fixups`' element recursion — the `_across_calls` guard
// pins that). See each .easl header for the exact surface, and the
// container-support design notes for the agreed fix (sharedness-branch
// + compile-time-emitted per-element fixup loops).
cpu_test!(dyn_container_enum_elements);
cpu_test!(dyn_container_struct_elements);
cpu_test!(dyn_container_element_store);
cpu_test!(dyn_container_copy_semantics);
cpu_test!(dyn_container_string_elements);
cpu_test!(dyn_container_global);
cpu_test!(fixed_array_dyn_elements_across_calls);
cpu_test!(fixed_array_dyn_element_store);
cpu_test!(fixed_array_enum_element_store);
cpu_test!(fixed_array_struct_element_store);
