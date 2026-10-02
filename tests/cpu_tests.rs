use easl::compiler::core::load_easl_program_from_file;
use easl::compiler::program::CompilerTarget;
use easl::interpreter::{
  CpuRuntime, IOEvent, MidiNoteState, MidiState, StringIO,
  run_program_capturing_output_with_runtime, run_program_with_runtime,
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
cpu_test!(nested_dynamic_array_deep_element_store);
cpu_test!(nested_dynamic_array_deep_compound_assignment);
cpu_test!(immutable_ref_element_args);
cpu_test!(user_ref_fn_deep_element_args);
cpu_test!(storage_ref_cpu);
cpu_test!(storage_ref_two_globals);
cpu_test!(storage_ref_mixed_binding);
cpu_test!(storage_ref_local_and_global_sites);
cpu_test!(storage_ref_forwarding);
cpu_test!(storage_ref_whole_assign);
cpu_test!(storage_ref_struct_field);
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
cpu_test!(length_aliases);
cpu_test!(into_dynamic_array_alias);
cpu_test!(empty_dynamic_array_conversion);
cpu_test!(into_inference_contexts);
cpu_test!(dynamic_zeroed_array);
cpu_test!(static_zeroed_array);
cpu_test!(hof_avoids_skipping_calls);
cpu_test!(hof_calls_not_skipped);
cpu_test!(nested_associatives);
cpu_test!(any_all);
cpu_test!(early_return_unit);
cpu_test!(disambiguated_overload);
// Overloads differing only in a function-typed param's return type —
// resolvable because `FunctionSignature::compatible` compares return
// types (see the .easl header).
cpu_test!(overload_fn_return_types);
cpu_test!(disambiguated_into_overload);
cpu_test!(audio_closure_entry);
cpu_test!(audio_closure_entry_hofs);
cpu_test!(cpu_only_bool_var);
// The embedded-heap-id promotion pins: aggregates (structs, closure
// scopes) carrying runtime-sized fields across constructions and call
// boundaries, where the ids must be owned shares rather than borrows of
// the allocation site (see `HeapCopyPlan`).
cpu_test!(dyn_field_struct_across_calls);
cpu_test!(closure_capture_hof);
cpu_test!(dyn_array_push_insert_remove);
cpu_test!(dyn_array_push_embedded_heap);
cpu_test!(dyn_array_concat);
cpu_test!(dyn_array_concat_embedded_heap);
cpu_test!(dyn_array_reverse);
cpu_test!(generics_used_indirectly);
cpu_test!(nested_closure_outer_capture);
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
cpu_test!(generic_hof_closure_overload);
cpu_test!(generic_hof_struct_closure_overload);
cpu_test!(sibling_same_name_locals);
cpu_test!(const_generic_chain);
cpu_test!(const_generic_chain_three);
cpu_test!(audio_time_through_hof_chain);
cpu_test!(load_wav_local_binding);
cpu_test!(load_wav_raw);
cpu_test!(wav_sample_rate);
cpu_test!(save_wav_roundtrip);
cpu_test!(listenable_sources);
cpu_test!(midi_silent_defaults);
cpu_test!(for_loop_zero_iterations_lifted_condition);
cpu_test!(assign_field_in_dyn_array_element);
cpu_test!(const_generic_zeroed_array);
cpu_test!(const_generic_zeroed_array_map);
cpu_test!(const_generic_array_from_hof);
cpu_test!(overload_chain_returning_hof);
// The embedding-element container pins: RUNTIME-SIZED containers whose
// element type *embeds* heap ids without being one (`[(Option [f32])]`,
// `[Packet]`-with-dyn-field) store flat words (`DynMemory::Words`), and
// every path cloning words into or out of one re-owns / releases the
// embedded ids through compile-time-emitted per-element fixup sequences
// (`emit_embedding_element_store` / `emit_reown_container_elements` /
// `emit_ensure_unique_cell` in vm/compile.rs — sharedness reflected as
// an emitted `HeapUnique` branch, no runtime layout metadata; each
// .easl header covers one surface). The fixed_array_* tests guard the
// slot-resident half: whole-array copies and indexed element stores
// compose with the same fixup machinery via `vm_stack_size`'s
// per-element recursion.
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
cpu_test!(fixed_array_nested_element_store);
cpu_test!(print_fixed_array_dyn_elements);
cpu_test!(closure_fixed_array_capture);
// Aliased mutable-reference args are rejected at compile time
// (`validate_ref_arg_aliasing`; the aliased_ref_args_*_failure shader
// error tests pin the rejections) — this pins the *allowed* disjoint
// shapes' runtime behavior.
cpu_test!(disjoint_ref_args_swap);
cpu_test!(ref_element_snapshot_intervening_mutation);
cpu_test!(fixed_array_ref_element_intervening_mutation);
cpu_test!(fixed_array_ref_element_aliased_owned);
cpu_test!(fixed_array_literal_in_loop);
cpu_test!(ref_element_aliased_owned_snapshot);
cpu_test!(unit_if_compound_assignment_arms);
cpu_test!(unit_match_compound_assignment_arms);
cpu_test!(local_ref_capture_helper);
cpu_test!(ref_capture_hof_arg);

// First-class function values: functions and closures stored in arrays,
// struct fields, enum payloads, and globals, or chosen by `if`/`match` (see
// "First-class function values" in CLAUDE.md).
cpu_test!(fn_value_array_basic);
cpu_test!(fn_value_runtime_index);
cpu_test!(fn_value_closure_array);
cpu_test!(fn_value_if_merge);
cpu_test!(fn_value_struct_field);
cpu_test!(fn_value_struct_field_union);
cpu_test!(fn_value_enum_payload);
cpu_test!(fn_value_multi_layer);
cpu_test!(fn_value_closure_captures_array);
cpu_test!(fn_value_generic);
cpu_test!(fn_value_unsized_array);
cpu_test!(fn_value_global);
cpu_test!(fn_value_global_state);

// Function-value shapes that were a wrong result, a runtime crash, or a panic
// on at least one runtime in an earlier implementation.
cpu_test!(fn_value_nested_merge);
cpu_test!(fn_value_return_let_merge);
cpu_test!(fn_value_return_indexed);
cpu_test!(fn_value_array_containing_merge);
cpu_test!(fn_value_let_merge_into_array);
cpu_test!(fn_value_array_element_assign);
cpu_test!(fn_value_struct_array);
cpu_test!(fn_value_struct_field_from_union_elem);
cpu_test!(fn_value_enum_payload_from_union_elem);
cpu_test!(fn_value_overloaded_receiver);
cpu_test!(fn_value_named_and_lambda_receiver);
cpu_test!(fn_value_generic_struct_field);
cpu_test!(fn_value_struct_field_lambda);
cpu_test!(fn_value_struct_field_closure_capture);
// Stateful closures stored in arrays, struct fields, and merges must keep
// their captured state exactly as a directly-called closure does. This is a
// language requirement, not an optional improvement: never weaken, ignore, or
// delete these to get a green suite.
cpu_test!(fn_value_closure_array_state);
cpu_test!(fn_value_closure_bank_state);
cpu_test!(fn_value_closure_loop_state);
cpu_test!(fn_value_struct_field_closure_state);
cpu_test!(fn_value_struct_field_lambda_state);
cpu_test!(fn_value_closure_merge_state);
cpu_test!(fn_value_stored_closure_hof_arg);
cpu_test!(fn_value_closure_array_param);
cpu_test!(fn_value_struct_field_closure_copy);
cpu_test!(fn_value_struct_layer_composition);
cpu_test!(fn_value_ref_capture_local);
cpu_test!(closure_pure_alias);
cpu_test!(fn_value_dynamic_array_push);
cpu_test!(fn_value_merge_hof_params);
cpu_test!(fn_value_enum_payload_alias);
cpu_test!(fn_value_match_payload_alias);
cpu_test!(fn_value_match_payload_alias_let);
cpu_test!(fn_value_match_payload_reassigned);
cpu_test!(fn_value_match_payload_early_exit);
cpu_test!(fn_value_match_payload_places);
cpu_test!(fn_value_match_payload_temporary);
cpu_test!(fn_value_match_payload_pure_ref);
cpu_test!(fn_value_match_payload_struct);
cpu_test!(fn_value_match_payload_hof_and_loop);
cpu_test!(fn_value_match_payload_heap_return);
cpu_test!(fn_value_generic_producer);
cpu_test!(fn_value_frame_closure_bank);
// Functions that take or return functions, stored as values.
cpu_test!(fn_value_hof_members);
cpu_test!(fn_value_hof_lambda_members);
cpu_test!(fn_value_hof_stateful_args);
cpu_test!(fn_value_factories);
cpu_test!(fn_value_stateful_factories);
cpu_test!(fn_value_hof_param_through_stored_hof);
cpu_test!(fn_value_generic_hof_member);
cpu_test!(fn_value_struct_field_hof);
cpu_test!(fn_value_fn_taking_factory);
cpu_test!(fn_value_factory_bank);
cpu_test!(fn_value_hof_loop_state);
cpu_test!(fn_value_hof_global);
cpu_test!(fn_value_if_selected_hof);
cpu_test!(fn_value_unit_scope_union);
cpu_test!(fn_value_lent_closure_pure_twice);
cpu_test!(fn_value_lent_closure_union_copy_back);
cpu_test!(fn_value_local_hof_stored_args);
cpu_test!(fn_value_local_hof_pure_union_arg);
cpu_test!(fn_value_hof_pure_union_arg);
cpu_test!(fn_value_hof_arg_matrix);
cpu_test!(fn_value_factory_return_matrix);
cpu_test!(fn_value_generic_instantiated_at_function);
// Single-variant user enums must keep their VM layout (unrelated to function
// values, but broken by the same change).
cpu_test!(single_variant_enum_layout);
// Binding a closure to a new name copies its captured state.

cpu_test!(hof_specialization_across_passes);
cpu_test!(stored_pure_closures_captured_twice);
cpu_test!(unary_associative_in_boxed_walk);
cpu_test!(diverging_statements);
cpu_test!(captured_stateful_closure_bank);
cpu_test!(hof_returned_closure_in_stored_bank);

/// Compiling a program must not depend on hash-map iteration order. Each
/// thread seeds its maps afresh, so this compiles the fixture under many
/// orders: a higher-order specialization reached in two different inlining
/// passes once registered a duplicate under some orders, orphaning the
/// call sites that referenced the first copy.
#[test]
fn hof_specialization_across_passes_any_order() {
  for _ in 0..24 {
    std::thread::spawn(|| {
      let path = Path::new("./data/cpu/hof_specialization_across_passes.easl");
      let Ok(Ok((_, Ok(mut program)))) = load_easl_program_from_file(path)
      else {
        panic!("failed to load program");
      };
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      assert!(errors.is_empty(), "compile errors: {errors:#?}");
    })
    .join()
    .expect("compilation panicked");
  }
}

/// The full MIDI query surface against spoofed input state, on both
/// runtimes: per-note velocities, CC values, pitch bend, and the
/// held-note list (`data/cpu/midi_queries.easl`). Spoofing goes through
/// `StringIO::spoofed_midi` — the same `IOManager::midi_state` path the
/// live listener feeds in production.
#[test]
fn midi_queries_spoofed() {
  let expected = fs::read_to_string("./data/cpu/midi_queries.txt")
    .expect("Unable to read data/cpu/midi_queries.txt");
  let Ok(Ok((_, Ok(mut program)))) =
    load_easl_program_from_file(Path::new("./data/cpu/midi_queries.easl"))
  else {
    panic!("midi_queries: failed to load program");
  };
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  assert!(
    errors.is_empty(),
    "midi_queries: compile errors: {errors:#?}"
  );
  let mut midi = MidiState::default();
  midi.cc[1] = 0.25;
  midi.channel_aftertouch = 0.125;
  midi.pitch_bend = -0.5;
  midi.down_notes = vec![
    MidiNoteState {
      note: 60,
      velocity: 1.,
      aftertouch: 0.75,
    },
    MidiNoteState {
      note: 64,
      velocity: 0.5,
      aftertouch: 0.25,
    },
  ];
  midi.generation = 1;
  for (runtime, label) in [
    (CpuRuntime::TreeWalking, "tree-walking"),
    (CpuRuntime::BytecodeVm, "bytecode VM"),
  ] {
    let io = StringIO {
      spoofed_midi: Some(midi.clone()),
      ..StringIO::default()
    };
    let (io, _) =
      run_program_with_runtime(program.clone(), None, io, None, runtime)
        .unwrap_or_else(|e| {
          panic!("midi_queries: evaluation error ({label}): {e:#?}")
        });
    let mut output = String::new();
    for event in &io.events {
      if let IOEvent::Print(s) = event {
        output.push_str(s);
        output.push('\n');
      }
    }
    assert_eq!(output, expected, "midi_queries: output mismatch ({label})");
  }
}

/// `get-midi-note` against spoofed input on both runtimes: a held note
/// resolves to `Some` with its velocity/aftertouch, an unheld one to
/// `None`. Uses the builtin `Option`, without the program defining it.
#[test]
fn get_midi_note_spoofed() {
  let Ok(Ok((_, Ok(mut program)))) =
    load_easl_program_from_file(Path::new("./data/cpu/get_midi_note.easl"))
  else {
    panic!("get_midi_note: failed to load program");
  };
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  assert!(
    errors.is_empty(),
    "get_midi_note: compile errors: {errors:#?}"
  );
  let mut midi = MidiState::default();
  midi.down_notes = vec![
    MidiNoteState {
      note: 60,
      velocity: 1.,
      aftertouch: 0.75,
    },
    MidiNoteState {
      note: 64,
      velocity: 0.5,
      aftertouch: 0.25,
    },
  ];
  midi.generation = 1;
  let expected = "1.75\n-1.\n64u\n";
  for (runtime, label) in [
    (CpuRuntime::TreeWalking, "tree-walking"),
    (CpuRuntime::BytecodeVm, "bytecode VM"),
  ] {
    let io = StringIO {
      spoofed_midi: Some(midi.clone()),
      ..StringIO::default()
    };
    let (io, _) =
      run_program_with_runtime(program.clone(), None, io, None, runtime)
        .unwrap_or_else(|e| {
          panic!("get_midi_note: evaluation error ({label}): {e:#?}")
        });
    let mut output = String::new();
    for event in &io.events {
      if let IOEvent::Print(s) = event {
        output.push_str(s);
        output.push('\n');
      }
    }
    assert_eq!(output, expected, "get_midi_note: output mismatch ({label})");
  }
}
cpu_test!(hof_closure_factory_arg);
cpu_test!(copied_stateful_closure_allowed);
cpu_test!(integer_wraparound);
cpu_test!(zeroed_array_of_nested_closures);
cpu_test!(nested_lambda_scope_temporary);
cpu_test!(fn_value_generic_passthrough);
cpu_test!(fn_value_captured_single_member_calls);
cpu_test!(fn_value_mixed_signature_assignment);
cpu_test!(frame_closure_calls_unregistered_specialization);
cpu_test!(lambda_return);
cpu_test!(comments);
