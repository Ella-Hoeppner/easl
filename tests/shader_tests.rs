use easl::compile_easl_source_to_wgsl;
use easl::compiler::entry::InputOrOutput;
use easl::compiler::error::CompileErrorKind;
use easl::compiler::types::{
  TypeConstraintDescription, TypeDescription, TypeStateDescription,
};
use easl::compiler::vars::VariableAddressSpace;
use std::fs;

/// Compiles a .easl file from data/ and writes output (WGSL or error description) to out/.
/// Returns Ok(wgsl) on success, or Err(Vec<CompileErrorKind>) on compile error.
/// Panics on parse errors, since those indicate a broken test file rather than
/// the kind of user-facing error we'd want to test for.
fn compile_shader(name: &str) -> Result<String, Vec<CompileErrorKind>> {
  let easl_source = fs::read_to_string(format!("./data/gpu/{name}.easl"))
    .unwrap_or_else(|_| panic!("Unable to read data/{name}.easl"));
  fs::create_dir_all("./out/").expect("Unable to create out directory");

  match compile_easl_source_to_wgsl(&easl_source) {
    Ok(Ok(wgsl)) => {
      fs::write(format!("./out/{name}.wgsl"), &wgsl)
        .expect("Unable to write output file");
      Ok(wgsl)
    }
    Ok(Err((document, error_log))) => {
      fs::write(format!("./out/{name}.wgsl"), error_log.describe(&document))
        .expect("Unable to write output file");
      Err(error_log.errors.into_iter().map(|e| e.kind).collect())
    }
    Err(mut failed_document) => {
      let mut errors = vec![];
      std::mem::swap(&mut errors, &mut failed_document.parsing_failures);
      let description = errors
        .into_iter()
        .map(|err| err.describe(&failed_document))
        .collect::<Vec<String>>()
        .join("\n\n");
      fs::write(format!("./out/{name}.wgsl"), &description)
        .expect("Unable to write output file");
      panic!("Unexpected parse error in {name}:\n{description}");
    }
  }
}

/// Validate that a WGSL string is well-formed and type-correct according to naga.
fn validate_wgsl(name: &str, wgsl: &str) {
  let module = naga::front::wgsl::parse_str(wgsl).unwrap_or_else(|e| {
    panic!(
      "{name}: naga failed to parse generated WGSL:\n{e}\n\
       See out/{name}.wgsl for the generated code."
    )
  });
  let mut validator = naga::valid::Validator::new(
    naga::valid::ValidationFlags::all(),
    naga::valid::Capabilities::all(),
  );
  validator.validate(&module).unwrap_or_else(|e| {
    panic!(
      "{name}: naga validation failed on generated WGSL:\n{e}\n\
       See out/{name}.wgsl for the generated code."
    )
  });
}

/// Assert that a shader file compiles successfully to WGSL,
/// then validate the output with naga.
fn assert_compiles(name: &str) {
  let wgsl = compile_shader(name).unwrap_or_else(|errors| {
    panic!(
      "{name}.easl failed to compile: {errors:?}\n\
       See out/{name}.wgsl for details."
    )
  });
  validate_wgsl(name, &wgsl);
}

/// Assert that a shader file fails to compile with exactly the given errors.
fn assert_errors(name: &str, expected: &[CompileErrorKind]) {
  let actual = compile_shader(name).expect_err(&format!(
    "{name}.easl compiled successfully but was expected to fail.\n\
     See out/{name}.wgsl for the produced WGSL."
  ));

  for exp in expected {
    assert!(
      actual.iter().any(|a| a == exp),
      "Expected error `{exp}` not found.\n\
       Actual errors: {actual:?}\n\
       See out/{name}.wgsl for details."
    );
  }

  for act in &actual {
    assert!(
      expected.iter().any(|e| e == act),
      "Unexpected error `{act}`.\n\
       Expected errors: {expected:?}\n\
       See out/{name}.wgsl for details."
    );
  }
}

macro_rules! success_test {
  ($name:ident) => {
    #[test]
    fn $name() {
      assert_compiles(stringify!($name));
    }
  };
}

macro_rules! error_test {
  ($name:ident, $($error:expr),+ $(,)?) => {
    #[test]
    fn $name() {
      assert_errors(stringify!($name), &[$($error),+]);
    }
  };
}

// --- Success tests ---

success_test!(inversion);
success_test!(associative);
success_test!(assignment);
success_test!(simple_shader);
success_test!(variadic_vec);
success_test!(let_binding);
success_test!(block);
success_test!(accessor);
success_test!(swizzle);
success_test!(equality);
success_test!(variable);
success_test!(generic_struct);
success_test!(nested_generic_struct);
success_test!(generic_fn);
success_test!(monomorphized_fn);
success_test!(nested_monomorphized_fn);
success_test!(bool_match);
success_test!(int_match);
success_test!(generic_vec);
success_test!(heterogenous_vec_constructor);
success_test!(specialized_vec_constructor);
success_test!(for_loop);
success_test!(while_loop);
success_test!(math);
success_test!(if_macro);
success_test!(thread_macro);
success_test!(when_macro);
success_test!(array);
success_test!(argument_annotation);
success_test!(texture);
success_test!(cast);
success_test!(array_literal);
success_test!(fn_inlining);
success_test!(nested_fn_inlining);
success_test!(reference);
success_test!(generic_identity);
success_test!(shadow);
success_test!(let_in_let);
success_test!(block_in_let);
success_test!(match_in_let);
success_test!(block_in_application);
success_test!(match_in_application);
success_test!(let_in_application);
success_test!(mutating_block_lift);
success_test!(return_in_match);
success_test!(globally_mutating_block_lift);
success_test!(block_purity_filter);
success_test!(matrix);
success_test!(block_deexpressionify);
success_test!(block_deexpressionify_2);
success_test!(global_var_effect);
success_test!(array_assignment);
success_test!(variadic_arithmetic);
success_test!(application_in_return);
success_test!(deexpressionify_in_return);
success_test!(enum_generic_option);
success_test!(enum_generic_either);
success_test!(match_enum);
success_test!(match_enum_generic);
success_test!(option_map);
success_test!(swizzle_assignment);
success_test!(either_match);
success_test!(nested_option);
success_test!(enum_of_struct);
success_test!(address_space);
success_test!(monomorphized_name_collision);
success_test!(inlined_fn_name_collision);
success_test!(override_const);
success_test!(shadowed_arg);
success_test!(var_arg);
success_test!(angle_bracket);
success_test!(overload);
success_test!(overload_builtin);
success_test!(trig);
success_test!(any_and_all);
success_test!(vecb);
success_test!(break_in_match);
success_test!(array_literal_access);
success_test!(location_inference);
success_test!(double_monomorphize);
success_test!(complex_monomorphize);
success_test!(nested_user_reference);
success_test!(bind_discard);
success_test!(non_return_if);
success_test!(unused_enum);
success_test!(vector_equality);
success_test!(match_non_name_scrutinee);
success_test!(type_order);
success_test!(nested_let_name_reuse);
success_test!(local_binding_inlining);
success_test!(local_fn);
success_test!(returned_array);
success_test!(returned_array_generic);
success_test!(local_fn_inferred_type);
success_test!(passed_closure);
success_test!(float_match);
success_test!(for_multitype);
success_test!(complex_loops);
success_test!(reserved_wgsl_names);
success_test!(static_array_length);
success_test!(def_sized_array);
success_test!(closure);
success_test!(return_fn);
success_test!(okhsl);
success_test!(bitcast);
success_test!(mixed_vec);
success_test!(flowers);
success_test!(enum_type);

success_test!(generic_constraint_success);
success_test!(return_success);
success_test!(duplicate_defn_success);
success_test!(exp_after_control_flow_success);
success_test!(vec_match_success);
success_test!(illegal_fn_type_match_success);
success_test!(attributes_success);
success_test!(var_initialization_success);
success_test!(var_name_collision_success);
success_test!(compute_success);
success_test!(dpdx_success);
success_test!(discard_success);
success_test!(arg_annotation_success);
success_test!(return_annotation_success);
success_test!(user_reference_success);
success_test!(ownership_success);

// --- Error tests ---

error_test!(
  illegal_fn_type_struct,
  CompileErrorKind::CantStoreFunctionInDataStructure
);
error_test!(
  generic_constraint_failure,
  CompileErrorKind::FunctionArgumentTypesIncompatible {
    f: TypeStateDescription::Known(TypeDescription::Function {
      arg_types: vec![(
        TypeStateDescription::Known(TypeDescription::Bool),
        vec![TypeConstraintDescription {
          name: "Scalar".into(),
          args: vec![]
        }],
      )],
      return_type: Box::new(TypeStateDescription::Known(
        TypeDescription::Struct("TwoOf".into())
      )),
    }),
    args: vec![TypeStateDescription::Known(TypeDescription::Bool)],
  }
);
error_test!(
  return_failure,
  CompileErrorKind::IncompatibleTypes(
    TypeStateDescription::Known(TypeDescription::U32),
    TypeStateDescription::Known(TypeDescription::F32),
  )
);
error_test!(
  duplicate_defn_failure_duplicate,
  CompileErrorKind::DuplicateFunctionSignature("f".into())
);
error_test!(
  duplicate_defn_failure_builtin,
  CompileErrorKind::FunctionSignatureConflictsWithBuiltin("or".into())
);
error_test!(
  exp_after_control_flow_failure,
  CompileErrorKind::ExpressionAfterControlFlow("break".into())
);
error_test!(
  underscore_name_failure,
  CompileErrorKind::WildcardOutsidePattern
);
error_test!(vec_match_failure, CompileErrorKind::InvalidMatchPattern);
error_test!(
  illegal_fn_type_match_failure,
  CompileErrorKind::CantYieldFunctionFromMatch
);
error_test!(
  attributes_failure_group_only,
  CompileErrorKind::GroupMissingBinding
);
error_test!(
  attributes_failure_binding_only,
  CompileErrorKind::BindingMissingGroup
);
error_test!(
  attributes_failure_no_address,
  CompileErrorKind::NeedAddressAnnotation
);
error_test!(
  attributes_failure_private_binding,
  CompileErrorKind::DisallowedGroupAndBinding(VariableAddressSpace::Private)
);
error_test!(
  attributes_failure_texture_no_binding,
  CompileErrorKind::NeedsGroupAndBinding("(Texture2D f32)".into())
);
error_test!(
  attributes_failure_texture_address_only,
  CompileErrorKind::NeedsGroupAndBinding("(Texture2D f32)".into())
);
error_test!(
  var_initialization_failure_uniform,
  CompileErrorKind::DisallowedInitializationValue(
    VariableAddressSpace::Uniform
  )
);
error_test!(
  var_initialization_failure_storage,
  CompileErrorKind::DisallowedInitializationValue(
    VariableAddressSpace::StorageRead
  )
);
error_test!(
  var_name_collision_failure_def,
  CompileErrorKind::VariableNameCollision("a".into())
);
error_test!(
  var_name_collision_failure_defn,
  CompileErrorKind::VariableFunctionNameCollision("a".into())
);
error_test!(
  compute_failure_no_workgroup,
  CompileErrorKind::ComputeEntryMissingWorkgroupSize
);
error_test!(
  compute_failure_workgroup_on_fragment,
  CompileErrorKind::InvalidWorkgroupSizeAnnotation
);
error_test!(
  dpdx_failure,
  CompileErrorKind::FragmentExclusiveFunctionOutsideFragment("dpdx".into()),
  CompileErrorKind::FragmentExclusiveFunctionOutsideFragment("dpdy".into()),
);
error_test!(
  discard_failure_args,
  CompileErrorKind::BuiltInOperatorTakesNoArguments("discard".into())
);
error_test!(
  discard_failure_vertex,
  CompileErrorKind::UnboundName("discard".into())
);
error_test!(
  discard_failure_vertex_helper,
  CompileErrorKind::DiscardOutsideFragment
);
error_test!(
  arg_annotation_failure_wrong_type,
  CompileErrorKind::InvalidBuiltinType("front-facing".into())
);
error_test!(
  arg_annotation_failure_duplicate,
  CompileErrorKind::DuplicateBuiltinAttribute(
    InputOrOutput::Input,
    "front-facing".into(),
  )
);
error_test!(
  arg_annotation_failure_wrong_entry,
  CompileErrorKind::InvalidBuiltinForEntryPoint(
    "front-facing".into(),
    InputOrOutput::Input,
    "vertex".into(),
  )
);
error_test!(
  arg_annotation_failure_non_entry,
  CompileErrorKind::IOAttributesOnNonEntry
);
error_test!(
  arg_annotation_failure_invalid_singular,
  CompileErrorKind::InvalidArgumentAnnotation
);
error_test!(
  arg_annotation_failure_invalid_map,
  CompileErrorKind::InvalidArgumentAnnotation
);
error_test!(
  return_annotation_failure_non_entry,
  CompileErrorKind::IOAttributesOnNonEntry
);
error_test!(
  return_annotation_failure_invalid,
  CompileErrorKind::InvalidReturnAnnotations
);
error_test!(
  user_reference_failure_workgroup,
  CompileErrorKind::PassedReferenceFromInvalidAddressSpace(
    VariableAddressSpace::Workgroup
  )
);
error_test!(
  user_reference_failure_uniform,
  CompileErrorKind::PassedReferenceFromInvalidAddressSpace(
    VariableAddressSpace::Uniform
  ),
  CompileErrorKind::ImmutableOwnedPassedAsMutableReference,
);
error_test!(
  user_reference_failure_handle,
  CompileErrorKind::PassedReferenceFromInvalidAddressSpace(
    VariableAddressSpace::Handle
  )
);
error_test!(
  user_reference_failure_immutable,
  CompileErrorKind::ImmutableOwnedPassedAsMutableReference
);
error_test!(
  ownership_failure_let_immutable,
  CompileErrorKind::ImmutableOwnedPassedAsMutableReference
);
error_test!(
  ownership_failure_ref_immutable,
  CompileErrorKind::ReferenceMustBeMutable
);
error_test!(
  ownership_failure_owned_immutable,
  CompileErrorKind::ImmutableOwnedPassedAsMutableReference
);
error_test!(duplicate_field, CompileErrorKind::DuplicateStructFieldName);
error_test!(
  duplicate_variant_simple,
  CompileErrorKind::DuplicateEnumVariantName
);
error_test!(
  duplicate_variant_with_data,
  CompileErrorKind::DuplicateEnumVariantName
);
error_test!(
  illegal_fn_type_enum,
  CompileErrorKind::CantStoreFunctionInDataStructure
);

error_test!(reserved_name_struct_struct, CompileErrorKind::InvalidName);
error_test!(reserved_name_struct_defn, CompileErrorKind::InvalidName);
error_test!(reserved_name_struct_def, CompileErrorKind::InvalidName);
error_test!(reserved_name_field_def, CompileErrorKind::InvalidName);
error_test!(reserved_name_fn_defn, CompileErrorKind::InvalidName);
error_test!(reserved_name_fn_enum, CompileErrorKind::InvalidName);
error_test!(reserved_name_fn_struct, CompileErrorKind::InvalidName);
error_test!(reserved_name_fn_number, CompileErrorKind::InvalidName);
error_test!(reserved_name_fn_let, CompileErrorKind::InvalidName);
error_test!(reserved_name_fn_match, CompileErrorKind::InvalidName);
error_test!(reserved_name_fn_if, CompileErrorKind::InvalidName);
error_test!(reserved_name_fn_when, CompileErrorKind::InvalidName);
error_test!(reserved_name_fn_return, CompileErrorKind::InvalidName);
error_test!(reserved_name_fn_break, CompileErrorKind::InvalidName);
error_test!(reserved_name_fn_continue, CompileErrorKind::InvalidName);
error_test!(reserved_name_let_defn, CompileErrorKind::InvalidName);
error_test!(invalid_name_tilde, CompileErrorKind::InvalidName);
error_test!(invalid_name_dot, CompileErrorKind::InvalidName);
error_test!(invalid_name_leading_number, CompileErrorKind::InvalidName);
