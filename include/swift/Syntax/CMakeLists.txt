include(SwiftSyntaxLibrary)

foreach(category ${SYNTAX_GEN_CATEGORIES})
  set(syntax_gen_impl_output_dir "${CMAKE_CURRENT_BINARY_DIR}")
  set(out_filename)
  swift_add_syntax_generated_source(${category}
    OUTPUT_DIR ${syntax_gen_impl_output_dir}
    ACTION interface
    TARGET_LANGUAGE c++)
endforeach()
