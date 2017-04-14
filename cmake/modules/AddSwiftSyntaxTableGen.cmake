include(SwiftAddCustomCommandTarget)

function(swift_add_syntax_generated_source category)
  cmake_parse_arguments(
    SYNTAX_GEN # prefix
    "" # options
    "OUTPUT_DIR;ACTION;TARGET_LANGUAGE" # single-value args
    "" # multi-value args
    ${ARGN})
  precondition(category "Category is required")
  precondition(SYNTAX_GEN_OUTPUT_DIR "Output dir is required")
  precondition(SYNTAX_GEN_ACTION "Action is required")
  precondition(SYNTAX_GEN_TARGET_LANGUAGE "Target language is required")
  set(SYNTAX_GEN_CATEGORY ${category})

  set(out_filename ${SYNTAX_GEN_CATEGORY})
  if (SYNTAX_GEN_CATEGORY IN_LIST SYNTAX_GEN_SYNTAX_CATEGORIES)
    set(out_filename "${out_filename}Syntax")
  endif()

  if (${SYNTAX_GEN_ACTION} STREQUAL "interface")
    set(out_filename "${out_filename}.h")
  elseif (${SYNTAX_GEN_ACTION} STREQUAL "implementation")
    set(out_filename "${out_filename}.cpp")
  endif()

  precondition(out_filename "Calculated output filename is empty?!")

  set(syntax_tblgen_tool "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bin/swift-syntax-tblgen")
  set(syntax_gen_output_file "${SYNTAX_GEN_OUTPUT_DIR}/${out_filename}")
  set(syntax_gen_td_file "${CMAKE_SOURCE_DIR}/include/swift/Syntax/Syntax.td")
  set(syntax_gen_include_dir "${CMAKE_SOURCE_DIR}/include/swift/Syntax")

  # "return" this string to the caller. :roll-eyes:
  set(out_filename ${syntax_gen_output_file} PARENT_SCOPE)

  add_custom_command_target(
    syntax_gen_output_file
    COMMAND
      "${CMAKE_COMMAND}" -E make_directory "${SYNTAX_GEN_OUTPUT_DIR}"
    COMMAND
    "${syntax_tblgen_tool}" "-${SYNTAX_GEN_ACTION}" "-category" "${SYNTAX_GEN_CATEGORY}" "-language" "${SYNTAX_GEN_TARGET_LANGUAGE}" "-o" "${syntax_gen_output_file}" "-I" "${syntax_gen_include_dir}" "${syntax_gen_td_file}"
    OUTPUT "${syntax_gen_output_file}"
    DEPENDS ${syntax_tblgen_tool}
    )
  set_source_files_properties("${syntax_gen_output_file}" GENERATED)

  add_dependencies(swift-syntax-all-generated-source ${syntax_gen_output_file})

endfunction()
