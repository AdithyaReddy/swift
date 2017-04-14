include(SwiftAddCustomCommandTarget)

function(swift_add_syntax_generated_source category)
  cmake_parse_arguments(
    SYNTAX_GEN # prefix
    "" # options
    "OUTPUT_DIR;ACTION;CATEGORY;TARGET_LANGUAGE" # single-value args
    "" # multi-value args
    ${ARGN})
  precondition(SYNTAX_GEN_OUTPUT_DIR "Output dir is required")
  precondition(SYNTAX_GEN_ACTION "Action is required")
  precondition(SYNTAX_GEN_CATEGORY "Category is required")
  precondition(SYNTAX_GEN_TARGET_LANGUAGE "Target language is required")

  set(filename ${category})
  if (category IN_LIST SYNTAX_GEN_SYNTAX_CATEGORIES)
    set(filename "${filename}Syntax")
  endif()

  if (${SYNTAX_GEN_ACTION} STREQUAL "interface")
    set(filename "${filename}.h")
  elseif (${SYNTAX_GEN_ACTION} STREQUAL "implementation")
    set(filename "${filename}.cpp")
  endif()

  set(syntax_tblgen_tool "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bin/swift-syntax-tblgen")
  set(syntax_gen_output_file "${SYNTAX_GEN_OUTPUT_DIR}/${filename}")
  set(syntax_gen_td_file "${CMAKE_SOURCE_DIR}/include/swift/Syntax/Syntax.td")
  set(syntax_gen_include_dir "${CMAKE_SOURCE_DIR}/include/swift/Syntax")

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
