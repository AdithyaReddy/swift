add_subdirectory(TableGen)

include(SwiftSyntaxLibrary)

set(generated_sources)
foreach(category ${SYNTAX_GEN_CATEGORIES})
  set(syntax_gen_output_dir "${CMAKE_CURRENT_BINARY_DIR}")
  swift_add_syntax_generated_source("${category}.cpp"
    OUTPUT_DIR ${syntax_gen_output_dir}
    ACTION implementation
    CATEGORY ${category}
    TARGET_LANGUAGE c++)
  list(APPEND generated_sources "${syntax_gen_output_dir}/${category}.cpp")
endforeach()

add_swift_library(swiftSyntax STATIC
  ${generated_sources}
  Format.cpp
  TokenSyntax.cpp
  LegacyASTTransformer.cpp
  Trivia.cpp
  RawSyntax.cpp
  Syntax.cpp
  SyntaxData.cpp
  DEPENDS swift-syntax-all-generated-source
)

