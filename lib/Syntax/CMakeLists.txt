add_subdirectory(TableGen)

set(syntax_gen_impl_output_dir "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}")
swift_add_syntax_generated_source(DeclSyntax.cpp
  OUTPUT_DIR ${syntax_gen_impl_output_dir}
  ACTION implementation
  CATEGORY Decl
  TARGET_LANGUAGE c++)

add_swift_library(swiftSyntax STATIC
  "${syntax_gen_impl_output_dir}/DeclSyntax.cpp"
  Format.cpp
  TokenSyntax.cpp
  LegacyASTTransformer.cpp
  Trivia.cpp
  RawSyntax.cpp
  Syntax.cpp
  SyntaxData.cpp
  DEPENDS swift-syntax-all-generated-source
)

