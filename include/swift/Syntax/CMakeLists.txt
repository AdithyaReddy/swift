set(syntax_gen_categories "DeclSyntax;StmtSyntax;ExprSyntax;TypeSyntax;PatternSyntax;SyntaxFactory;SyntaxRewriter")
foreach(category ${syntax_gen_categories})
  set(syntax_gen_impl_output_dir "${CMAKE_CURRENT_BINARY_DIR}")
  swift_add_syntax_generated_source("${category}.h"
    OUTPUT_DIR ${syntax_gen_impl_output_dir}
    ACTION interface
    CATEGORY ${category}
    TARGET_LANGUAGE c++)
endforeach()
