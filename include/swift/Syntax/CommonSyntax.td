class Syntax {
  string Category = "Common";
}

class Layout<Syntax node> {
  Syntax Node = node;
  bit Required = 1;
}

class OptionalLayout<Syntax node> : Layout<node> {
  let Required = 0;
}

class SyntaxCollection<Syntax element> : Syntax {
  Syntax Element = element;
}

class StmtSyntax : Syntax {
  let Category = "StmtSyntax";
}
def AnyStmtSyntax: StmtSyntax;

class ExprSyntax : Syntax {
  let Category = "ExprSyntax";
}
def AnyExprSyntax : ExprSyntax;

class DeclSyntax : Syntax {
  let Category = "DeclSyntax";
}
def AnyDeclSyntax : DeclSyntax;

class PatternSyntax : Syntax {
  let Category = "PatternSyntax";
}
def AnyPatternSyntax : PatternSyntax;

class TypeSyntax : Syntax {
  let Category = "TypeSyntax";
}
def AnyTypeSyntax : TypeSyntax;

class AttributeSyntax : Syntax;

class TokenSyntax<string spelling, string kind> : Syntax {
  string Spelling = spelling;
  string Kind = kind;
  let Category = "TokenSyntax";
}
// FIXME: Use a list of possible tokens?
def AnyTokenSyntax : TokenSyntax<"", "">;

class TokenChoice<list<TokenSyntax> choices> : Syntax {
  list<TokenSyntax> Choices = choices;
}


