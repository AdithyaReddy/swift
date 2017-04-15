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

class Stmt : Syntax {
  let Category = "Stmt";
}
def AnyStmt: Stmt;

class Expr : Syntax {
  let Category = "Expr";
}
def AnyExpr : Expr;

class Decl : Syntax {
  let Category = "Decl";
}
def AnyDecl : Decl;

class Pattern : Syntax {
  let Category = "Pattern";
}
def AnyPattern : Pattern;

class Type : Syntax {
  let Category = "Type";
}
def AnyType : Type;

class Attribute : Syntax;

class Token<string spelling, string kind> : Syntax {
  string Spelling = spelling;
  string Kind = kind;
  let Category = "Token";
}
// FIXME: Use a list of possible tokens?
def AnyToken : Token<"", "">;

class TokenChoice<list<Token> choices> : Syntax {
  list<Token> Choices = choices;
}


