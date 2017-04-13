class Syntax {
  bit Required = 1;
}

class SyntaxCollection<Syntax element> {
  Syntax Element = element;
}

class Optional<Syntax node> : Syntax {
  Syntax Node = node;
  let Required = 0;
}

class Stmt : Syntax;
def AnyStmt: Stmt;

class Expr : Syntax;
def AnyExpr : Expr;

class Decl : Syntax;
def AnyDecl : Decl;

class Pattern : Syntax;
def AnyPattern : Pattern;

class Type : Syntax;
def AnyType : Type;

class Token<string expectedText> : Syntax {
  string ExpectedText = expectedText;
}

