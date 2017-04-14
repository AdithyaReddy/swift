class Syntax;

class Layout<Syntax node> {
  Syntax Node = node;
}

class SyntaxCollection<Syntax element> : Syntax {
  Syntax Element = element;
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

class Token<string spelling, string kind> : Syntax {
  string Spelling = spelling;
  string Kind = kind;
}
def AnyToken : Token<"", "">;

