
def TryOperatorSyntax : Syntax {
  Layout Try = Layout<TryTokenSyntax>;
  Layout QuestionOrExclamationMark = OptionalLayout<TokenChoice<[QuestionMarkPostfixTokenSyntax, ExclamationMarkTokenSyntax]>>;
}

class PostfixExprSyntax : ExprSyntax;
def AnyPostfixExprSyntax : PostfixExprSyntax;

class LiteralExprSyntax : PostfixExprSyntax;
def PoundLineExprSyntax : LiteralExprSyntax {
  Layout PoundLine = Layout<PoundLineTokenSyntax>;
}
def PoundFileExprSyntax : LiteralExprSyntax {
  Layout PoundFile = Layout<PoundFileTokenSyntax>;
}
def PoundColumnExprSyntax : LiteralExprSyntax {
  Layout PoundColumn = Layout<PoundColumnTokenSyntax>;
}
def PoundFunctionExprSyntax : LiteralExprSyntax {
  Layout PoundFunction = Layout<PoundFunctionTokenSyntax>;
}

def IntegerLiteralExprSyntax : LiteralExprSyntax {
  Layout Sign = OptionalLayout<PrefixOperatorTokenSyntax>; // - or +
  Layout Digits = Layout<IntegerLiteralTokenSyntax>;
}
def FloatLiteralExprSyntax : LiteralExprSyntax {
  Layout Sign = OptionalLayout<PrefixOperatorTokenSyntax>; // - or +
  Layout FloatingDigits = Layout<FloatingLiteralTokenSyntax>;
}
def StringLiteralExprSyntax : LiteralExprSyntax {
  Layout String
}

class PrefixExprSyntax : ExprSyntax;
def AnyPrefixExprSyntax : PrefixExprSyntax;

def InOutExprSyntax : PrefixExprSyntax {
  Layout Ampersand = Layout<AmpersandTokenSyntax>;
  Layout Identifier = Layout<IdentifierTokenSyntax>;
}
def PrefixOperatorExprSyntax : PrefixExprSyntax {
  Layout Operator = OptionalLayout<PrefixOperatorTokenSyntax>;
  Layout PostfixExpression = Layout<AnyPostfixExprSyntax>;
}

def BinaryExprSyntax:
def BinaryExprListSyntax:
def ExpressionSyntax : ExprSyntax {
  Layout Try = OptionalLayout<TryOperatorSyntax>;
  Layout PrefixExpression = Layout<AnyPrefixExpr>;
  Layout BinaryExpressions = OptionaLayout<BinaryExprListSyntax>;
}

def ExpressionListElement : Syntax {
  Layout Expression = Layout<AnyExpr>;
  Layout Comma = Layout<CommaToken>;
}
def ExpressionListSyntax : SyntaxCollection<ExpressionListElement>;
