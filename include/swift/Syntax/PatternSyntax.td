class BindablePattern : Pattern;
def AnyBindablePattern : BindablePattern;

def WilcardPatternSyntax : BindablePattern {
 Layout Wildcard = Layout<WildcardToken>;
 Layout TypeAnnotation = OptionalLayout<TypeAnnotationSyntax>;
}
def IdentifierPatternSyntax : BindablePattern {
  Layout Identifier = Layout<IdentifierToken>;
  Layout TypeAnnotation = OptionalLayout<TypeAnnotationSyntax>;
}
def LabelSyntax : Syntax {
  Layout Identifier = Layout<IdentifierToken>;
  Layout Colon = Layout<ColonToken>;
}
def TuplePatternElementSyntax : Syntax {
  Layout Label = OptionalLayout<LabelSyntax>;
  Layout Pattern = Layout<AnyPattern>;
  Layout Comma = Layout<LabelSyntax>;
}
def TuplePatternElementListSyntax : SyntaxCollection<TuplePatternElementSyntax>;
def TuplePatternSyntax : BindablePattern {
  Layout OpenParen = Layout<LeftParenToken>;
  Layout Elements = Layout<TuplePatternElementListSyntax>;
  Layout CloseParen = Layout<RightParenToken>;
  Layout TypeAnnotation = OptionalLayout<TypeAnnotationSyntax>;
}
def OptionalPatternSyntax : BindablePattern {
  Layout Identifier = Layout<IdentifierToken>;
  Layout QuestionMark = Layout<QuestionMarkPostfixToken>;
}
def AsTypePatternSyntax : BindablePattern {
  Layout Pattern = Layout<AnyBindablePattern>;
  Layout As = Layout<AsToken>;
  Layout Type = Layout<AnyType>;
}
def EnumCasePatternSyntax : Pattern {
  Layout TypeIdentifier = OptionalLayout<TypeIdentifierSyntax>;
  Layout Period = Layout<TokenChoice<[PeriodToken, PeriodPrefixToken]>>;
  Layout CaseName = Layout<IdentifierToken>;
  Layout AssociatedTuple = OptionalLayout<TuplePatternSyntax>;
}
def IsTypePattern : Pattern {
  Layout Is = Layout<IsToken>;
  Layout Type = Layout<AnyType>;
}
def ValueBindingPatternSyntax : Pattern {
  Layout LetOrVar = Layout<TokenChoice<[LetToken, VarToken]>>;
  Layout ValuePattern = Layout<AnyBindablePattern>;
}
def ExpressionPatternSyntax : Pattern {
  Layout Expression = Layout<AnyExpr>;
}

