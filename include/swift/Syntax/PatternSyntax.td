class BindablePatternSyntax : PatternSyntax;
def AnyBindablePatternSyntax : BindablePatternSyntax;

def WilcardPatternSyntax : BindablePatternSyntax {
 Layout Wildcard = Layout<WildcardTokenSyntax>;
 Layout TypeAnnotation = OptionalLayout<TypeAnnotationSyntax>;
}
def IdentifierPatternSyntax : BindablePatternSyntax {
  Layout Identifier = Layout<IdentifierTokenSyntax>;
  Layout TypeAnnotation = OptionalLayout<TypeAnnotationSyntax>;
}
def LabelSyntax : Syntax {
  Layout Identifier = Layout<IdentifierTokenSyntax>;
  Layout Colon = Layout<ColonTokenSyntax>;
}
def TuplePatternElementSyntax : Syntax {
  Layout Label = OptionalLayout<LabelSyntax>;
  Layout PatternSyntax = Layout<AnyPatternSyntax>;
  Layout Comma = Layout<LabelSyntax>;
}
def TuplePatternElementListSyntax : SyntaxCollection<TuplePatternElementSyntax>;
def TuplePatternSyntax : BindablePatternSyntax {
  Layout OpenParen = Layout<LeftParenTokenSyntax>;
  Layout Elements = Layout<TuplePatternElementListSyntax>;
  Layout CloseParen = Layout<RightParenTokenSyntax>;
  Layout TypeAnnotation = OptionalLayout<TypeAnnotationSyntax>;
}
def OptionalPatternSyntax : BindablePatternSyntax {
  Layout Identifier = Layout<IdentifierTokenSyntax>;
  Layout QuestionMark = Layout<QuestionMarkPostfixTokenSyntax>;
}
def AsTypePatternSyntax : BindablePatternSyntax {
  Layout PatternSyntax = Layout<AnyBindablePatternSyntax>;
  Layout As = Layout<AsTokenSyntax>;
  Layout Type = Layout<AnyTypeSyntax>;
}
def EnumCasePatternSyntax : PatternSyntax {
  Layout TypeIdentifier = OptionalLayout<TypeIdentifierSyntax>;
  Layout Period = Layout<PeriodTokenSyntax>;
  Layout CaseName = Layout<IdentifierTokenSyntax>;
  Layout AssociatedTuple = OptionalLayout<TuplePatternSyntax>;
}
def IsTypePatternSyntax : PatternSyntax {
  Layout Is = Layout<IsTokenSyntax>;
  Layout Type = Layout<AnyTypeSyntax>;
}
def ValueBindingPatternSyntax : PatternSyntax {
  Layout LetOrVar = Layout<TokenChoice<[LetTokenSyntax, VarTokenSyntax]>>;
  Layout ValuePatternSyntax = Layout<AnyBindablePatternSyntax>;
}
def ExpressionPatternSyntax : PatternSyntax {
  Layout Expression = Layout<AnyExprSyntax>;
}

