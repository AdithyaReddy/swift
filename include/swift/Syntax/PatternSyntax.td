class BindablePattern : Pattern;
class AnyBindablePatter : BindablePattern;

def WilcardPatternSyntax : BindablePattern {
 Layout Wilcard = Layout<WildcardToken>;
 Layout TypeAnnotation = OptionalLayout<TypeAnnotactionSyntax>;
}

def IdentifierPatternSyntax : BindablePattern {
  Layout Identifier = Layout<Identifier>;
  Layout TypeAnnotation = OptionalLayout<TypeAnnotactionSyntax>;
}

def LabelSyntax : Syntax {
  Layout Identifier = Layout<IdentifierToken>;
  Layout Colon = Layout<ColonToken>;
}
def TuplePatternElementSyntax : Syntax {
  Layout Label : OptionalLayout<LabelSyntax>;
  Layout Pattern : Layout<AnyPattern>;
  Layout Comma : Layout<LabelSyntax>;
}
def TuplePatternElementListSyntax : SyntaxCollection<TuplePatternElementSyntax>;
def TuplePatternSyntax : BindablePattern {
  Layout OpenParen = Layout<LeftParenToken>;
  Layout Elements = Layout<TuplePatternElementListSyntax>;
  Layout CloseParen = Layout<RightParenToken>;
  Layout TypeAnnotation = OptionalLayout<TypeAnnotationSyntax>;
}

def ValueBindingPatternSyntax : Pattern {
  Layout LetOrVar = Layout<TokenChoice<list<LetToken, VarToken>>;
  Layout ValuePattern = Layout<AnyBindablePattern>;
}

