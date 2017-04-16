def TypeAnnotationSyntax : Syntax {
  Layout Attributes = Layout<AttributeListSyntax>;
  Layout InOut = Layout<InoutTokenSyntax>;
  Layout Type = Layout<AnyTypeSyntax>;
}

def GenericArgumentClauseElementSyntax : Syntax {
  Layout GenericArgument = Layout<AnyTypeSyntax>;
  Layout Comma = Layout<CommaTokenSyntax>;
}
def GenericArgumentClauseSyntax : SyntaxCollection<GenericArgumentClauseElementSyntax>;
def TypeIdentifierSyntax : Syntax {
  Layout TypeName = Layout<IdentifierTokenSyntax>;
  Layout GenericArgumentClause = Layout<GenericArgumentClauseSyntax>;
  Layout Period = Layout<PeriodTokenSyntax>;
  Layout TypeIdentifier = Layout<TypeIdentifierSyntax>;
}

def TupleTypeElementSyntax : Syntax {
  Layout Label = Layout<IdentifierTokenSyntax>;
  Layout Colon = Layout<ColonTokenSyntax>;
  Layout TypeAttributes = Layout<SyntaxCollection<AttributeSyntax>>;
  Layout InOut = Layout<InoutTokenSyntax>;
  Layout Type = Layout<AnyTypeSyntax>;
  Layout Comma = Layout<CommaTokenSyntax>;
}
def TupleTypeElementListSyntax : SyntaxCollection<TupleTypeElementSyntax>;
def TupleTypeSyntax : TypeSyntax {
  Layout LeftParen = Layout<LeftParenTokenSyntax>;
  Layout ElementList = Layout<TupleTypeElementListSyntax>;
  Layout RightParen = Layout<RightParenTokenSyntax>;
}

def FunctionTypeArgumentSyntax : Syntax {
  Layout ExternalParameterName = Layout<IdentifierTokenSyntax>;
  Layout LocalParameterName = Layout<IdentifierTokenSyntax>;
  Layout Colon = Layout<ColonTokenSyntax>;
  Layout TypeAttributes = Layout<SyntaxCollection<AttributeSyntax>>;
  Layout Inout = Layout<InoutTokenSyntax>;
  Layout Type = Layout<AnyTypeSyntax>;
}
def FunctionTypeArgumentListSyntax : SyntaxCollection<FunctionTypeArgumentSyntax>;
def FunctionTypeSyntax : TypeSyntax {
  Layout TypeAttributes = Layout<AttributeListSyntax>;
  Layout OpenParen = Layout<LeftParenTokenSyntax>;
  Layout ArgumentList = Layout<FunctionTypeArgumentListSyntax>;
  Layout CloseParen = Layout<RightParenTokenSyntax>;
  Layout ThrowsOrRethrows = Layout<AnyTokenSyntax>;
  Layout Arrow = Layout<ArrowTokenSyntax>;
  Layout ReturnType = Layout<AnyTypeSyntax>;
}

def ArrayTypeSyntax : TypeSyntax {
  Layout LeftBracket = Layout<LeftSquareTokenSyntax>;
  Layout ElementType = Layout<AnyTypeSyntax>;
  Layout RightBracket = Layout<RightSquareTokenSyntax>;
}

def DictionaryTypeSyntax : TypeSyntax {
  Layout LeftSquare = Layout<LeftSquareTokenSyntax>;
  Layout KeyType = Layout<AnyTypeSyntax>;
  Layout Colon = Layout<ColonTokenSyntax>;
  Layout ValueType = Layout<AnyTypeSyntax>;
  Layout RightSquare = Layout<RightSquareTokenSyntax>;
}

def OptionalTypeSyntax : TypeSyntax {
  Layout ValueType = Layout<AnyTypeSyntax>;
  Layout QuestionMark = Layout<QuestionMarkPostfixTokenSyntax>;
}

def ImplicitlyUnwrappedOptionalTypeSyntax : TypeSyntax {
  Layout ValueType = Layout<AnyTypeSyntax>;
  Layout ExclamationMark = Layout<ExclamationMarkTokenSyntax>;
}

def ProtocolCompositionElementSyntax : Syntax {
  Layout ProtocolType = Layout<TypeIdentifierSyntax>;
  Layout Ampersand = Layout<AmpersandTokenSyntax>;
}

def ProtocolCompositionTypeSyntax : SyntaxCollection<ProtocolCompositionElementSyntax>;

def MetatypeTypeSyntax : TypeSyntax {
  Layout TypeValue = Layout<AnyTypeSyntax>;
  Layout Period = Layout<PeriodTokenSyntax>;
  // FIXME: Add set of allowable values to assert
  Layout TypeOrProtocol = Layout<IdentifierTokenSyntax>;
}

