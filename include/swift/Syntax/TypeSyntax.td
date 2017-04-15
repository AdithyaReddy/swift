def TypeAnnotationSyntax : Syntax {
  Layout Attributes = Layout<AttributeListSyntax>;
  Layout InOut = Layout<InoutToken>;
  Layout Type = Layout<AnyType>;
}

def GenericArgumentClauseElementSyntax : Syntax {
  Layout GenericArgument = Layout<AnyType>;
  Layout Comma = Layout<CommaToken>;
}
def GenericArgumentClauseSyntax : SyntaxCollection<GenericArgumentClauseElementSyntax>;
def TypeIdentifierSyntax : Syntax {
  Layout TypeName = Layout<IdentifierToken>;
  Layout GenericArgumentClause = Layout<GenericArgumentClauseSyntax>;
  Layout Period = Layout<PeriodToken>;
  Layout TypeIdentifier = Layout<TypeIdentifierSyntax>;
}

def TupleTypeElementSyntax : Syntax {
  Layout Label = Layout<IdentifierToken>;
  Layout Colon = Layout<ColonToken>;
  Layout TypeAttributes = Layout<SyntaxCollection<AttributeSyntax>>;
  Layout InOut = Layout<InoutToken>;
  Layout Type = Layout<AnyType>;
  Layout Comma = Layout<CommaToken>;
}
def TupleTypeElementListSyntax : SyntaxCollection<TupleTypeElementSyntax>;
def TupleTypeSyntax : Type {
  Layout LeftParen = Layout<LeftParenToken>;
  Layout ElementList = Layout<TupleTypeElementListSyntax>;
  Layout RightParen = Layout<RightParenToken>;
}

def FunctionTypeArgumentSyntax : Syntax {
  Layout ExternalParameterName = Layout<IdentifierToken>;
  Layout LocalParameterName = Layout<IdentifierToken>;
  Layout Colon = Layout<ColonToken>;
  Layout TypeAttributes = Layout<SyntaxCollection<AttributeSyntax>>;
  Layout Inout = Layout<InoutToken>;
  Layout Type = Layout<AnyType>;
}
def FunctionTypeArgumentListSyntax : SyntaxCollection<FunctionTypeArgumentSyntax>;
def FunctionTypeSyntax : Type {
  Layout TypeAttributes = Layout<AttributeListSyntax>;
  Layout OpenParen = Layout<LeftParenToken>;
  Layout ArgumentList = Layout<FunctionTypeArgumentListSyntax>;
  Layout CloseParen = Layout<RightParenToken>;
  Layout ThrowsOrRethrows = Layout<AnyToken>;
  Layout Arrow = Layout<ArrowToken>;
  Layout ReturnType = Layout<AnyType>;
}

def ArrayTypeSyntax : Type {
  Layout LeftBracket = Layout<LeftSquareToken>;
  Layout ElementType = Layout<AnyType>;
  Layout RightBracket = Layout<RightSquareToken>;
}

def DictionaryTypeSyntax : Type {
  Layout LeftSquare = Layout<LeftSquareToken>;
  Layout KeyType = Layout<AnyType>;
  Layout Colon = Layout<ColonToken>;
  Layout ValueType = Layout<AnyType>;
  Layout RightSquare = Layout<RightSquareToken>;
}

def OptionalTypeSyntax : Type {
  Layout ValueType = Layout<AnyType>;
  Layout QuestionMark = Layout<QuestionMarkPostfixToken>;
}

def ImplicityUnwrappedOptionalTypeSyntax : Type {
  Layout ValueType = Layout<AnyType>;
  Layout ExclamationMark = Layout<ExclamationMarkToken>;
}

def ProtocolCompositionElementSyntax : Syntax {
  Layout ProtocolType = Layout<TypeIdentifierSyntax>;
  Layout Ampersand = Layout<AmpersandToken>;
}

def ProtocolCompositionTypeSyntax : SyntaxCollection<ProtocolCompositionElementSyntax>;

def MetatypeTypeSyntax : Type {
  Layout TypeValue = Layout<AnyType>;
  Layout Period = Layout<PeriodToken>;
  // FIXME: Add set of allowable values to assert
  Layout TypeOrProtocol = Layout<IdentifierToken>;
}

