class AttributeArgumentListElement;
def AnyAttributeArgumentListElement : AttributeArgumentListElement, Syntax;

def AttributeArgumentListSyntax : SyntaxCollection<AnyAttributeArgumentListElement>;

def BalancedTokensSyntax : AttributeArgumentListElement {
  Layout LeftMatch = Layout<AnyToken>;
  Layout BalancedTokens = Layout<AttributeArgumentListSyntax>;
  Layout RightMatch = Layout<AnyToken>;
}
// FIXME: AnyToken -> blacklist ()[]{}
def TokenListSyntax : AttributeArgumentListElement, SyntaxCollection<AnyToken>;
def AttributeSyntax : Syntax {
  Layout AtSign = Layout<AtSignToken>;
  Layout AttributeName = Layout<IdentifierToken>;
  Layout OpenParen = Layout<LeftParenToken>;
  Layout AttributeArguments = Layout<AttributeArgumentListSyntax>;
  Layout CloseParen = Layout<RightParenToken>;
}

def AttributeListSyntax : SyntaxCollection<AttributeSyntax>;
