class AttributeArgumentListElement {
  string Category = "Attribute";
}
def AnyAttributeArgumentListElement : AttributeArgumentListElement, Syntax;

def AttributeArgumentListSyntax : SyntaxCollection<AnyAttributeArgumentListElement> {
  let Category = "Attribute";
}

def BalancedTokensSyntax : AttributeArgumentListElement, Syntax {
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

def AttributeListSyntax : SyntaxCollection<AttributeSyntax> {
  let Category = "Attribute";
}
