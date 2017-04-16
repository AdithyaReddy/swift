class AttributeArgumentListElement {
  string Category = "Attribute";
}
def AnyAttributeArgumentListElement : AttributeArgumentListElement, Syntax;

def AttributeArgumentListSyntax : SyntaxCollection<AnyAttributeArgumentListElement> {
  let Category = "Attribute";
}

def BalancedTokenSyntaxsSyntax : AttributeArgumentListElement, Syntax {
  Layout LeftMatch = Layout<AnyTokenSyntax>;
  Layout BalancedTokenSyntaxs = Layout<AttributeArgumentListSyntax>;
  Layout RightMatch = Layout<AnyTokenSyntax>;
}
// FIXME: AnyTokenSyntax -> blacklist ()[]{}
def TokenSyntaxListSyntax : AttributeArgumentListElement, SyntaxCollection<AnyTokenSyntax>;
def AttributeSyntax : Syntax {
  Layout AtSign = Layout<AtSignTokenSyntax>;
  Layout AttributeName = Layout<IdentifierTokenSyntax>;
  Layout OpenParen = Layout<LeftParenTokenSyntax>;
  Layout AttributeArguments = Layout<AttributeArgumentListSyntax>;
  Layout CloseParen = Layout<RightParenTokenSyntax>;
}

def AttributeListSyntax : SyntaxCollection<AttributeSyntax> {
  let Category = "Attribute";
}
