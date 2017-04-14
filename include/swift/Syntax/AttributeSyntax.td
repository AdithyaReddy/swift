class BalancedTokenSyntaxBase : Syntax;
def AnyBalancedToken : BalancedTokenSyntaxBase;

def BalancedTokenListSyntax : SyntaxCollection<AnyBalancedToken>;
def BalancedTokenSyntax : BalancedTokenSyntaxBase {
  Layout LeftMatch = Layout<AnyToken>;
  Layout BalancedTokens = Layout<BalancedTokenListSyntax>;
  Layout RightMatch = Layout<AnyToken>;
}

def AttributeSyntax : Syntax {
  Layout AtSign = Layout<AtSignToken>;
  Layout AttributeName = Layout<IdentifierToken>;
  Layout BalancedTokens = Layout<BalancedTokenSyntax>;
}
