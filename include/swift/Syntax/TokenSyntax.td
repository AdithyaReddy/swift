class Keyword<string spelling, string kind> : Token<spelling, kind>;

def WhileToken : Keyword<"while", "kw_while">;
def InoutToken : Keyword<"inout", "kw_inout">;

def ArrowToken: Token<"->", "arrow">;
def ColonToken: Token<":", "colon">;
def CommaToken: Token<",", "comma">;
def PeriodToken: Token<".", "period">;
def PeriodPrefixToken: Token<".", "period_prefix">;
def IdentifierToken : Token<"", "identifier">;
def LeftParenToken: Token<"(", "l_paren">;
def RightParenToken: Token<")", "r_paren">;
def LeftBraceToken : Token<"{", "l_brace">;
def RightBraceToken : Token<"}", "r_brace">;
def LeftSquareToken: Token<"[", "l_square">;
def RightSquareToken: Token<"]", "r_square">;
def AmpersandToken: Token<"&", "amp_prefix">;
def QuestionMarkPostfixToken: Token<"?", "question_postfix">;
def QuestionMarkInfixToken: Token<"?", "question_infix">;
def ExclamationMarkToken: Token<"!", "exclaim_postfix">;

