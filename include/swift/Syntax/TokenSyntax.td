class Keyword<string spelling, string kind> : Token<spelling, kind>;

def BreakToken : Keyword<"break", "kw_break">;
def CaseToken : Keyword<"case", "kw_case">;
def CatchToken : Keyword<"catch", "kw_catch">;
def ContinueToken : Keyword<"continue", "kw_continue">;
def DefaultToken : Keyword<"default", "kw_default">;
def DeferToken : Keyword<"defer", "kw_defer">;
def DoToken : Keyword<"do", "kw_do">;
def ElseToken : Keyword<"else", "kw_else">;
def FallthroughToken : Keyword<"fallthrough", "kw_fallthrough">;
def ForToken : Keyword<"for", "kw_for">;
def GuardToken : Keyword<"guard", "kw_guard">;
def IfToken : Keyword<"if", "kw_if">;
def InToken : Keyword<"in", "kw_in">;
def InoutToken : Keyword<"inout", "kw_inout">;
def RepeatToken : Keyword<"repeat", "kw_repeat">;
def ReturnToken : Keyword<"return", "kw_return">;
def SwitchToken : Keyword<"swift", "kw_switch">;
def ThrowToken : Keyword<"throw", "kw_throw">;
def WhereToken : Keyword<"where", "kw_where">;
def WhileToken : Keyword<"while", "kw_while">;

def PoundAvailableToken : Keyword<"#available", "pound_available">;
def PoundEndifToken : Keyword<"#endif", "pound_endif">;
def PoundElseToken : Keyword<"#else", "pound_else">;
def PoundElseifToken : Keyword<"#elseif", "pound_elseif">;
def PoundIfToken : Keyword<"#if", "pound_if">;
def PoundSourceLocationToken : Keyword<"#sourceLocation", "pound_sourceLocation">;

def ArrowToken: Token<"->", "arrow">;
def AtSignToken: Token<"@", "at_sign">;
def ColonToken: Token<":", "colon">;
def SemicolonToken : Token<";", "semi">;
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

def IntegerLiteralToken : Token<"", "integer_literal">;
def FloatingLiteralToken : Token<"", "floating_literal">;
def StringLiteralToken : Token<"", "string_literal">;

