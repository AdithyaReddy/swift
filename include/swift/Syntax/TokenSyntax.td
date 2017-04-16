class Keyword<string spelling, string kind> : TokenSyntax<spelling, kind>;

def AsTokenSyntax : Keyword<"as", "kw_as">;
def BreakTokenSyntax : Keyword<"break", "kw_break">;
def CaseTokenSyntax : Keyword<"case", "kw_case">;
def CatchTokenSyntax : Keyword<"catch", "kw_catch">;
def ContinueTokenSyntax : Keyword<"continue", "kw_continue">;
def DefaultTokenSyntax : Keyword<"default", "kw_default">;
def DeferTokenSyntax : Keyword<"defer", "kw_defer">;
def DoTokenSyntax : Keyword<"do", "kw_do">;
def ElseTokenSyntax : Keyword<"else", "kw_else">;
def FallthroughTokenSyntax : Keyword<"fallthrough", "kw_fallthrough">;
def ForTokenSyntax : Keyword<"for", "kw_for">;
def GuardTokenSyntax : Keyword<"guard", "kw_guard">;
def IfTokenSyntax : Keyword<"if", "kw_if">;
def InTokenSyntax : Keyword<"in", "kw_in">;
def InoutTokenSyntax : Keyword<"inout", "kw_inout">;
def IsTokenSyntax : Keyword<"is", "kw_is">;
def LetTokenSyntax : Keyword<"let", "kw_let">;
def RepeatTokenSyntax : Keyword<"repeat", "kw_repeat">;
def ReturnTokenSyntax : Keyword<"return", "kw_return">;
def SwitchTokenSyntax : Keyword<"swift", "kw_switch">;
def ThrowTokenSyntax : Keyword<"throw", "kw_throw">;
def VarTokenSyntax : Keyword<"var", "kw_var">;
def WhereTokenSyntax : Keyword<"where", "kw_where">;
def WhileTokenSyntax : Keyword<"while", "kw_while">;
def WildcardTokenSyntax : Keyword<"_", "kw__">;

def PoundAvailableTokenSyntax : Keyword<"#available", "pound_available">;
def PoundEndifTokenSyntax : Keyword<"#endif", "pound_endif">;
def PoundElseTokenSyntax : Keyword<"#else", "pound_else">;
def PoundElseifTokenSyntax : Keyword<"#elseif", "pound_elseif">;
def PoundIfTokenSyntax : Keyword<"#if", "pound_if">;
def PoundSourceLocationTokenSyntax : Keyword<"#sourceLocation", "pound_sourceLocation">;

def ArrowTokenSyntax: TokenSyntax<"->", "arrow">;
def AtSignTokenSyntax: TokenSyntax<"@", "at_sign">;
def ColonTokenSyntax: TokenSyntax<":", "colon">;
def SemicolonTokenSyntax : TokenSyntax<";", "semi">;
def CommaTokenSyntax: TokenSyntax<",", "comma">;
def PeriodTokenSyntax: TokenSyntax<".", "period">;
def PeriodPrefixTokenSyntax: TokenSyntax<".", "period_prefix">;
def IdentifierTokenSyntax : TokenSyntax<"", "identifier">;
def LeftParenTokenSyntax: TokenSyntax<"(", "l_paren">;
def RightParenTokenSyntax: TokenSyntax<")", "r_paren">;
def LeftBraceTokenSyntax : TokenSyntax<"{", "l_brace">;
def RightBraceTokenSyntax : TokenSyntax<"}", "r_brace">;
def LeftSquareTokenSyntax: TokenSyntax<"[", "l_square">;
def RightSquareTokenSyntax: TokenSyntax<"]", "r_square">;
def AmpersandTokenSyntax: TokenSyntax<"&", "amp_prefix">;
def QuestionMarkPostfixTokenSyntax: TokenSyntax<"?", "question_postfix">;
def QuestionMarkInfixTokenSyntax: TokenSyntax<"?", "question_infix">;
def ExclamationMarkTokenSyntax: TokenSyntax<"!", "exclaim_postfix">;

def IntegerLiteralTokenSyntax : TokenSyntax<"", "integer_literal">;
def FloatingLiteralTokenSyntax : TokenSyntax<"", "floating_literal">;
def StringLiteralTokenSyntax : TokenSyntax<"", "string_literal">;

