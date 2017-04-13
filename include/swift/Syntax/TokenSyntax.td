class Keyword<string expectedText> : Token<expectedText>;

def WhileToken : Keyword<"while">;
def Colon: Token<":">;
def Identifier : Token<"">;
def LeftBrace : Token<"{">;
def RightBrace : Token<"}">;
def LeftSquare: Token<"[">;
def RightSquare: Token<"]">;
def Ampersand: Token<"&">;
def QuestionMark: Token<"?">;
def ExclamationMark: Token<"!">;

