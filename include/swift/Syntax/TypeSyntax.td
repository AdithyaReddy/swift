// FIXME: Dummy def
class TypeIdentifier : Syntax;
def AnyTypeIdentifier : TypeIdentifier;

def ArrayType : Type {
  Token LeftBracket = LeftSquare;
  Type ElementType = AnyType;
  Token RightBracket = RightSquare;
}

def DictionaryType : Type {
  Type KeyType = AnyType;
  Token Colon = Colon;
  Type ValueType = AnyType;
}

def OptionalType : Type {
  Type ValueType = AnyType;
  Token QuestionMark = QuestionMark;
}

def ImplicityUnwrappedOptionalType : Type {
  Type AnyType = AnyType;
  Token Exclamation = Exclamation;
}

def ProtocolCompositionListElement : Syntax {
  TypeIdentifier ProtocolType = AnyTypeIdentifier;
  Optional MaybeAmp = Optional<Ampersand>;
}

def ProtocolCompositionType : Syntax, SyntaxCollection<ProtocolCompositionListElement>;

