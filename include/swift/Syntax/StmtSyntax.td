def ExpressionStmtSyntax : StmtSyntax {
  Layout Expression = Layout<AnyExprSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}
def DeclarationStmtSyntax : StmtSyntax {
  Layout Declaration = Layout<AnyDeclSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def StatementListSyntax : SyntaxCollection<AnyStmtSyntax>;
def CodeBlockSyntax : Syntax {
  Layout OpenBrace = Layout<LeftBraceTokenSyntax>;
  Layout Statments = Layout<StatementListSyntax>;
  Layout CloseBrace = Layout<RightBraceTokenSyntax>;
}

class LabelledStmtSyntax : StmtSyntax {
  Layout LabelName = Layout<IdentifierTokenSyntax>;
  Layout LabelColon = Layout<ColonTokenSyntax>;
}

class ConditionSyntax : Syntax;
def AnyCondition : ConditionSyntax;

def PlatformVersionSyntax : Syntax {
  Layout MajorVersion = Layout<IntegerLiteralTokenSyntax>;
  Layout MinorPeriod = Layout<PeriodTokenSyntax>;
  Layout MinorVersion = Layout<IntegerLiteralTokenSyntax>;
  Layout BugfixPeriod = Layout<PeriodTokenSyntax>;
  Layout BugfixVersion = Layout<IntegerLiteralTokenSyntax>;
}
def AvailabilityArgumentSyntax : Syntax {
  // FIXME: whitelist?
  Layout PlatformName = Layout<IdentifierTokenSyntax>;
  Layout PlatformVersion = Layout<PlatformVersionSyntax>;
  Layout Comma = Layout<CommaTokenSyntax>;
}
def AvailabilityArgumentList : SyntaxCollection<AvailabilityArgumentSyntax>;
def AvailabilityConditionSyntax : ConditionSyntax {
  Layout Available = Layout<PoundAvailableTokenSyntax>;
  Layout OpenParen = Layout<LeftParenTokenSyntax>;
  Layout AvailabilityArguments = Layout<AvailabilityArgumentList>;
  Layout CloseParen = Layout<RightParenTokenSyntax>; 
}
def BindingConditionSyntax : ConditionSyntax {
  // FIXME: whitelist of case,let
  Layout Keyword = Layout<AnyTokenSyntax>;
  Layout Pattern = Layout<AnyPatternSyntax>;
  Layout Initializer = Layout<AnyExprSyntax>;
}
def BooleanConditionSyntax : ConditionSyntax {
  Layout Expression = Layout<AnyExprSyntax>;
}

def ConditionListElementSyntax : Syntax {
  Layout Condition = Layout<AnyCondition>;
  Layout Comma = Layout<CommaTokenSyntax>;
}
def ConditionListSyntax : SyntaxCollection<ConditionListElementSyntax>;
def WhereClauseSyntax : Syntax {
  Layout Where = Layout<WhereTokenSyntax>;
  Layout WhereExpr = Layout<AnyExprSyntax>;
}
def ForInStmtSyntax : LabelledStmtSyntax {
  Layout For = Layout<ForTokenSyntax>;
  Layout Case = Layout<CaseTokenSyntax>;
  Layout ItemPattern = Layout<AnyPatternSyntax>;
  Layout In = Layout<InTokenSyntax>;
  Layout CollectionExpr = Layout<AnyExprSyntax>;
  Layout WhereClause = Layout<WhereClauseSyntax>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def WhileStmtSyntax : LabelledStmtSyntax {
  Layout While = Layout<WhileTokenSyntax>;
  Layout Conditions = Layout<ConditionListSyntax>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def RepeatWhileStmtSyntax : LabelledStmtSyntax {
  Layout Repeat = Layout<RepeatTokenSyntax>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout While = Layout<WhileTokenSyntax>;
  Layout Condition = Layout<AnyExprSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

class ElseClauseSyntax : Syntax {
  Layout Else = Layout<ElseTokenSyntax>;
}
def AnyElseClause : ElseClauseSyntax;

def ElseBlockSyntax : ElseClauseSyntax {
  Layout Body = Layout<CodeBlockSyntax>;
}

def ElseIfContinuationSyntax : ElseClauseSyntax {
  Layout IfStmtSyntax = Layout<ElseTokenSyntax>;
}

def IfStmtSyntax : LabelledStmtSyntax {
  Layout If = Layout<IfTokenSyntax>;
  Layout Conditions = Layout<ConditionListSyntax>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout ElseClause = Layout<AnyElseClause>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def CaseItemSyntax : Syntax {
  Layout Pattern = Layout<AnyPatternSyntax>;
  Layout WhereClause = Layout<WhereClauseSyntax>;
  Layout Comma = Layout<CommaTokenSyntax>;
}
def CaseItemListSyntax : SyntaxCollection<CaseItemSyntax>;

class SwitchLabelSyntax : Syntax;
def AnySwitchLabel : SwitchLabelSyntax;
def SwitchCaseLabelSyntax : SwitchLabelSyntax {
  Layout Case = Layout<CaseTokenSyntax>;
  Layout CaseItems = Layout<CaseItemListSyntax>;
  Layout Colon = Layout<ColonTokenSyntax>;
}
def SwitchDefaultLabelSyntax : SwitchLabelSyntax {
  Layout Default = Layout<DefaultTokenSyntax>;
  Layout Colon = Layout<ColonTokenSyntax>;
}

def SwitchCaseSyntax : Syntax {
  Layout Label = Layout<AnySwitchLabel>;
  Layout Body = Layout<StatementListSyntax>;
}

def SwitchCaseListSyntax : SyntaxCollection<SwitchCaseSyntax>;
def SwitchStmtSyntax : LabelledStmtSyntax {
  Layout Switch = Layout<SwitchTokenSyntax>;
  Layout Expression = Layout<AnyExprSyntax>;
  Layout OpenBrace = Layout<LeftBraceTokenSyntax>;
  Layout Cases = Layout<SwitchCaseListSyntax>;
  Layout CloseBrace = Layout<RightBraceTokenSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def CatchClauseSyntax : Syntax {
  Layout Catch = Layout<CatchTokenSyntax>;
  Layout Pattern = Layout<AnyPatternSyntax>;
  Layout WhereClause = Layout<WhereClauseSyntax>;
  Layout Body = Layout<CodeBlockSyntax>;
}
def CatchClauseListSyntax : SyntaxCollection<CatchClauseSyntax>;
def DoStmtSyntax : LabelledStmtSyntax {
  Layout Do = Layout<DoTokenSyntax>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout CatchClauses = Layout<CatchClauseListSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def GuardStmtSyntax : StmtSyntax {
  Layout Guard = Layout<GuardTokenSyntax>;
  Layout Conditions = Layout<ConditionListSyntax>;
  Layout Else = Layout<ElseTokenSyntax>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def BreakStmtSyntax : StmtSyntax {
  Layout Break = Layout<BreakTokenSyntax>;
  Layout Label = Layout<IdentifierTokenSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def ContinueStmtSyntax : StmtSyntax {
  Layout Continue = Layout<ContinueTokenSyntax>;
  Layout Label = Layout<IdentifierTokenSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def FallthroughStmtSyntax : StmtSyntax {
  Layout Fallthrough = Layout<FallthroughTokenSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def ReturnStmtSyntax : StmtSyntax {
  Layout Return = Layout<ReturnTokenSyntax>;
  Layout Expression = Layout<AnyExprSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def ThrowStmtSyntax : StmtSyntax {
  Layout Throw = Layout<ThrowTokenSyntax>;
  Layout Expression = Layout<AnyExprSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def DeferStmtSyntax : StmtSyntax {
  Layout Defer = Layout<DeferTokenSyntax>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout Semicolon = Layout<SemicolonTokenSyntax>;
}

def ElseDirectiveClause : Syntax {
  Layout PoundElse = Layout<PoundElseTokenSyntax>;
  Layout Body = Layout<StatementListSyntax>;
}
def ElseifDirectiveClause : Syntax {
  Layout PoundElseif = Layout<PoundElseifTokenSyntax>;
  Layout Condition = Layout<AnyExprSyntax>;
  Layout Body = Layout<StatementListSyntax>;
}
def ElseifDirectiveClauseListSyntax : SyntaxCollection<ElseifDirectiveClause>;
def ConditionalCompilationStmtSyntax : StmtSyntax {
  Layout PoundIf = Layout<PoundIfTokenSyntax>;
  Layout Condition = Layout<AnyExprSyntax>;
  Layout Body = Layout<StatementListSyntax>;
  Layout ElseifDirectiveClauses = Layout<ElseifDirectiveClauseListSyntax>;
  Layout ElseClause = Layout<ElseDirectiveClause>;
  Layout PoundEndif = Layout<PoundEndifTokenSyntax>;
}

def LineControlSpecSyntax : Syntax {
  Layout File = Layout<IdentifierTokenSyntax>; // FIXME: file
  Layout FileColon = Layout<ColonTokenSyntax>;
  Layout FileValue = Layout<StringLiteralTokenSyntax>;
  Layout Comma = Layout<CommaTokenSyntax>;
  Layout Line = Layout<IdentifierTokenSyntax>; // FIXME: line
  Layout LineColon = Layout<ColonTokenSyntax>;
  Layout LineValue = Layout<IntegerLiteralTokenSyntax>;
}
def LineControlStmtSyntax : StmtSyntax {
  Layout PoundSourceLocation = Layout<PoundSourceLocationTokenSyntax>;
  Layout OpenParen = Layout<LeftParenTokenSyntax>;
  Layout FileAndLine = Layout<LineControlSpecSyntax>;
  Layout CloseParen = Layout<RightParenTokenSyntax>;
}
