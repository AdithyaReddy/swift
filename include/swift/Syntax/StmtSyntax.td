def ExpressionStmtSyntax : Stmt {
  Layout Expression = Layout<AnyExpr>;
  Layout Semicolon = Layout<SemicolonToken>;
}
def DeclarationStmtSyntax : Stmt {
  Layout Declaration = Layout<AnyDecl>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def StatementListSyntax : SyntaxCollection<AnyStmt>;
def CodeBlockSyntax : Syntax {
  Layout OpenBrace = Layout<LeftBraceToken>;
  Layout Statments = Layout<StatementListSyntax>;
  Layout CloseBrace = Layout<RightBraceToken>;
}

class LabelledStmt : Stmt {
  Layout LabelName = Layout<IdentifierToken>;
  Layout LabelColon = Layout<ColonToken>;
}

class ConditionSyntax : Syntax;
def AnyCondition : ConditionSyntax;

def PlatformVersionSyntax : Syntax {
  Layout MajorVersion = Layout<IntegerLiteralToken>;
  Layout MinorPeriod = Layout<PeriodToken>;
  Layout MinorVersion = Layout<IntegerLiteralToken>;
  Layout BugfixPeriod = Layout<PeriodToken>;
  Layout BugfixVersion = Layout<IntegerLiteralToken>;
}
def AvailabilityArgumentSyntax : Syntax {
  // FIXME: whitelist?
  Layout PlatformName = Layout<IdentifierToken>;
  Layout PlatformVersion = Layout<PlatformVersionSyntax>;
  Layout Comma = Layout<CommaToken>;
}
def AvailabilityArgumentList : SyntaxCollection<AvailabilityArgumentSyntax>;
def AvailabilityConditionSyntax : ConditionSyntax {
  Layout Available = Layout<PoundAvailableToken>;
  Layout OpenParen = Layout<LeftParenToken>;
  Layout AvailabilityArguments = Layout<AvailabilityArgumentList>;
  Layout CloseParen = Layout<RightParenToken>; 
}
def BindingConditionSyntax : ConditionSyntax {
  // FIXME: whitelist of case,let
  Layout Keyword = Layout<AnyToken>;
  Layout Pattern = Layout<AnyPattern>;
  Layout Initializer = Layout<AnyExpr>;
}
def BooleanConditionSyntax : ConditionSyntax {
  Layout Expression = Layout<AnyExpr>;
}

def ConditionListElementSyntax : Syntax {
  Layout Condition = Layout<AnyCondition>;
  Layout Comma = Layout<CommaToken>;
}
def ConditionListSyntax : SyntaxCollection<ConditionListElementSyntax>;
def WhereClauseSyntax : Syntax {
  Layout Where = Layout<WhereToken>;
  Layout WhereExpr = Layout<AnyExpr>;
}
def ForInStmtSyntax : LabelledStmt {
  Layout For = Layout<ForToken>;
  Layout Case = Layout<CaseToken>;
  Layout ItemPattern = Layout<AnyPattern>;
  Layout In = Layout<InToken>;
  Layout CollectionExpr = Layout<AnyExpr>;
  Layout WhereClause = Layout<WhereClauseSyntax>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def WhileStmtSyntax : LabelledStmt {
  Layout While = Layout<WhileToken>;
  Layout Conditions = Layout<ConditionListSyntax>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def RepeatWhileStmtSyntax : LabelledStmt {
  Layout Repeat = Layout<RepeatToken>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout While = Layout<WhileToken>;
  Layout Condition = Layout<AnyExpr>;
  Layout Semicolon = Layout<SemicolonToken>;
}

class ElseClauseSyntax : Syntax {
  Layout Else = Layout<ElseToken>;
}
def AnyElseClause : ElseClauseSyntax;

def ElseBlockSyntax : ElseClauseSyntax {
  Layout Body = Layout<CodeBlockSyntax>;
}

def ElseIfContinuationSyntax : ElseClauseSyntax {
  Layout IfStmt = Layout<ElseToken>;
}

def IfStmtSyntax : LabelledStmt {
  Layout If = Layout<IfToken>;
  Layout Conditions = Layout<ConditionListSyntax>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout ElseClause = Layout<AnyElseClause>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def CaseItemSyntax : Syntax {
  Layout Pattern = Layout<AnyPattern>;
  Layout WhereClause = Layout<WhereClauseSyntax>;
  Layout Comma = Layout<CommaToken>;
}
def CaseItemListSyntax : SyntaxCollection<CaseItemSyntax>;

class SwitchLabelSyntax : Syntax;
def AnySwitchLabel : SwitchLabelSyntax;
def SwitchCaseLabelSyntax : SwitchLabelSyntax {
  Layout Case = Layout<CaseToken>;
  Layout CaseItems = Layout<CaseItemListSyntax>;
  Layout Colon = Layout<ColonToken>;
}
def SwitchDefaultLabelSyntax : SwitchLabelSyntax {
  Layout Default = Layout<DefaultToken>;
  Layout Colon = Layout<ColonToken>;
}

def SwitchCaseSyntax : Syntax {
  Layout Label = Layout<AnySwitchLabel>;
  Layout Body = Layout<StatementListSyntax>;
}

def SwitchCaseListSyntax : SyntaxCollection<SwitchCaseSyntax>;
def SwitchStmtSyntax : LabelledStmt {
  Layout Switch = Layout<SwitchToken>;
  Layout Expression = Layout<AnyExpr>;
  Layout OpenBrace = Layout<LeftBraceToken>;
  Layout Cases = Layout<SwitchCaseListSyntax>;
  Layout CloseBrace = Layout<RightBraceToken>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def CatchClauseSyntax : Syntax {
  Layout Catch = Layout<CatchToken>;
  Layout Pattern = Layout<AnyPattern>;
  Layout WhereClause = Layout<WhereClauseSyntax>;
  Layout Body = Layout<CodeBlockSyntax>;
}
def CatchClauseListSyntax : SyntaxCollection<CatchClauseSyntax>;
def DoStmtSyntax : LabelledStmt {
  Layout Do = Layout<DoToken>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout CatchClauses = Layout<CatchClauseListSyntax>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def GuardStmtSyntax : Stmt {
  Layout Guard = Layout<GuardToken>;
  Layout Conditions = Layout<ConditionListSyntax>;
  Layout Else = Layout<ElseToken>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def BreakStmtSyntax : Stmt {
  Layout Break = Layout<BreakToken>;
  Layout Label = Layout<IdentifierToken>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def ContinueStmtSyntax : Stmt {
  Layout Continue = Layout<ContinueToken>;
  Layout Label = Layout<IdentifierToken>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def FallthroughStmtSyntax : Stmt {
  Layout Fallthrough = Layout<FallthroughToken>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def ReturnStmtSyntax : Stmt {
  Layout Return = Layout<ReturnToken>;
  Layout Expression = Layout<AnyExpr>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def ThrowStmtSyntax : Stmt {
  Layout Throw = Layout<ThrowToken>;
  Layout Expression = Layout<AnyExpr>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def DeferStmtSyntax : Stmt {
  Layout Defer = Layout<DeferToken>;
  Layout Body = Layout<CodeBlockSyntax>;
  Layout Semicolon = Layout<SemicolonToken>;
}

def ElseDirectiveClause : Syntax {
  Layout PoundElse = Layout<PoundElseToken>;
  Layout Body = Layout<StatementListSyntax>;
}
def ElseifDirectiveClause : Syntax {
  Layout PoundElseif = Layout<PoundElseifToken>;
  Layout Condition = Layout<AnyExpr>;
  Layout Body = Layout<StatementListSyntax>;
}
def ElseifDirectiveClauseListSyntax : SyntaxCollection<ElseifDirectiveClause>;
def ConditionalCompilationStmtSyntax : Stmt {
  Layout PoundIf = Layout<PoundIfToken>;
  Layout Condition = Layout<AnyExpr>;
  Layout Body = Layout<StatementListSyntax>;
  Layout ElseifDirectiveClauses = Layout<ElseifDirectiveClauseListSyntax>;
  Layout ElseClause = Layout<ElseDirectiveClause>;
  Layout PoundEndif = Layout<PoundEndifToken>;
}

