use super::SelectStatement;

/// An SQL expression
/// See details [sqlite-expression]
///
/// [sqlite-expression]: https://www.sqlite.org/lang_expr.html
#[derive(Debug, PartialEq)]
pub enum Expression {
    /// A single identifier
    Identifier(String),

    /// A compound identifier (e.g. "schema_name.table_name.column_name")
    CompoundIdentifier(Vec<String>),

    /// A function call
    Function(Function),

    /// A binary operation
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),

    /// A unary operation
    UnaryOp(UnaryOp, Box<Expression>),

    /// An expression list (e.g. (1, 2, 3))
    ExpressionList(Vec<Expression>),

    /// A cast expression (e.g. CAST(expression AS type))
    Cast(Box<Expression>, TypeName),

    /// A collate expression (e.g. expression COLLATE collation_name)
    CollateExpression(Box<Expression>, String),

    /// A unary matching expression (e.g. expression IS NOT NULL)
    UnaryMatchingExpression(UnaryMatchingExpressionType, Box<Expression>),

    /// A binary matching expression (e.g. expression LIKE pattern)
    BinaryMatchingExpression(Box<Expression>, BinaryMatchingExpression),

    /// An exists statement
    ExistsStatement(ExistsStatement),

    /// A case expression (e.g. CASE expression WHEN value1 THEN result1 WHEN value2 THEN result2 ELSE result3 END)
    CaseExpression(CaseExpression),

    /// A raise function
    RaiseFunction(RaiseFunction),
}

/// A binary operation
#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    /// Addition (+)
    Add,
    /// Subtraction (-)
    Sub,
    /// Multiplication (*)
    Mul,
    /// Division (/)
    Div,
}

/// A unary operation
#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    /// Negation (-)
    Neg,
}

/// An SQL function
#[derive(Debug, PartialEq)]
pub struct Function {
    /// The name of the function
    name: String,
    /// The arguments of the function
    args: Vec<FunctionArg>,
    /// The filter clause of the function
    filter_clause: Option<Box<Expression>>,
    /// The over clause of the function
    over_clause: Option<OverClause>,
}

/// A function argument
#[derive(Debug, PartialEq)]
pub enum FunctionArg {
    /// A distinct argument
    Distinct(Expression),

    /// An expression argument
    Expression(Vec<Expression>),

    /// An ordered by expression argument
    OrderedByExpression(Expression, Vec<OrderingTerm>),

    /// A wildcard argument
    Wildcard,

    /// No argument
    None,
}

/// An over clause
#[derive(Debug, PartialEq)]
pub struct OverClause {
    /// The window name
    window_name: Option<String>,
    /// The partition by clause
    partition_by: Option<Vec<Expression>>,
    /// The order by clause
    order_by: Option<Vec<OrderingTerm>>,
    /// The frame spec
    frame_spec: Option<FrameSpec>,
}

/// An ordering term
#[derive(Debug, PartialEq)]
pub struct OrderingTerm {
    /// The expression to order by
    expression: Box<Expression>,
    /// The collation name
    collation_name: Option<String>,
    /// The ordering
    ordering: Option<Ordering>,
    /// The nulls ordering
    nulls_ordering: Option<NullsOrdering>,
}

/// An ordering
#[derive(Debug, PartialEq)]
pub enum Ordering {
    /// Ascending
    Asc,
    /// Descending
    Desc,
}

/// Nulls ordering
#[derive(Debug, PartialEq)]
pub enum NullsOrdering {
    /// Nulls first
    First,
    /// Nulls last
    Last,
}

/// A frame spec
#[derive(Debug, PartialEq)]
pub struct FrameSpec {
    /// The frame type
    frame_type: FrameType,
    /// The frame spec type
    frame_spec_type: FrameSpecType,
    /// The exclude clause
    exclude: Option<FrameSpecExclude>,
}

/// A frame type
#[derive(Debug, PartialEq)]
pub enum FrameType {
    /// Rows frame type
    Rows,
    /// Range frame type
    Range,
    /// Groups frame type
    Groups,
}

/// A frame spec type
#[derive(Debug, PartialEq)]
pub enum FrameSpecType {
    /// A between frame spec
    Between(BetweenFrameSpec),
    /// Unbounded preceding
    UnboundedPreceding,
    /// Preceding frame spec
    Preceding(Box<Expression>),
    /// Current row
    CurrentRow,
}

/// A between frame spec
#[derive(Debug, PartialEq)]
pub struct BetweenFrameSpec {
    /// The start frame spec type
    start: BetweenFrameSpecType,
    /// The end frame spec type
    end: BetweenFrameSpecType,
}

/// A between frame spec type
#[derive(Debug, PartialEq)]
pub enum BetweenFrameSpecType {
    /// Unbounded preceding
    UnboundedPreceding,
    /// Preceding frame spec
    Preceding(Box<Expression>),
    /// Current row
    CurrentRow,
    /// Following frame spec
    Following(Box<Expression>),
    /// Unbounded following
    UnboundedFollowing,
}

/// A frame spec exclude
#[derive(Debug, PartialEq)]
pub enum FrameSpecExclude {
    /// No others
    NoOthers,
    /// Current row
    CurrentRow,
    /// Group
    Group,
    /// Ties
    Ties,
}

/// A type name
#[derive(Debug, PartialEq)]
pub enum TypeName {
    /// A data type name
    DataType(String),
}

/// A unary matching expression type
#[derive(Debug, PartialEq)]
pub enum UnaryMatchingExpressionType {
    /// Is Null
    IsNull,
    /// Is Not Null
    IsNotNull,
}

/// A binary matching expression
#[derive(Debug, PartialEq)]
pub enum BinaryMatchingExpression {
    /// Not BinaryMatchingExpression, i.e. NOT $expression
    Not(Box<BinaryMatchingExpression>),

    /// Like
    Like(LikeExpressionType),

    /// Glob
    Glob(Box<Expression>),

    /// Regexp
    Regexp(Box<Expression>),

    /// Match
    Match(Box<Expression>),

    /// Is
    Is(AnIsExpression),

    /// In
    In(InExpressionType),

    /// Between
    Between(BetweenExpression),
}

/// A like expression type
#[derive(Debug, PartialEq)]
pub enum LikeExpressionType {
    /// An expression
    Expression(Box<Expression>),

    /// An escape expression
    EscapeExpression(EscapeExpression),
}

/// An escape expression
#[derive(Debug, PartialEq)]
pub struct EscapeExpression {
    /// The expression
    expression: Box<Expression>,
    /// The escape expression
    escape_expression: Option<Box<Expression>>,
}

/// An IS expression
#[derive(Debug, PartialEq)]
pub struct AnIsExpression {
    /// The expression
    pub expression: Box<Expression>,
    /// Whether the expression is distinct
    pub distinct: bool,
}

/// A between expression
#[derive(Debug, PartialEq)]
pub struct BetweenExpression {
    /// The lower bound
    pub lower_bound: Box<Expression>,
    /// The upper bound
    pub upper_bound: Box<Expression>,
}

/// An in expression
#[derive(Debug, PartialEq)]
pub enum InExpressionType {
    /// Empty
    Empty,

    /// Select
    Select(SelectStatement),

    /// Expressions
    Expression(Vec<Box<Expression>>),

    /// Identity, i.e. schema_name.table_name.*
    Identity(Vec<String>),

    /// Table function
    TableFunction(Vec<Box<Expression>>),
}

/// An exists statement type
#[derive(Debug, PartialEq)]
pub enum ExistsStatement {
    /// Exists
    Exists(SelectStatement),

    /// Not Exists
    NotExists(SelectStatement),
}

/// A case expression
#[derive(Debug, PartialEq)]
pub struct CaseExpression {
    /// The expression
    pub expression: Box<Option<Expression>>,

    /// The when expressions
    pub when_expressions: Vec<WhenExpression>,

    /// The else expression
    pub else_expression: Box<Option<Expression>>,
}

/// A when expression
#[derive(Debug, PartialEq)]
pub struct WhenExpression {
    /// The condition (next after WHEN keyword)
    pub condition: Box<Expression>,
    /// The result (next after THEN keyword)
    pub result: Box<Expression>,
}

/// A raise function
#[derive(Debug, PartialEq)]
pub enum RaiseFunction {
    /// Ignore
    Ignore,

    /// Rollback with error message
    Rollback(String),

    /// Abort with error message
    Abort(String),

    /// Fail with error message
    Fail(String),
}
