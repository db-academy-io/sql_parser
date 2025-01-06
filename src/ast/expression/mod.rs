mod frame_spec;
mod function;
mod identifier;
mod literal_value;
mod operations;
mod over_clause;

use super::SelectStatement;
pub use frame_spec::{
    BetweenFrameSpec, BetweenFrameSpecType, FrameSpec, FrameSpecExclude, FrameSpecType, FrameType,
};
pub use function::{FunctionArg, FunctionArgType, FunctionExpression};
pub use identifier::Identifier;
pub use literal_value::LiteralValue;
pub use operations::{BinaryOp, UnaryOp};
pub use over_clause::{OverClause, WindowDefinition};

/// An SQLite3 [expr](https://www.sqlite.org/lang_expr.html) expression
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    /// A literal value
    LiteralValue(LiteralValue),

    /// A bind parameter
    BindParameter(String),

    /// A single identifier
    Identifier(Identifier),

    /// A unary operation
    UnaryOp(UnaryOp, Box<Expression>),

    /// A binary operation
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),

    /// A function call
    Function(FunctionExpression),

    /// An expression list (e.g. (1, 2, 3))
    ExpressionList(Vec<Expression>),

    /// A cast expression (e.g. CAST(expression AS type))
    Cast(Box<Expression>, DataType),

    /// A collate expression (e.g. $expression COLLATE collation_name)
    CollateExpression(CollateExpression),

    // /// For NOT $BinaryMatchingExpression use cases
    // Not(Box<BinaryMatchingExpression>),
    /// Like
    LikeExpression(LikeExpressionType),

    /// Glob
    GlobExpression(GlobExpression),

    /// Regexp
    RegexpExpression(RegexpMatchingExpression),

    /// Match
    MatchExpression(MatchExpression),

    /// Is
    IsExpression(AnIsExpression),

    /// In
    InExpression(InExpression),

    /// Between
    BetweenExpression(BetweenExpression),

    /// A binary matching expressions (e.g. $expr1 NOT MATCH $expr2)
    BinaryMatchingExpression(Box<Expression>, BinaryMatchingExpression),

    /// A unary matching expression (e.g. expression IS NOT NULL)
    UnaryMatchingExpression(Box<Expression>, UnaryMatchingExpression),

    /// An exists statement
    ExistsStatement(ExistsStatement),

    /// A case expression (e.g. CASE expression WHEN value1 THEN result1 WHEN value2 THEN result2 ELSE result3 END)
    CaseExpression(CaseExpression),

    /// A raise function
    RaiseFunction(RaiseFunction),
}

/// A window definition
#[derive(Debug, PartialEq, Clone, Default)]
pub struct NamedWindowDefinition {
    /// The window name
    pub window_name: String,

    /// The window definition
    pub window_definition: WindowDefinition,
}

/// Nulls ordering
#[derive(Debug, PartialEq, Clone)]
pub enum NullsOrdering {
    /// Nulls first
    First,
    /// Nulls last
    Last,
}

/// A data type name
#[derive(Debug, PartialEq, Clone)]
pub enum DataTypeName {
    /// A single data type name, e.g. INTEGER
    Single(String),
    /// A compound data type name, e.g. DOUBLE PRECISION
    Compound(Vec<String>),
}

impl From<&str> for DataTypeName {
    fn from(s: &str) -> Self {
        if s.contains(" ") {
            DataTypeName::Compound(s.split(" ").map(|s| s.to_string()).collect())
        } else {
            DataTypeName::Single(s.to_string())
        }
    }
}

/// A data type enum, representing the [sqlite-data-types](https://www.sqlite.org/datatype3.html)
#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
    /// A data type name, e.g. INTEGER
    PlainDataType(DataTypeName),

    /// A sized data type, e.g. VARCHAR(10)
    SizedDataType(DataTypeName, String),

    /// A bounded data type name, e.g. VARCHAR(1, 10)
    BoundedDataType(DataTypeName, String, String),
}

/// An unary matching expression type
#[derive(Debug, PartialEq, Clone)]
pub enum UnaryMatchingExpression {
    /// Is Null
    IsNull,
    /// Is Not Null
    IsNotNull,
}

/// A binary matching expression
#[derive(Debug, PartialEq, Clone)]
pub enum BinaryMatchingExpression {
    /// For NOT $BinaryMatchingExpression use cases
    Not(Box<BinaryMatchingExpression>),

    /// Is
    Is(AnIsExpression),

    /// In
    In(InExpression),

    /// Between
    Between(BetweenExpression),
}

/// A like expression type
#[derive(Debug, PartialEq, Clone)]
pub struct LikeExpressionType {
    /// An expression
    pub expression: Box<Expression>,

    /// Whether the expression is not
    pub not: bool,

    /// The like expression
    pub like_expression: Box<Expression>,

    /// The escape expression
    pub escape_expression: Option<Box<Expression>>,
}

impl From<LikeExpressionType> for Expression {
    fn from(like_expr: LikeExpressionType) -> Self {
        Expression::LikeExpression(like_expr)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct GlobExpression {
    pub expression: Box<Expression>,

    pub not: bool,

    pub pattern: Box<Expression>,
}

impl From<GlobExpression> for Expression {
    fn from(glob_expr: GlobExpression) -> Self {
        Expression::GlobExpression(glob_expr)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct RegexpMatchingExpression {
    pub expression: Box<Expression>,

    pub not: bool,

    pub pattern: Box<Expression>,
}

impl From<RegexpMatchingExpression> for Expression {
    fn from(regexp_expr: RegexpMatchingExpression) -> Self {
        Expression::RegexpExpression(regexp_expr)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct MatchExpression {
    pub expression: Box<Expression>,

    pub not: bool,

    pub pattern: Box<Expression>,
}

impl From<MatchExpression> for Expression {
    fn from(match_expr: MatchExpression) -> Self {
        Expression::MatchExpression(match_expr)
    }
}

/// An IS expression
#[derive(Debug, PartialEq, Clone)]
pub struct AnIsExpression {
    /// The expression
    pub expression: Box<Expression>,
    /// Whether the expression is distinct
    pub distinct: bool,
}

/// A between expression
#[derive(Debug, PartialEq, Clone)]
pub struct BetweenExpression {
    /// The lower bound
    pub lower_bound: Box<Expression>,
    /// The upper bound
    pub upper_bound: Box<Expression>,
}

/// An in expression
#[derive(Debug, PartialEq, Clone)]
pub enum InExpression {
    /// Empty
    Empty,

    /// Select
    Select(SelectStatement),

    /// Expressions
    Expression(Vec<Expression>),

    /// Identity, i.e. schema_name.table_name
    Identity(Identifier),

    /// Table function
    TableFunction(Identifier, Vec<Expression>),
}

/// An exists statement type
#[derive(Debug, PartialEq, Clone)]
pub enum ExistsStatement {
    /// Exists
    Exists(SelectStatement),

    /// Not Exists
    NotExists(SelectStatement),
}

/// A case expression
#[derive(Debug, PartialEq, Clone)]
pub struct CaseExpression {
    /// The expression
    pub expression: Option<Box<Expression>>,

    /// The when expressions
    pub when_expressions: Vec<WhenExpression>,

    /// The else expression
    pub else_expression: Option<Box<Expression>>,
}

/// A collate expression
#[derive(Debug, PartialEq, Clone)]
pub struct CollateExpression {
    /// The expression
    pub expression: Box<Expression>,

    /// The collation name
    pub collation_name: String,
}

/// A when expression
#[derive(Debug, PartialEq, Clone)]
pub struct WhenExpression {
    /// The condition (next after WHEN keyword)
    pub condition: Box<Expression>,
    /// The result (next after THEN keyword)
    pub then_expression: Box<Expression>,
}

/// A raise function
#[derive(Debug, PartialEq, Clone)]
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
