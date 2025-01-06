mod between;
mod case;
mod frame_spec;
mod function;
mod identifier;
mod in_expression;
mod literal_value;
mod matching;
mod operations;
mod window_definition;

use super::{DataType, SelectStatement};
pub use between::BetweenExpression;
pub use case::{CaseExpression, WhenExpression};
pub use frame_spec::{
    BetweenFrameSpec, BetweenFrameSpecType, FrameSpec, FrameSpecExclude, FrameSpecType, FrameType,
};
pub use function::{FunctionArg, FunctionArgType, FunctionExpression};
pub use identifier::Identifier;
pub use in_expression::{InExpression, InExpressionType};
pub use literal_value::LiteralValue;
pub use matching::{GlobExpression, LikeExpression, MatchExpression, RegexpExpression};
pub use operations::{BinaryOp, UnaryOp};
pub use window_definition::WindowDefinition;

/// An SQLite3 [expr](https://www.sqlite.org/lang_expr.html) expression
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    /// A literal value, i.e. 123
    LiteralValue(LiteralValue),

    /// A bind parameter, i.e. :?
    BindParameter(String),

    /// A single identifier, i.e. table_name.column_name
    Identifier(Identifier),

    /// A unary operation, i.e. -$expr
    UnaryOp(UnaryOp, Box<Expression>),

    /// A binary operation, i.e. $expr1 + $expr2
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),

    /// A function call, i.e. $func_name($expr1, $expr2, $expr3)
    Function(FunctionExpression),

    /// An expression list, i.e. ($expr1, $expr2, $expr3)
    ExpressionList(Vec<Expression>),

    /// A cast expression, i.e. CAST($expr AS $type)
    Cast(Box<Expression>, DataType),

    /// A collate expression, i.e. $expr COLLATE $collation_name
    CollateExpression(CollateExpression),

    /// Like, i.e. $expr [NOT] LIKE $expr [ESCAPE $expr]
    LikeExpression(LikeExpression),

    /// Glob, i.e. $expr [NOT] GLOB $expr
    GlobExpression(GlobExpression),

    /// Regexp, i.e. $expr [NOT] REGEXP $expr
    RegexpExpression(RegexpExpression),

    /// Match, i.e. $expr [NOT] MATCH $expr
    MatchExpression(MatchExpression),

    /// Is, i.e. $expr IS [NOT] [DISTINCT FROM] $expr
    IsExpression(IsExpression),

    /// In, i.e. $expr [NOT] IN $expr
    InExpression(InExpression),

    /// Is Null, i.e. $expr IS NULL
    IsNull(Box<Expression>),

    /// Is Not Null, i.e. $expr IS NOT NULL
    IsNotNull(Box<Expression>),

    /// Between, i.e. $expr [NOT] BETWEEN $lower_bound AND $upper_bound
    BetweenExpression(BetweenExpression),

    /// An exists statement, i.e. [NOT] EXISTS (select statement)
    ExistsStatement(ExistsStatement),

    /// A case expression (e.g. CASE $expr WHEN $value1 THEN $result1 ELSE $result3 END)
    CaseExpression(CaseExpression),

    /// A raise function, i.e. RAISE(error_name, error_message)
    RaiseFunction(RaiseFunction),
}

/// An IS expression, i.e. $expr IS [NOT] [DISTINCT FROM] $expr
#[derive(Debug, PartialEq, Clone)]
pub struct IsExpression {
    /// The expression
    pub expression: Box<Expression>,

    /// Whether the expression is not
    pub not: bool,

    /// Whether the expression is distinct
    pub distinct: bool,

    /// The matching expression
    pub matching_expression: Box<Expression>,
}

impl From<IsExpression> for Expression {
    fn from(is_expr: IsExpression) -> Self {
        Expression::IsExpression(is_expr)
    }
}

/// An exists statement, i.e. $expr EXISTS (select statement)
#[derive(Debug, PartialEq, Clone)]
pub struct ExistsStatement {
    pub select_statement: SelectStatement,

    pub not: bool,
}

/// A collate expression
#[derive(Debug, PartialEq, Clone)]
pub struct CollateExpression {
    /// The expression
    pub expression: Box<Expression>,

    /// The collation name
    pub collation_name: String,
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
