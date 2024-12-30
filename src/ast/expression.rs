use std::fmt::Display;

use crate::{errors::ParsingError, Keyword, TokenType};

use super::{OrderingTerm, SelectStatement};

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
    Function(Function),

    /// An expression list (e.g. (1, 2, 3))
    ExpressionList(Vec<Expression>),

    /// A cast expression (e.g. CAST(expression AS type))
    Cast(Box<Expression>, DataType),

    /// A collate expression (e.g. expression COLLATE collation_name)
    CollateExpression(CollateExpressionStatement),

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

/// A literal value
#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    /// A number
    Number(String),

    /// A string
    String(String),

    /// A blob
    Blob(String),

    /// A boolean
    Boolean(bool),

    /// A null value
    Null,

    /// True
    True,

    /// False
    False,

    /// Current time
    CurrentTime,

    /// Current date
    CurrentDate,

    /// Current timestamp
    CurrentTimestamp,
}

/// An identifier
#[derive(Debug, PartialEq, Clone)]
pub enum Identifier {
    /// A single identifier
    Single(String),

    /// A compound identifier
    Compound(Vec<String>),

    /// A wildcard
    Wildcard,

    /// A table or column name with wildcard
    NameWithWildcard(String),
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Self {
        if s == "*" {
            return Identifier::Wildcard;
        }
        Identifier::Single(s.to_string())
    }
}

/// A binary operation
#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    /// Addition (+)
    Plus,
    /// Subtraction (-)
    Minus,
    /// Multiplication (*)
    Mul,
    /// Division (/)
    Div,
    /// Modulo (%)
    Remainder,
    /// And (AND)
    And,
    /// Or (OR)
    Or,
    /// Greater than (>)
    GreaterThan,
    /// Greater than or equal (>=)
    GreaterThanOrEquals,
    /// Less than (<)
    LessThan,
    /// Less than or equal (<=)
    LessThanOrEquals,
    /// Equals (=)
    Equals,
    /// Equals equals (==)
    EqualsEquals,
    /// Not equals (!=)
    NotEquals,
    /// Concatenation (||)
    Concat,
    /// Bitwise AND (&)
    BitAnd,
    /// Bitwise OR (|)
    BitOr,
    /// Left shift (<<)
    LeftShift,
    /// Right shift (>>)
    RightShift,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::And => write!(f, "AND"),
            BinaryOp::Or => write!(f, "OR"),
            BinaryOp::Remainder => write!(f, "%"),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::GreaterThanOrEquals => write!(f, ">="),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::LessThanOrEquals => write!(f, "<="),
            BinaryOp::Equals => write!(f, "="),
            BinaryOp::EqualsEquals => write!(f, "=="),
            BinaryOp::NotEquals => write!(f, "!="),
            BinaryOp::Concat => write!(f, "||"),
            BinaryOp::BitAnd => write!(f, "&"),
            BinaryOp::BitOr => write!(f, "|"),
            BinaryOp::LeftShift => write!(f, "<<"),
            BinaryOp::RightShift => write!(f, ">>"),
        }
    }
}

impl<'a> TryFrom<&'a TokenType<'a>> for BinaryOp {
    type Error = ParsingError;

    fn try_from(token_type: &'a TokenType<'a>) -> Result<Self, Self::Error> {
        match token_type {
            TokenType::Plus => Ok(BinaryOp::Plus),
            TokenType::Minus => Ok(BinaryOp::Minus),
            TokenType::Star => Ok(BinaryOp::Mul),
            TokenType::Slash => Ok(BinaryOp::Div),
            TokenType::Remainder => Ok(BinaryOp::Remainder),
            TokenType::Keyword(Keyword::And) => Ok(BinaryOp::And),
            TokenType::Keyword(Keyword::Or) => Ok(BinaryOp::Or),
            TokenType::GreaterThan => Ok(BinaryOp::GreaterThan),
            TokenType::GreaterEquals => Ok(BinaryOp::GreaterThanOrEquals),
            TokenType::LessThan => Ok(BinaryOp::LessThan),
            TokenType::LessEquals => Ok(BinaryOp::LessThanOrEquals),
            TokenType::Equals => Ok(BinaryOp::Equals),
            TokenType::EqualsEquals => Ok(BinaryOp::EqualsEquals),
            TokenType::NotEquals => Ok(BinaryOp::NotEquals),
            TokenType::Concat => Ok(BinaryOp::Concat),
            TokenType::BitAnd => Ok(BinaryOp::BitAnd),
            TokenType::BitOr => Ok(BinaryOp::BitOr),
            TokenType::LeftShift => Ok(BinaryOp::LeftShift),
            TokenType::RightShift => Ok(BinaryOp::RightShift),
            _ => Err(ParsingError::UnexpectedTokenType(format!(
                "Unexpected token type: {}",
                token_type
            ))),
        }
    }
}

/// A unary operation
#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    /// Plus (+)
    Plus,

    /// Minus (-)
    Minus,

    /// Not (!) or NOT
    Not,
}

/// An SQL function
#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    /// The name of the function
    pub name: Identifier,
    /// The arguments of the function
    pub arg: FunctionArg,
    /// The filter clause of the function
    pub filter_clause: Option<Box<Expression>>,
    /// The over clause of the function
    pub over_clause: Option<OverClause>,
}

/// A function argument
#[derive(Debug, PartialEq, Clone, Default)]
pub struct FunctionArg {
    /// The DISTINCT keyword and expressions
    pub distinct: bool,
    /// The arguments
    pub arguments: Vec<FunctionArgType>,
}

/// A function argument type
#[derive(Debug, PartialEq, Clone)]
pub enum FunctionArgType {
    /// An expression wrapper
    Expression(Expression),
    /// An expression with ordering terms
    OrderedBy(Expression, Vec<OrderingTerm>),
    /// A wildcard
    Wildcard,
}

/// An over clause
#[derive(Debug, PartialEq, Clone)]
pub enum OverClause {
    /// A window definition
    WindowDefinition(WindowDefinition),
    /// A window name
    WindowName(String),
}

/// A window definition
#[derive(Debug, PartialEq, Clone, Default)]
pub struct WindowDefinition {
    /// The window name
    pub base_window_name: Option<String>,
    /// The partition by clause
    pub partition_by: Option<Vec<Expression>>,
    /// The order by clause
    pub order_by: Option<Vec<OrderingTerm>>,
    /// The frame spec
    pub frame_spec: Option<FrameSpec>,
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

/// A frame spec
#[derive(Debug, PartialEq, Clone)]
pub struct FrameSpec {
    /// The frame type
    pub frame_type: FrameType,
    /// The frame spec type
    pub frame_spec_type: FrameSpecType,
    /// The exclude clause
    pub exclude: Option<FrameSpecExclude>,
}

/// A frame type
#[derive(Debug, PartialEq, Clone)]
pub enum FrameType {
    /// Rows frame type
    Rows,
    /// Range frame type
    Range,
    /// Groups frame type
    Groups,
}

/// A frame spec type
#[derive(Debug, PartialEq, Clone)]
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
#[derive(Debug, PartialEq, Clone)]
pub struct BetweenFrameSpec {
    /// The start frame spec type
    pub start: BetweenFrameSpecType,
    /// The end frame spec type
    pub end: BetweenFrameSpecType,
}

/// A between frame spec type
#[derive(Debug, PartialEq, Clone)]
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
#[derive(Debug, PartialEq, Clone)]
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
    In(InExpression),

    /// Between
    Between(BetweenExpression),
}

/// A like expression type
#[derive(Debug, PartialEq, Clone)]
pub enum LikeExpressionType {
    /// An expression
    Expression(Box<Expression>),

    /// An escape expression
    EscapeExpression(EscapeExpression),
}

/// An escape expression
#[derive(Debug, PartialEq, Clone)]
pub struct EscapeExpression {
    /// The expression
    pub expression: Box<Expression>,
    /// The escape expression
    pub escape_expression: Option<Box<Expression>>,
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
pub struct CollateExpressionStatement {
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
