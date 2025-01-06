use super::Expression;

/// A like expression type
#[derive(Debug, PartialEq, Clone)]
pub struct LikeExpression {
    /// An expression
    pub expression: Box<Expression>,

    /// Whether the expression is not
    pub not: bool,

    /// The like expression
    pub like_expression: Box<Expression>,

    /// The escape expression
    pub escape_expression: Option<Box<Expression>>,
}

impl From<LikeExpression> for Expression {
    fn from(like_expr: LikeExpression) -> Self {
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
pub struct RegexpExpression {
    pub expression: Box<Expression>,

    pub not: bool,

    pub pattern: Box<Expression>,
}

impl From<RegexpExpression> for Expression {
    fn from(regexp_expr: RegexpExpression) -> Self {
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
