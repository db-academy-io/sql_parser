use super::Expression;

/// A between expression, i.e. $expr [NOT] BETWEEN $lower_bound AND $upper_bound
#[derive(Debug, PartialEq, Clone)]
pub struct BetweenExpression {
    /// The expression
    pub expression: Box<Expression>,

    /// Whether the expression is not
    pub not: bool,

    /// The lower bound
    pub lower_bound: Box<Expression>,

    /// The upper bound
    pub upper_bound: Box<Expression>,
}

impl From<BetweenExpression> for Expression {
    fn from(between_expr: BetweenExpression) -> Self {
        Expression::BetweenExpression(between_expr)
    }
}
