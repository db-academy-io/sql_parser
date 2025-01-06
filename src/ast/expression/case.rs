use super::Expression;

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

/// A when expression
#[derive(Debug, PartialEq, Clone)]
pub struct WhenExpression {
    /// The condition (next after WHEN keyword)
    pub condition: Box<Expression>,

    /// The result (next after THEN keyword)
    pub then_expression: Box<Expression>,
}
