use crate::SelectStatement;

use super::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct InExpression {
    pub expression: Box<Expression>,

    pub in_expression: InExpressionType,

    pub is_not: bool,
}

impl From<InExpression> for Expression {
    fn from(in_expr: InExpression) -> Self {
        Expression::InExpression(in_expr)
    }
}

/// An in expression
#[derive(Debug, PartialEq, Clone)]
pub enum InExpressionType {
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
