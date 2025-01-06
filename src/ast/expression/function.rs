use crate::OrderingTerm;

use super::{Expression, Identifier, WindowDefinition};

/// An SQL function
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpression {
    /// The name of the function
    pub name: Identifier,
    /// The arguments of the function
    pub arg: FunctionArg,
    /// The filter clause of the function
    pub filter_clause: Option<Box<Expression>>,
    /// The over clause of the function
    pub over_clause: Option<WindowDefinition>,
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
