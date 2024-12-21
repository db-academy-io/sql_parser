use std::fmt::Display;

use super::{Identifier, SelectStatement};

/// An AST for [WITH](https://www.sqlite.org/lang_with.html) SQL statement.
/// The WITH statement is used in SELECT, INSERT, UPDATE, and DELETE
/// statements to define Common Table Expressions (CTEs).
#[derive(Debug, PartialEq, Clone)]
pub struct WithCteStatement {
    pub recursive: bool,

    pub cte_expressions: Vec<CteExpression>,
}

/// An AST for a single CTE expression.
#[derive(Debug, PartialEq, Clone)]
pub struct CteExpression {
    pub name: Identifier,

    pub column_names: Vec<Identifier>,

    pub materialized: Option<MaterializationType>,

    pub select: SelectStatement,
}

#[derive(Debug, PartialEq, Clone)]
pub enum MaterializationType {
    Materialized,
    NotMaterialized,
}

impl Display for MaterializationType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MaterializationType::Materialized => write!(f, "MATERIALIZED"),
            MaterializationType::NotMaterialized => write!(f, "NOT MATERIALIZED"),
        }
    }
}
