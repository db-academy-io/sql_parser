use super::{Identifier, SelectStatement, Statement};

/// An AST for [WITH](https://www.sqlite.org/lang_with.html) SQL statement.
/// The WITH statement is used in SELECT, INSERT, UPDATE, and DELETE
/// statements to define Common Table Expressions (CTEs).
#[derive(Debug, PartialEq)]
pub struct WithCteStatement {
    pub recursive: bool,

    pub cte_expressions: Vec<CteExpression>,

    pub statement: Box<Statement>,
}

/// An AST for a single CTE expression.
#[derive(Debug, PartialEq)]
pub struct CteExpression {
    pub name: Identifier,

    pub column_names: Vec<Identifier>,

    pub materialized: Option<MaterializationType>,

    pub select: SelectStatement,
}

#[derive(Debug, PartialEq)]
pub enum MaterializationType {
    Materialized,
    NotMaterialized,
}
