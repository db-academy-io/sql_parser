use super::{Expression, QualifiedTableName};

/// An AST for [DELETE](https://www.sqlite.org/lang_delete.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct DeleteStatement {
    pub table_name: QualifiedTableName,

    pub where_clause: Option<Box<Expression>>,

    pub returning_clause: Vec<ReturningClause>,
}

/// A [RETURNING](https://www.sqlite.org/lang_returning.html) clause types
#[derive(Debug, PartialEq)]
pub enum ReturningClause {
    /// A single expression
    Expr(Expression),
    /// An expression with an alias
    ExprWithAlias(Expression, String),
    /// A wildcard
    Wildcard,
}
