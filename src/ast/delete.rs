use super::{Expression, LimitClause, OrderingTerm, QualifiedTableName};

/// An AST for [DELETE](https://www.sqlite.org/lang_delete.html) SQL statement.
#[derive(Debug, PartialEq, Clone)]
pub struct DeleteStatement {
    /// The table name to delete from
    pub table_name: QualifiedTableName,

    /// The WHERE clause
    pub where_clause: Option<Box<Expression>>,

    /// The RETURNING clause
    pub returning_clause: Vec<ReturningClause>,

    /// The ORDER BY clause
    pub order_by: Option<Vec<OrderingTerm>>,

    /// The LIMIT clause
    pub limit: Option<LimitClause>,
}

/// A [RETURNING](https://www.sqlite.org/lang_returning.html) clause types
#[derive(Debug, PartialEq, Clone)]
pub enum ReturningClause {
    /// A single expression
    Expr(Expression),
    /// An expression with an alias
    ExprWithAlias(Expression, String),
    /// A wildcard
    Wildcard,
}
