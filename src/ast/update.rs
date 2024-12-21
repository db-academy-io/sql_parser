use super::{
    ConflictClause, Expression, FromClause, Identifier, LimitClause, OrderingTerm,
    QualifiedTableName, ReturningClause, WithCteStatement,
};

/// An AST for [UPDATE SQL](https://www.sqlite.org/lang_update.html) statement.
#[derive(Debug, PartialEq, Clone)]
pub struct UpdateStatement {
    pub with_cte: Option<WithCteStatement>,

    // The conflict clause defined after the OR keyword is used to specify the
    // behavior of the UPDATE statement when a constraint violation occurs.
    pub conflict_clause: ConflictClause,

    // The table name to update.
    pub table_name: QualifiedTableName,

    // The set clause to update the table with.
    pub set_clause: Vec<SetClause>,

    // The from clause to select the rows to update.
    pub from_clause: Option<FromClause>,

    // The where clause to filter the rows to update.
    pub where_clause: Option<Box<Expression>>,

    // The returning clause to return the updated rows.
    pub returning_clause: Vec<ReturningClause>,

    // The order by clause to sort the rows to update.
    pub order_by: Option<Vec<OrderingTerm>>,

    // The limit clause to limit the number of rows to update.
    pub limit: Option<LimitClause>,
}

/// A [SetClause](https://www.sqlite.org/syntax/set-clause.html)
#[derive(Debug, PartialEq, Clone)]
pub enum SetClause {
    /// A single column assignment, e.g. `column = expression`.
    ColumnAssignment(Identifier, Expression),

    /// A multiple column assignment, e.g. `(column1, column2) = expression`.
    MultipleColumnAssignment(Vec<Identifier>, Expression),
}
