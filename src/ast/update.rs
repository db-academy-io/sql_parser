use super::{
    ConflictClause, Expression, FromClause, Identifier, QualifiedTableName, ReturningClause,
};

/// An AST for [UPDATE SQL](https://www.sqlite.org/lang_update.html) statement.
#[derive(Debug, PartialEq)]
pub struct UpdateStatement {
    // The conflict clause defined after the OR keyword is used to specify the
    // behavior of the UPDATE statement when a constraint violation occurs.
    pub conflict_clause: Option<ConflictClause>,

    // The table name to update.
    pub table_name: QualifiedTableName,

    // The set clause to update the table with.
    pub set_clause: Vec<SetClause>,

    // The from clause to select the rows to update.
    pub from_clause: Option<FromClause>,

    // The where clause to filter the rows to update.
    pub where_clause: Option<Expression>,

    // The returning clause to return the updated rows.
    pub returning_clause: Option<ReturningClause>,
}

/// A [SetClause](https://www.sqlite.org/syntax/set-clause.html)
#[derive(Debug, PartialEq)]
pub enum SetClause {
    /// A single column assignment, e.g. `column = expression`.
    ColumnAssignment(Identifier, Expression),

    /// A multiple column assignment, e.g. `(column1, column2) = expression`.
    MultipleColumnAssignment((Vec<Identifier>, Expression)),
}
