use super::{
    ConflictClause, Expression, Identifier, Ordering, QualifiedTableName, ReturningClause,
    SelectStatement, SetClause,
};

/// An AST for [INSERT](https://www.sqlite.org/lang_insert.html) SQL statement.
#[derive(Debug, PartialEq, Clone)]
pub struct InsertStatement {
    pub conflict_clause: ConflictClause,

    /// TODO: Change the type, as QualifiedTableName contains indexing type
    /// which is not applicable for insert statements
    pub table_name: QualifiedTableName,

    pub columns: Vec<Identifier>,

    pub values: InsertValues,

    pub upsert_clause: Option<Vec<UpsertClause>>,

    pub returning_clause: Vec<ReturningClause>,
}

/// The values to insert into the table.
#[derive(Debug, PartialEq, Clone)]
pub enum InsertValues {
    /// The values to insert into the table.
    Values(Vec<Vec<Expression>>),

    /// The values to insert into the table from a select statement.
    Select(SelectStatement),

    /// The values to insert into the table from a default value.
    DefaultValues,
}

/// The upsert clause to update the table with the new values.
#[derive(Debug, PartialEq, Clone)]
pub struct UpsertClause {
    pub conflict_target: Option<UpsertConflictTarget>,
    pub action: UpsertAction,
}

/// The [conflict target](https://www.sqlite.org/lang_upsert.html#upsert_conflict_target)
/// to update the table with the new values.
#[derive(Debug, PartialEq, Clone)]
pub struct UpsertConflictTarget {
    pub columns: Vec<IndexedColumn>,
    pub where_clause: Option<Box<Expression>>,
}

/// An indexed column
#[derive(Debug, PartialEq, Clone)]
pub struct IndexedColumn {
    pub column: Expression,

    pub ordering: Option<Ordering>,
}

/// The [action](https://www.sqlite.org/lang_upsert.html#upsert_action) to take when a conflict occurs.
#[derive(Debug, PartialEq, Clone)]
pub enum UpsertAction {
    /// The action to take when a conflict occurs.
    Nothing,

    /// The action to take when a conflict occurs.
    Update(UpsertUpdate),
}

/// The [update set](https://www.sqlite.org/lang_upsert.html) action to take when a conflict occurs.
#[derive(Debug, PartialEq, Clone)]
pub struct UpsertUpdate {
    pub set_clauses: Vec<SetClause>,
    pub where_clause: Option<Box<Expression>>,
}
