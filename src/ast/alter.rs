use std::fmt::Display;

use super::{DataType, Expression, Identifier, Ordering};

/// An AST for [ALTER TABLE](https://www.sqlite.org/lang_altertable.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct AlterTableStatement {
    pub table_name: Identifier,

    pub statement_type: AlterTableStatementType,
}

/// An ALTER TABLE statement type
#[derive(Debug, PartialEq)]
pub enum AlterTableStatementType {
    /// Rename a table statement
    RenameTable(Identifier),

    /// Rename a column statement
    RenameColumn(Identifier, Identifier),

    /// Add a column statement
    AddColumn(ColumnDefinition),

    /// Drop a column statement
    DropColumn(Identifier),
}

/// A ColumnDefinition, used in ALTER TABLE ADD COLUMN statement
#[derive(Debug, PartialEq, Clone)]
pub struct ColumnDefinition {
    pub column_name: Identifier,

    pub column_type: Option<DataType>,

    pub column_constraints: Vec<ColumnConstraint>,
}

/// A ColumnConstraint, used in ALTER TABLE ADD COLUMN statement
#[derive(Debug, PartialEq, Clone)]
pub struct ColumnConstraint {
    /// An optional name for the constraint
    pub name: Option<Identifier>,

    /// A constraint type
    pub constraint_type: ColumnConstraintType,
}

/// A constraint type
#[derive(Debug, PartialEq, Clone)]
pub enum ColumnConstraintType {
    PrimaryKey(PrimaryKeyConstraint),
    NotNull(ConflictClause),
    Unique(ConflictClause),
    Check(Expression),
    Default(Expression),
    Collate(Identifier),
    ForeignKey(ForeignKeyClause),
    GeneratedAs(GeneratedColumnConstraint),
}

/// A PrimaryKeyConstraint, used in ALTER TABLE ADD COLUMN statement
#[derive(Debug, PartialEq, Clone)]
pub struct PrimaryKeyConstraint {
    pub ordering: Option<Ordering>,
    pub conflict_clause: ConflictClause,
    pub auto_increment: bool,
}

/// A [ConflictClause](https://www.sqlite.org/lang_conflict.html), used in
/// ALTER TABLE statement.
///
/// Do not get confused with the conflict clause in the
/// [RaiseFunction](https://www.sqlite.org/lang_raise.html) as they look similar
/// but they are different.
#[derive(Debug, PartialEq, Clone)]
pub enum ConflictClause {
    /// No conflict clause
    None,

    /// When an applicable constraint violation occurs, the ROLLBACK resolution
    /// algorithm aborts the current SQL statement with an SQLITE_CONSTRAINT
    /// error and rolls back the current transaction.
    ///
    /// If no transaction is active (other than the implied transaction that is
    /// created on every command) then the ROLLBACK resolution algorithm works
    /// the same as the ABORT algorithm.
    Rollback,

    /// When an applicable constraint violation occurs, the ABORT resolution
    /// algorithm aborts the current SQL statement with an SQLITE_CONSTRAINT
    /// error and backs out any changes made by the current SQL statement; but
    /// changes caused by prior SQL statements within the same transaction are
    /// preserved and the transaction remains active. This is the default
    /// behavior and the behavior specified by the SQL standard.
    Abort,

    /// When an applicable constraint violation occurs, the FAIL resolution
    /// algorithm aborts the current SQL statement with an SQLITE_CONSTRAINT
    /// error. But the FAIL resolution does not back out prior changes of the
    /// SQL statement that failed nor does it end the transaction.
    ///
    /// For example, if an UPDATE statement encountered a constraint violation
    /// on the 100th row that it attempts to update, then the first 99 row
    /// changes are preserved but changes to rows 100 and beyond never occur.
    ///
    /// The FAIL behavior only works for uniqueness, NOT NULL, and CHECK
    /// constraints. A foreign key constraint violation causes an ABORT.
    Fail,

    /// When an applicable constraint violation occurs, the IGNORE resolution
    /// algorithm skips the one row that contains the constraint violation and
    /// continues processing subsequent rows of the SQL statement as if nothing
    /// went wrong. Other rows before and after the row that contained the
    /// constraint violation are inserted or updated normally.
    ///
    /// No error is returned for uniqueness, NOT NULL, and UNIQUE constraint
    /// errors when the IGNORE conflict resolution algorithm is used. However,
    /// the IGNORE conflict resolution algorithm works like ABORT for foreign
    /// key constraint errors.
    Ignore,

    /// When a UNIQUE or PRIMARY KEY constraint violation occurs, the REPLACE
    /// algorithm deletes pre-existing rows that are causing the constraint
    /// violation prior to inserting or updating the current row and the
    /// command continues executing normally. If a NOT NULL constraint
    /// violation occurs, the REPLACE conflict resolution replaces the NULL
    /// value with the default value for that column, or if the column has no
    /// default value, then the ABORT algorithm is used. If a CHECK constraint
    /// or foreign key constraint violation occurs, the REPLACE conflict
    /// resolution algorithm works like ABORT.
    ///
    /// When the REPLACE conflict resolution strategy deletes rows in order to
    /// satisfy a constraint, delete triggers fire if and only if recursive
    /// triggers are enabled.
    Replace,
}

impl Display for ConflictClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConflictClause::None => write!(f, ""),
            ConflictClause::Replace => write!(f, "OR REPLACE"),
            ConflictClause::Rollback => write!(f, "OR ROLLBACK"),
            ConflictClause::Abort => write!(f, "OR ABORT"),
            ConflictClause::Fail => write!(f, "OR FAIL"),
            ConflictClause::Ignore => write!(f, "OR IGNORE"),
        }
    }
}

/// A [ForeignKeyConstraint](https://www.sqlite.org/syntax/foreign-key-clause.html)
#[derive(Debug, PartialEq, Clone)]
pub struct ForeignKeyClause {
    /// The name of the table that the foreign key constraint is defined on
    pub table_name: Identifier,

    /// The columns that the foreign key constraint is defined on
    pub columns: Vec<Identifier>,

    /// A list of actions to be taken when a constraint violation occurs
    /// Actions are started with ON or MATCH keyword
    pub constraint_actions: Vec<FKConstraintAction>,

    /// A Deferrable clause in a ForeignKeyConstraint
    pub deferrable: Option<FKDeferrableType>,
}

/// A FKConstraintAction, used in ALTER TABLE ADD COLUMN statement
#[derive(Debug, PartialEq, Clone)]
pub enum FKConstraintAction {
    OnDelete(FKAction),
    OnUpdate(FKAction),
    Match(Identifier),
}

/// A FKAction, used in ALTER TABLE ADD COLUMN statement
#[derive(Debug, PartialEq, Clone)]
pub enum FKAction {
    SetNull,
    SetDefault,
    Cascade,
    Restrict,
    NoAction,
}

/// A [Deferrable](https://www.sqlite.org/syntax/foreign-key-clause.html)
/// clause in a ForeignKeyConstraint
#[derive(Debug, PartialEq, Clone)]
pub enum FKDeferrableType {
    Deferrable,
    InitiallyImmediate,
    InitiallyDeferred,
    Not(Box<FKDeferrableType>),
}

/// A GeneratedColumnConstraint, used in ALTER TABLE ADD COLUMN statement
#[derive(Debug, PartialEq, Clone)]
pub struct GeneratedColumnConstraint {
    pub expression: Expression,
    pub generated_type: Option<GeneratedColumnType>,
}

/// A GeneratedColumnType, used in ALTER TABLE ADD COLUMN statement
#[derive(Debug, PartialEq, Clone)]
pub enum GeneratedColumnType {
    Virtual,
    Stored,
}
