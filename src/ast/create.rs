use super::{
    ColumnDefinition, ConflictClause, DeleteStatement, Expression, ForeignKeyClause, Identifier,
    IndexedColumn, InsertStatement, SelectStatement, UpdateStatement,
};

/// An AST for [CREATE VIRTUAL TABLE](https://www.sqlite.org/lang_createvtab.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct CreateVirtualTableStatement {
    pub if_not_exists: bool,

    pub table_name: Identifier,

    pub module_name: Identifier,

    pub module_arguments: Vec<Expression>,
}

/// An AST for [CREATE INDEX](https://www.sqlite.org/lang_createindex.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct CreateIndexStatement {
    pub unique: bool,

    pub if_not_exists: bool,

    pub index_name: Identifier,

    pub table_name: Identifier,

    pub columns: Vec<Identifier>,

    pub where_clause: Option<Expression>,
}

/// An AST for [CREATE VIEW](https://www.sqlite.org/lang_createview.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct CreateViewStatement {
    pub temporary: bool,

    pub if_not_exists: bool,

    pub view_name: Identifier,

    pub columns: Vec<Identifier>,

    pub select_statement: SelectStatement,
}

/// An AST for [CREATE TABLE](https://www.sqlite.org/lang_createtable.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct CreateTableStatement {
    pub temporary: bool,

    pub if_not_exists: bool,

    pub table_name: Identifier,

    pub create_table_option: CreateTableOption,
}

#[derive(Debug, PartialEq)]
pub enum CreateTableOption {
    ColumnDefinitions(Vec<CreateTableColumnDef>),

    SelectStatement(SelectStatement),
}

#[derive(Debug, PartialEq)]
pub struct CreateTableColumnDef {
    pub columns: Vec<ColumnDefinition>,

    pub table_constraints: Vec<TableConstraint>,

    pub table_options: Vec<TableOption>,
}

#[derive(Debug, PartialEq)]
pub struct TableConstraint {
    pub constraint_name: Option<Identifier>,

    pub constraint_type: TableConstraintType,
}

#[derive(Debug, PartialEq)]
pub enum TableConstraintType {
    PrimaryKey(Vec<IndexedColumn>, ConflictClause),

    Unique(Vec<IndexedColumn>, ConflictClause),

    Check(Expression),

    ForeignKey(Vec<Identifier>, ForeignKeyClause),
}

#[derive(Debug, PartialEq)]
pub enum TableOption {
    Strict,

    WithoutRowId,
}

/// An AST for [CREATE TRIGGER](https://www.sqlite.org/lang_createtrigger.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct CreateTriggerStatement {
    pub temporary: bool,

    pub if_not_exists: bool,

    pub trigger_name: Identifier,

    pub trigger_pre_condition: Option<TriggerPreCondition>,

    pub trigger_event: TriggerEvent,

    pub for_each_row: bool,

    pub when_clause: Option<Expression>,

    pub trigger_statements: Vec<TriggerStatement>,
}

#[derive(Debug, PartialEq)]
pub enum TriggerPreCondition {
    Before,

    After,

    InsteadOf,
}

#[derive(Debug, PartialEq)]
pub struct TriggerEvent {
    pub event_type: TriggerEventType,

    pub table_name: Identifier,
}

#[derive(Debug, PartialEq)]
pub enum TriggerEventType {
    Delete,
    Insert,
    Update(Option<Vec<Identifier>>),
}

#[derive(Debug, PartialEq)]
pub enum TriggerStatement {
    Update(UpdateStatement),

    Insert(InsertStatement),

    Delete(DeleteStatement),

    Select(SelectStatement),
}
