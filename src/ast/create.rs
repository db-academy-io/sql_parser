/// An AST for [CREATE TABLE](https://www.sqlite.org/lang_createtable.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct CreateTableStatement {}

/// An AST for [CREATE INDEX](https://www.sqlite.org/lang_createindex.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct CreateIndexStatement {}

/// An AST for [CREATE VIEW](https://www.sqlite.org/lang_createview.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct CreateViewStatement {}

/// An AST for [CREATE TRIGGER](https://www.sqlite.org/lang_createtrigger.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct CreateTriggerStatement {}

/// An AST for [CREATE VIRTUAL TABLE](https://www.sqlite.org/lang_createtable.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct CreateVirtualTableStatement {}
