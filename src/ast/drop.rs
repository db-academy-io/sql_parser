/// An AST for DROP TABLE SQL statement.
/// See details [sqlite-drop-table-statement]
///
/// [sqlite-drop-table-statement]:https://www.sqlite.org/lang_droptable.html
#[derive(Debug, PartialEq)]
pub struct DropTableStatement {
    pub if_exists: bool,
    pub schema_name: Option<String>,
    pub table_name: String,
}

impl DropTableStatement {
    pub fn name(name: String) -> Self {
        Self {
            if_exists: false,
            schema_name: None,
            table_name: name,
        }
    }
}

/// An AST for DROP INDEX SQL statement.
/// See details [sqlite-drop-index-statement]
///
/// [sqlite-drop-index-statement]:https://www.sqlite.org/lang_dropindex.html
#[derive(Debug, PartialEq)]
pub struct DropIndexStatement {
    pub if_exists: bool,
    pub schema_name: Option<String>,
    pub index_name: String,
}

impl DropIndexStatement {
    pub fn name(name: String) -> Self {
        Self {
            if_exists: false,
            schema_name: None,
            index_name: name,
        }
    }
}

/// An AST for DROP TRIGGER SQL statement.
/// See details [sqlite-drop-trigger-statement]
///
/// [sqlite-drop-trigger-statement]:https://www.sqlite.org/lang_droptrigger.html
#[derive(Debug, PartialEq)]
pub struct DropTriggerStatement {
    pub if_exists: bool,
    pub schema_name: Option<String>,
    pub trigger_name: String,
}

impl DropTriggerStatement {
    pub fn name(name: String) -> Self {
        Self {
            if_exists: false,
            schema_name: None,
            trigger_name: name,
        }
    }
}

/// An AST for DROP VIEW SQL statement.
/// See details [sqlite-drop-view-statement]
///
/// [sqlite-drop-view-statement]:https://www.sqlite.org/lang_dropview.html
#[derive(Debug, PartialEq)]
pub struct DropViewStatement {
    pub if_exists: bool,
    pub schema_name: Option<String>,
    pub view_name: String,
}

impl DropViewStatement {
    pub fn name(name: String) -> Self {
        Self {
            if_exists: false,
            schema_name: None,
            view_name: name,
        }
    }
}
