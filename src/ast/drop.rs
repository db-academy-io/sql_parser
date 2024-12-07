use super::Identifier;

/// An AST for DROP TABLE SQL statement.
/// See details [sqlite-drop-table-statement]
///
/// [sqlite-drop-table-statement]:https://www.sqlite.org/lang_droptable.html
#[derive(Debug, PartialEq)]
pub struct DropTableStatement {
    pub if_exists: bool,
    pub identifier: Identifier,
}

impl DropTableStatement {
    pub fn name(name: String) -> Self {
        Self {
            if_exists: false,
            identifier: Identifier::Single(name),
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
    pub identifier: Identifier,
}

impl DropIndexStatement {
    pub fn name(name: String) -> Self {
        Self {
            if_exists: false,
            identifier: Identifier::Single(name),
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
    pub identifier: Identifier,
}

impl DropTriggerStatement {
    pub fn name(name: String) -> Self {
        Self {
            if_exists: false,
            identifier: Identifier::Single(name),
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
    pub identifier: Identifier,
}

impl DropViewStatement {
    pub fn name(name: String) -> Self {
        Self {
            if_exists: false,
            identifier: Identifier::Single(name),
        }
    }
}
