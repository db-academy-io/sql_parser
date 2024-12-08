use super::Identifier;

/// An AST for [DROP TABLE](https://www.sqlite.org/lang_droptable.html) SQL statement.
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

/// An AST for [DROP INDEX](https://www.sqlite.org/lang_dropindex.html) SQL statement.
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

/// An AST for [DROP TRIGGER](https://www.sqlite.org/lang_droptrigger.html) SQL statement.
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

/// An AST for [DROP VIEW](https://www.sqlite.org/lang_dropview.html) SQL statement.
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
