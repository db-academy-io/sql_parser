/// A DROP table statement
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
    pub fn table_name(name: String) -> Self {
        DropTableStatement {
            if_exists: false,
            schema_name: None,
            table_name: name,
        }
    }
}
