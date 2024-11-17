/// An AST for VACUUM SQL statement.
/// See details [sqlite-vacuum-statement]
///
/// [sqlite-vacuum-statement]:https://www.sqlite.org/lang_vacuum.html
///
#[derive(Debug, PartialEq, Default)]
pub struct VacuumStatement {
    pub schema_name: Option<String>,
    pub file_name: Option<String>,
}

#[derive(Debug, PartialEq)]
pub struct DetachStatement {
    pub schema_name: String,
}

#[derive(Debug, PartialEq, Default)]
pub struct AnalyzeStatement {
    pub schema_name: Option<String>,
    pub table_or_index_name: Option<String>,
}

#[derive(Debug, PartialEq)]
pub struct ReindexStatement {}

#[derive(Debug, PartialEq)]
pub struct PragmaStatement {}

#[derive(Debug, PartialEq)]
pub struct AttachStatement {}
