use super::{Expression, Identifier};

/// An AST for VACUUM SQL statement.
/// See details [sqlite-vacuum-statement]
///
/// [sqlite-vacuum-statement]:https://www.sqlite.org/lang_vacuum.html
#[derive(Debug, PartialEq, Default)]
pub struct VacuumStatement {
    pub schema_name: Option<String>,
    pub file_name: Option<String>,
}

/// An AST for DETACH SQL statement.
/// See details [sqlite-detach-statement]
///
/// [sqlite-detach-statement]:https://www.sqlite.org/lang_detach.html
#[derive(Debug, PartialEq)]
pub struct DetachStatement {
    pub schema_name: String,
}

/// An AST for ANALYZE SQL statement.
/// See details [sqlite-analyze-statement]
///
/// [sqlite-analyze-statement]:https://www.sqlite.org/lang_analyze.html
#[derive(Debug, PartialEq, Default)]
pub struct AnalyzeStatement {
    pub schema_name: Option<String>,
    pub table_or_index_name: Option<String>,
}

/// An AST for REINDEX SQL statement.
/// See details [sqlite-reindex-statement]
///
/// [sqlite-reindex-statement]:https://www.sqlite.org/lang_reindex.html
#[derive(Debug, PartialEq, Default)]
pub struct ReindexStatement {
    /// schema name
    pub schema_name: Option<String>,
    /// The collation or table or index name
    pub target_name: Option<String>,
}

/// An AST for PRAGMA SQL statement.
/// See details [sqlite-pragma-statement]
///
/// [sqlite-pragma-statement]:https://www.sqlite.org/pragma.html
#[derive(Debug, PartialEq)]
pub struct PragmaStatement {
    /// schema name
    pub schema_name: Option<String>,
    /// pragma name
    pub pragma_name: String,
    /// pragma value
    pub pragma_value: Option<String>,
}

#[derive(Debug, PartialEq)]
pub struct AttachStatement {
    /// an attach expression
    pub expression: Expression,
    /// an attached schema name
    pub schema_name: Identifier,
}
