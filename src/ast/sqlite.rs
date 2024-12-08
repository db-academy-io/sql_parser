use super::{Expression, Identifier};

/// An AST for [VACUUM](https://www.sqlite.org/lang_vacuum.html) SQL statement.
#[derive(Debug, PartialEq, Default)]
pub struct VacuumStatement {
    pub schema_name: Option<String>,
    pub file_name: Option<String>,
}

/// An AST for [DETACH](https://www.sqlite.org/lang_detach.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct DetachStatement {
    pub schema_name: String,
}

/// An AST for [ANALYZE](https://www.sqlite.org/lang_analyze.html) SQL statement.
#[derive(Debug, PartialEq, Default)]
pub struct AnalyzeStatement {
    pub schema_name: Option<String>,
    pub table_or_index_name: Option<String>,
}

/// An AST for [REINDEX](https://www.sqlite.org/lang_reindex.html) SQL statement.
#[derive(Debug, PartialEq, Default)]
pub struct ReindexStatement {
    /// schema name
    pub schema_name: Option<String>,
    /// The collation or table or index name
    pub target_name: Option<String>,
}

/// An AST for [PRAGMA](https://www.sqlite.org/pragma.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct PragmaStatement {
    /// schema name
    pub schema_name: Option<String>,
    /// pragma name
    pub pragma_name: String,
    /// pragma value
    pub pragma_value: Option<String>,
}

/// An AST for [ATTACH](https://www.sqlite.org/lang_attach.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct AttachStatement {
    /// an attach expression
    pub expression: Expression,
    /// an attached schema name
    pub schema_name: Identifier,
}
