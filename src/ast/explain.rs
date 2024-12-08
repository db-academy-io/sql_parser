use super::Statement;

/// An AST for [EXPLAIN](https://www.sqlite.org/lang_explain.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct ExplainStatement {
    /// The statement to explain
    pub statement: Box<Statement>,

    /// Whether to include the query plan
    pub query_plan: bool,
}
