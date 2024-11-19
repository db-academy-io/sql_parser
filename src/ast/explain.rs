use super::Statement;

#[derive(Debug, PartialEq)]
pub struct ExplainStatement {
    /// The statement to explain
    pub statement: Box<Statement>,

    /// Whether to include the query plan
    pub query_plan: bool,
}
