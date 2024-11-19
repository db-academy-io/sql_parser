use super::Expression;

/// An AST for SELECT SQL statement.
/// See details [sqlite-select-statement]
///
/// [sqlite-select-statement]:https://www.sqlite.org/lang_select.html
#[derive(Debug, PartialEq)]
pub struct SelectStatement {
    /// The list of columns to select
    pub columns: Vec<SelectItem>,
    // pub from: Option<SelectFrom>,
    // pub where_clause: Option<Expression>,
    // pub order_by: Option<OrderBy>,
    // pub limit: Option<Expression>,
    // pub offset: Option<Expression>,
}

/// An enum representing the possible items in a SELECT statement
#[derive(Debug, PartialEq)]
pub enum SelectItem {
    /// A single expression
    Expression(Expression),

    /// An expression with an alias
    ExpressionWithAlias(Expression, String),

    /// A wildcard (*), which matches all columns
    Wildcard,

    /// A table name with a wildcard
    TableNameWithWildcard(String),
}
