use super::Expression;

/// An AST for SELECT SQL statement.
/// See details [sqlite-select-statement]
///
/// [sqlite-select-statement]:https://www.sqlite.org/lang_select.html
#[derive(Debug, PartialEq, Default, Clone)]
pub struct SelectStatement {
    /// Whether the SELECT statement is distinct
    pub distinct: bool,

    /// Whether the SELECT statement is all
    pub all: bool,

    /// The list of columns to select
    pub columns: Vec<SelectItem>,

    /// The FROM clause
    pub from: Option<SelectFrom>,
    // pub where_clause: Option<Expression>,
    // pub order_by: Option<OrderBy>,
    // pub limit: Option<Expression>,
    // pub offset: Option<Expression>,
}

/// An enum representing the possible items in a SELECT statement
#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum SelectFrom {
    Table(SelectFromTable),

    Function(SelectFromFunction),

    Subquery(SelectFromSubquery),

    Froms(Vec<SelectFrom>),

    Join(JoinClause),
}

#[derive(Debug, PartialEq, Clone)]
pub struct SelectFromTable {
    pub schema: Option<String>,
    pub table_name: String,
    pub alias: Option<String>,

    pub indexed_by: Option<String>,
    pub not_indexed: Option<bool>,
}

/// A function in a FROM clause
#[derive(Debug, PartialEq, Clone)]
pub struct SelectFromFunction {
    pub schema: Option<String>,
    pub function_name: String,
    pub arguments: Vec<Expression>,
    pub alias: Option<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SelectFromSubquery {
    pub subquery: Box<SelectStatement>,
    pub alias: Option<String>,
}

/// A clause for a JOIN statement
#[derive(Debug, PartialEq, Clone)]
pub struct JoinClause {
    pub lhs_table: Box<SelectFrom>,
    pub join_type: JoinType,
    pub rhs_table: Box<SelectFrom>,
    pub join_constraints: Vec<JoinConstraint>,
}

/// A type of JOIN clause
#[derive(Debug, PartialEq, Clone)]
pub enum JoinType {
    /// INNER JOIN
    Inner,
    /// LEFT JOIN
    Left,
    /// RIGHT JOIN
    Right,
    /// NATURAL JOIN
    Natural(Box<JoinType>),
    /// OUTER JOIN (LEFT OUTER JOIN, RIGHT OUTER JOIN, FULL OUTER JOIN)
    Outer(Box<JoinType>),
    /// CROSS JOIN
    Cross,
}

/// A constraint for a JOIN clause
#[derive(Debug, PartialEq, Clone)]
pub enum JoinConstraint {
    /// ON clause
    On(Expression),
    /// USING clause
    Using(Vec<String>),
}
