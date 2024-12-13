use super::{Expression, Identifier, OrderingTerm, WindowDefinition};

/// An enum representing the possible types of SELECT statements
#[derive(Debug, PartialEq, Clone)]
pub enum SelectStatementType {
    /// A normal SELECT statement
    Select(SelectStatement),
    /// A VALUES statement
    Values(ValuesStatement),
}

/// An AST for [SELECT](https://www.sqlite.org/lang_select.html) SQL statement.
#[derive(Debug, PartialEq, Default, Clone)]
pub struct SelectStatement {
    /// Whether the SELECT statement is distinct
    pub distinct_type: DistinctType,

    /// The list of columns to select
    pub columns: Vec<SelectItem>,

    /// The FROM clause
    pub from: Option<SelectFrom>,

    /// The WHERE clause
    pub where_clause: Option<Box<Expression>>,

    /// The GROUP BY clause
    pub group_by: Option<Vec<Box<Expression>>>,

    /// The HAVING clause
    pub having: Option<Box<Expression>>,

    /// The WINDOW clause
    pub window: Option<Vec<WindowDefinition>>,

    /// The ORDER BY clause
    pub order_by: Option<OrderingTerm>,

    /// The LIMIT clause
    pub limit: Option<LimitClause>,
}

/// An enum representing the possible distinct types
#[derive(Debug, PartialEq, Clone, Default)]
pub enum DistinctType {
    Distinct,
    All,
    #[default]
    None,
}

/// An enum representing the possible items in a SELECT statement
#[derive(Debug, PartialEq, Clone)]
pub enum SelectItem {
    /// A single expression
    Expression(Expression),

    /// An expression with an alias
    ExpressionWithAlias(Expression, String),
}

/// An AST for representing a FROM clause
#[derive(Debug, PartialEq, Clone)]
pub enum SelectFrom {
    Table(SelectFromTable),

    Function(SelectFromFunction),

    Subquery(SelectFromSubquery),

    Froms(Vec<SelectFrom>),

    Join(JoinClause),
}

/// A table in a FROM clause
#[derive(Debug, PartialEq, Clone)]
pub struct SelectFromTable {
    pub table_id: Identifier,

    pub alias: Option<String>,

    pub indexed_type: Option<IndexedType>,
}

impl From<Identifier> for SelectFromTable {
    fn from(table_id: Identifier) -> Self {
        Self {
            table_id,
            alias: None,
            indexed_type: None,
        }
    }
}

/// An enum representing the possible indexed types
#[derive(Debug, PartialEq, Clone)]
pub enum IndexedType {
    /// Indexed by a specific index
    Indexed(String),
    /// Not indexed
    NotIndexed,
}

/// A function in a FROM clause
#[derive(Debug, PartialEq, Clone)]
pub struct SelectFromFunction {
    pub function_name: Identifier,
    pub arguments: Vec<Expression>,
    pub alias: Option<String>,
}

/// A subquery in a FROM clause
#[derive(Debug, PartialEq, Clone)]
pub struct SelectFromSubquery {
    pub subquery: Box<SelectStatementType>,
    pub alias: Option<String>,
}

/// A clause for a JOIN statement
#[derive(Debug, PartialEq, Clone)]
pub struct JoinClause {
    pub lhs_table: Box<SelectFrom>,

    pub join_tables: Vec<JoinTable>,
}

/// A table in a JOIN clause
#[derive(Debug, PartialEq, Clone)]
pub struct JoinTable {
    pub join_type: JoinType,
    pub table: Box<SelectFrom>,
    pub constraints: JoinConstraint,
}

/// A type alias for the `NATURAL` keyword in a JOIN clause
pub type IsNaturalJoin = bool;

/// A type of JOIN clause
#[derive(Debug, PartialEq, Clone)]
pub enum JoinType {
    /// LEFT JOIN
    Left(IsNaturalJoin),
    /// RIGHT JOIN
    Right(IsNaturalJoin),
    /// FULL JOIN
    Full(IsNaturalJoin),
    /// INNER JOIN
    Inner(IsNaturalJoin),
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

/// A clause for a LIMIT statement
#[derive(Debug, PartialEq, Clone)]
pub struct LimitClause {
    pub limit: Box<Expression>,
    pub offset: Option<Box<Expression>>,
    pub additional_limit: Option<Box<Expression>>,
}

/// A VALUES statement
#[derive(Debug, PartialEq, Clone)]
pub struct ValuesStatement {
    /// The list of values in a VALUES statement grouped by parenthesis
    pub values: Vec<Vec<Expression>>,
}
