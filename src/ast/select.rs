use super::{Expression, Identifier, OrderingTerm, WindowDefinition, WithCteStatement};
use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub struct SelectStatement {
    pub with_cte: Option<WithCteStatement>,

    pub select: SelectBody,

    pub order_by: Option<Vec<OrderingTerm>>,

    pub limit: Option<LimitClause>,
}

/// An enum representing the possible types of [SELECT](https://www.sqlite.org/lang_select.html) statements
#[derive(Debug, PartialEq, Clone)]
pub enum SelectBody {
    /// A normal SELECT statement
    Select(Select),
    /// A VALUES statement
    Values(ValuesStatement),
    /// Union statement
    Union(UnionStatement),
}

/// An AST for [SELECT](https://www.sqlite.org/lang_select.html) SQL statement
#[derive(Debug, PartialEq, Default, Clone)]
pub struct Select {
    /// Whether the SELECT statement is distinct
    pub distinct_type: DistinctType,

    /// The list of columns to select
    pub columns: Vec<SelectItem>,

    /// The FROM clause
    pub from: Option<FromClause>,

    /// The WHERE clause
    pub where_clause: Option<Box<Expression>>,

    /// The GROUP BY clause
    pub group_by: Option<Vec<Expression>>,

    /// The HAVING clause
    pub having: Option<Box<Expression>>,

    /// The WINDOW clause
    pub window: Option<Vec<NamedWindowDefinition>>,
}

/// An enum representing the possible distinct options in the
/// [SELECT](https://www.sqlite.org/lang_select.html) statement
#[derive(Debug, PartialEq, Clone, Default)]
pub enum DistinctType {
    Distinct,
    All,
    #[default]
    None,
}

/// An enum representing the possible column representations in a
/// [SELECT](https://www.sqlite.org/lang_select.html) statement
#[derive(Debug, PartialEq, Clone)]
pub enum SelectItem {
    /// A single expression
    Expression(Expression),

    /// An expression with an alias
    ExpressionWithAlias(Expression, String),
}

/// An AST for representing a FROM clause in the
/// [SELECT](https://www.sqlite.org/lang_select.html) statement
#[derive(Debug, PartialEq, Clone)]
pub enum FromClause {
    /// Select from a table
    Table(QualifiedTableName),

    /// Select from a function
    Function(SelectFromFunction),

    /// Select from a subquery
    Subquery(SelectFromSubquery),

    /// Select from cartesian product of tables
    Froms(Vec<FromClause>),

    /// Select from a JOIN clause
    Join(JoinClause),
}

/// A [Qualified Table Name](https://www.sqlite.org/syntax/qualified-table-name.html)
#[derive(Debug, PartialEq, Clone)]
pub struct QualifiedTableName {
    /// The table name
    pub table_id: Identifier,

    /// The alias for the table
    pub alias: Option<String>,

    /// The table index option
    pub indexed_type: Option<IndexedType>,
}

impl From<Identifier> for QualifiedTableName {
    fn from(table_id: Identifier) -> Self {
        Self {
            table_id,
            alias: None,
            indexed_type: None,
        }
    }
}

/// An enum representing the possible table index types
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
    pub subquery: Box<SelectStatement>,
    pub alias: Option<String>,
}

/// A clause for a JOIN statement
#[derive(Debug, PartialEq, Clone)]
pub struct JoinClause {
    pub lhs_table: Box<FromClause>,

    pub join_tables: Vec<JoinTable>,
}

/// A table in a JOIN clause
#[derive(Debug, PartialEq, Clone)]
pub struct JoinTable {
    pub join_type: JoinType,
    pub table: Box<FromClause>,
    pub constraints: Option<JoinConstraint>,
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

impl Display for JoinType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JoinType::Left(is_natural) => {
                write!(f, "{} LEFT", if *is_natural { "NATURAL" } else { "" })
            }
            JoinType::Right(is_natural) => {
                write!(f, "{} RIGHT", if *is_natural { "NATURAL" } else { "" })
            }
            JoinType::Full(is_natural) => {
                write!(f, "{} FULL", if *is_natural { "NATURAL" } else { "" })
            }
            JoinType::Inner(is_natural) => {
                write!(f, "{} INNER", if *is_natural { "NATURAL" } else { "" })
            }
            JoinType::Cross => write!(f, "CROSS"),
        }
    }
}

/// A constraint for a JOIN clause
#[derive(Debug, PartialEq, Clone)]
pub enum JoinConstraint {
    /// ON clause
    On(Expression),
    /// USING clause
    Using(Vec<Identifier>),
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

/// An union statement type, based on the [compound-operator](https://www.sqlite.org/syntax/compound-operator.html)
#[derive(Debug, PartialEq, Clone)]
pub enum UnionStatementType {
    Union,
    UnionAll,
    Intersect,
    Except,
}

impl Display for UnionStatementType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnionStatementType::Union => write!(f, "UNION"),
            UnionStatementType::UnionAll => write!(f, "UNION ALL"),
            UnionStatementType::Intersect => write!(f, "INTERSECT"),
            UnionStatementType::Except => write!(f, "EXCEPT"),
        }
    }
}

/// A [UNION](https://www.sqlite.org/lang_select.html#union) statement
///
/// A union statement is a statement that combines the results of two SELECT
/// statements with the `union_type`.
#[derive(Debug, PartialEq, Clone)]
pub struct UnionStatement {
    pub union_type: UnionStatementType,
    pub left: Box<SelectStatement>,
    pub right: Box<SelectStatement>,
}

/// A window definition
#[derive(Debug, PartialEq, Clone, Default)]
pub struct NamedWindowDefinition {
    /// The window name
    pub window_name: String,

    /// The window definition
    pub window_definition: WindowDefinition,
}
