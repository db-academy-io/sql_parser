/// An ordering
#[derive(Debug, PartialEq, Clone)]
pub enum Ordering {
    /// Ascending
    Asc,
    /// Descending
    Desc,
}

/// Nulls ordering
#[derive(Debug, PartialEq, Clone)]
pub enum NullsOrdering {
    /// Nulls first
    First,
    /// Nulls last
    Last,
}
