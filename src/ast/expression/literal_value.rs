/// A literal value
#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    /// A number
    Number(String),

    /// A string
    String(String),

    /// A blob
    Blob(String),

    /// A boolean
    Boolean(bool),

    /// A null value
    Null,

    /// True
    True,

    /// False
    False,

    /// Current time
    CurrentTime,

    /// Current date
    CurrentDate,

    /// Current timestamp
    CurrentTimestamp,
}
