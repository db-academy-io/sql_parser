use std::fmt::Display;

/// An identifier
#[derive(Debug, PartialEq, Clone)]
pub enum Identifier {
    /// A single identifier
    Single(String),

    /// A compound identifier
    Compound(Vec<String>),

    /// A wildcard
    Wildcard,

    /// A table or column name with wildcard
    NameWithWildcard(String),
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Self {
        if s == "*" {
            return Identifier::Wildcard;
        }
        Identifier::Single(s.to_string())
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Single(s) => write!(f, "{}", s),
            Identifier::Compound(s) => write!(f, "{}", s.join(".")),
            Identifier::Wildcard => write!(f, "*"),
            Identifier::NameWithWildcard(s) => write!(f, "{}*", s),
        }
    }
}
