/// A data type enum, representing the [sqlite-data-types](https://www.sqlite.org/datatype3.html)
#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
    /// A data type name, e.g. INTEGER
    PlainDataType(DataTypeName),

    /// A sized data type, e.g. VARCHAR(10)
    SizedDataType(DataTypeName, String),

    /// A bounded data type name, e.g. VARCHAR(1, 10)
    BoundedDataType(DataTypeName, String, String),
}

/// A data type name
#[derive(Debug, PartialEq, Clone)]
pub enum DataTypeName {
    /// A single data type name, e.g. INTEGER
    Single(String),
    /// A compound data type name, e.g. DOUBLE PRECISION
    Compound(Vec<String>),
}

impl From<&str> for DataTypeName {
    fn from(s: &str) -> Self {
        if s.contains(" ") {
            DataTypeName::Compound(s.split(" ").map(|s| s.to_string()).collect())
        } else {
            DataTypeName::Single(s.to_string())
        }
    }
}
