use crate::{Parser, ParsingError};

pub trait SQLite3NameParser {
    /// Parses a name
    fn parse_sqlite3_name(&mut self) -> Result<String, ParsingError>;
}

impl<'a> SQLite3NameParser for Parser<'a> {
    fn parse_sqlite3_name(&mut self) -> Result<String, ParsingError> {
        if let Ok(value) = self.peek_as_number() {
            self.consume_as_number()?;

            return Ok(value.to_string());
        }

        if let Ok(value) = self.peek_as_id() {
            self.consume_as_id()?;

            return Ok(value.to_string());
        }

        let value = self.peek_as_string()?;
        self.consume_as_string()?;
        Ok(value)
    }
}
