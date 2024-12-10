use crate::{Parser, ParsingError, ValuesStatement};

pub trait ValuesStatementParser {
    fn parse_values_statement(&mut self) -> Result<ValuesStatement, ParsingError>;
}

impl<'a> ValuesStatementParser for Parser<'a> {
    fn parse_values_statement(&mut self) -> Result<ValuesStatement, ParsingError> {
        Ok(ValuesStatement { values: Vec::new() })
    }
}
