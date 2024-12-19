mod create_index;
mod create_table;
mod create_trigger;
mod create_view;
mod create_virtual_table;

pub use create_virtual_table::*;

use crate::{Keyword, Statement};

use super::{Parser, ParsingError};

pub trait CreateStatementParser {
    fn parse_create_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_if_not_exists(&mut self) -> Result<bool, ParsingError>;
}

impl<'a> CreateStatementParser for Parser<'a> {
    fn parse_create_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Create)?;

        match self.peek_as_keyword()? {
            Keyword::Virtual => {
                CreateVirtualTableStatementParser::parse_create_virtual_table_statement(self)
                    .map(Statement::CreateVirtualTable)
            }
            _ => Err(ParsingError::UnexpectedParsingState(
                "Unimplemented".to_string(),
            )),
        }
    }

    fn parse_if_not_exists(&mut self) -> Result<bool, ParsingError> {
        if self.consume_as_keyword(Keyword::If).is_ok() {
            self.consume_as_keyword(Keyword::Not)?;
            self.consume_as_keyword(Keyword::Exists)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
}
