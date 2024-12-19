mod create_index;
mod create_table;
mod create_trigger;
mod create_view;
mod create_virtual_table;

use create_index::CreateIndexStatementParser;
use create_view::CreateViewStatementParser;
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
            Keyword::Unique => {
                self.consume_as_keyword(Keyword::Unique)?;
                CreateIndexStatementParser::parse_create_index_statement(self, true)
                    .map(Statement::CreateIndex)
            }
            Keyword::Index => CreateIndexStatementParser::parse_create_index_statement(self, false)
                .map(Statement::CreateIndex),

            Keyword::View => CreateViewStatementParser::parse_create_view_statement(self, false)
                .map(Statement::CreateView),

            Keyword::Temp => {
                self.consume_as_keyword(Keyword::Temp)?;
                if let Ok(Keyword::View) = self.peek_as_keyword() {
                    CreateViewStatementParser::parse_create_view_statement(self, true)
                        .map(Statement::CreateView)
                } else {
                    Err(ParsingError::UnexpectedParsingState(
                        "Expected table name after TEMP keyword".to_string(),
                    ))
                }
            }

            Keyword::Temporary => {
                self.consume_as_keyword(Keyword::Temporary)?;
                if let Ok(Keyword::View) = self.peek_as_keyword() {
                    CreateViewStatementParser::parse_create_view_statement(self, true)
                        .map(Statement::CreateView)
                } else {
                    Err(ParsingError::UnexpectedParsingState(
                        "Expected table name after TEMP keyword".to_string(),
                    ))
                }
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
