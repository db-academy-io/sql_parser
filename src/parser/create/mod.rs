pub mod create_index;
pub mod create_table;
pub mod create_trigger;
pub mod create_view;
pub mod create_virtual_table;

use create_index::CreateIndexStatementParser;
use create_table::CreateTableStatementParser;
use create_trigger::CreateTriggerStatementParser;
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

            Keyword::Table => CreateTableStatementParser::parse_create_table_statement(self, false)
                .map(Statement::CreateTable),

            Keyword::Trigger => {
                CreateTriggerStatementParser::parse_create_trigger_statement(self, false)
                    .map(Statement::CreateTrigger)
            }

            Keyword::Temp => {
                self.consume_as_keyword(Keyword::Temp)?;
                if let Ok(Keyword::View) = self.peek_as_keyword() {
                    CreateViewStatementParser::parse_create_view_statement(self, true)
                        .map(Statement::CreateView)
                } else if let Ok(Keyword::Table) = self.peek_as_keyword() {
                    CreateTableStatementParser::parse_create_table_statement(self, true)
                        .map(Statement::CreateTable)
                } else if let Ok(Keyword::Trigger) = self.peek_as_keyword() {
                    CreateTriggerStatementParser::parse_create_trigger_statement(self, true)
                        .map(Statement::CreateTrigger)
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
                } else if let Ok(Keyword::Table) = self.peek_as_keyword() {
                    CreateTableStatementParser::parse_create_table_statement(self, true)
                        .map(Statement::CreateTable)
                } else if let Ok(Keyword::Trigger) = self.peek_as_keyword() {
                    CreateTriggerStatementParser::parse_create_trigger_statement(self, true)
                        .map(Statement::CreateTrigger)
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
