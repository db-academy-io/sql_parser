pub mod drop_generic;
pub mod drop_index;
pub mod drop_table;
pub mod drop_trigger;
pub mod drop_view;

use drop_generic::*;
use drop_index::*;
use drop_table::*;
use drop_trigger::*;
use drop_view::*;

use crate::parser::errors::ParsingError;
use crate::{Keyword, Parser, Statement};

pub trait DropStatementParser {
    fn parse_drop_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> DropStatementParser for Parser<'a> {
    /// Parse DROP statement
    /// The DROP statement consists of the
    ///   - DROP TABLE statements
    ///   - DROP VIEW statements
    ///   - DROP INDEX statements
    ///   - DROP TRIGGER statements
    fn parse_drop_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Drop)?;
        let keyword = self.peek_as_keyword()?;

        let drop_statement = match keyword {
            Keyword::Table => self.parse_drop_table_statement()?,
            Keyword::View => self.parse_drop_view_statement()?,
            Keyword::Index => self.parse_drop_index_statement()?,
            Keyword::Trigger => self.parse_drop_trigger_statement()?,
            _ => return Err(ParsingError::UnexpectedKeyword(keyword)),
        };

        self.finalize_statement_parsing()?;

        Ok(drop_statement)
    }
}
