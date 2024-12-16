use crate::UpdateStatement;

use super::{Parser, ParsingError};

pub trait UpdateStatementParser {
    fn parse_update_statement(&mut self) -> Result<UpdateStatement, ParsingError>;
}

impl<'a> UpdateStatementParser for Parser<'a> {
    fn parse_update_statement(&mut self) -> Result<UpdateStatement, ParsingError> {
        todo!()
    }
}
