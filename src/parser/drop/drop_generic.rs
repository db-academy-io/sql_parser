use crate::{expression::IdentifierParser, Identifier, Keyword, Parser, ParsingError};

pub trait DropGenericStatementParser {
    fn parse_drop_statement_generic(&mut self) -> Result<(bool, Identifier), ParsingError>;
}

impl<'a> DropGenericStatementParser for Parser<'a> {
    fn parse_drop_statement_generic(&mut self) -> Result<(bool, Identifier), ParsingError> {
        // Parse optional [IF EXISTS] part of the statement
        let if_exists = if self.consume_as_keyword(Keyword::If).is_ok() {
            self.consume_as_keyword(Keyword::Exists)?;
            true
        } else {
            false
        };
        let identifier = self.parse_identifier()?;
        Ok((if_exists, identifier))
    }
}
