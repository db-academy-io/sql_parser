use crate::{Keyword, Parser, ParsingError, SavepointStatement, Statement};

pub trait SavepointStatementParser {
    fn parse_savepoint_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_savepoint_name(&mut self) -> Result<String, ParsingError>;
}

impl<'a> SavepointStatementParser for Parser<'a> {
    fn parse_savepoint_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the SAVEPOINT keyword
        self.consume_keyword(Keyword::Savepoint)?;

        let savepoint_name = self.parse_savepoint_name()?;

        self.finalize_statement_parsing()?;
        Ok(Statement::Savepoint(SavepointStatement { savepoint_name }))
    }

    fn parse_savepoint_name(&mut self) -> Result<String, ParsingError> {
        if let Ok(id) = self.peek_as_id() {
            // Consume the id token
            self.consume_token()?;
            return Ok(id.to_string());
        }

        if let Ok(string) = self.peek_as_string() {
            // Consume the id token
            self.consume_token()?;
            return Ok(string.to_string());
        }

        let token = self.peek_token()?;
        Err(ParsingError::UnexpectedToken(token.to_string()))
    }
}
