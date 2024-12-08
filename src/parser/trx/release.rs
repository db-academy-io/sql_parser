use crate::{Keyword, Parser, ParsingError, ReleaseStatement, Statement};

use super::savepoint::SavepointStatementParser;

pub trait ReleaseStatementParser {
    fn parse_release_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> ReleaseStatementParser for Parser<'a> {
    fn parse_release_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_keyword(Keyword::Release)?;

        let _ = self.consume_keyword(Keyword::Savepoint);

        let savepoint_name = self.parse_savepoint_name()?;
        self.finalize_statement_parsing()?;
        Ok(Statement::Release(ReleaseStatement { savepoint_name }))
    }
}
