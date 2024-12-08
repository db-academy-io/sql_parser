use crate::{CommitTransactionStatement, Keyword, Parser, ParsingError, Statement};

pub trait CommitStatementParser {
    fn parse_commit_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> CommitStatementParser for Parser<'a> {
    fn parse_commit_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the COMMIT OR END keyword
        let _ = self.consume_keyword(Keyword::Commit);
        let _ = self.consume_keyword(Keyword::End);

        // Check if we've got only 'COMMIT;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::CommitTransaction(CommitTransactionStatement));
        }

        let _ = self.consume_keyword(Keyword::Transaction);

        self.finalize_statement_parsing()?;
        Ok(Statement::CommitTransaction(CommitTransactionStatement))
    }
}
