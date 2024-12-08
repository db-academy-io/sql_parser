use crate::{Keyword, Parser, ParsingError, RollbackTransactionStatement, Statement};

use super::savepoint::SavepointStatementParser;

pub trait RollbackStatementParser {
    fn parse_rollback_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> RollbackStatementParser for Parser<'a> {
    fn parse_rollback_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_keyword(Keyword::Rollback)?;

        // Consume the optional TRANSACTION keyword
        let _ = self.consume_keyword(Keyword::Transaction);

        if self.consume_keyword(Keyword::To).is_ok() {
            let _ = self.consume_keyword(Keyword::Savepoint);
            let savepoint_name = self.parse_savepoint_name()?;
            self.finalize_statement_parsing()?;
            return Ok(Statement::RollbackTransaction(
                RollbackTransactionStatement {
                    savepoint_name: Some(savepoint_name.to_string()),
                },
            ));
        }

        self.finalize_statement_parsing()?;
        Ok(Statement::RollbackTransaction(
            RollbackTransactionStatement::default(),
        ))
    }
}
