use crate::{BeginTransactionStatement, Keyword, Parser, ParsingError, Statement, TransactionType};

pub trait BeginStatementParser {
    fn parse_begin_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> BeginStatementParser for Parser<'a> {
    fn parse_begin_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_keyword(Keyword::Begin)?;

        let mut statement = BeginTransactionStatement::default();

        // Check if we've got only 'BEGIN;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::BeginTransaction(statement));
        }

        if self.consume_keyword(Keyword::Deferred).is_ok() {
            statement.transaction_type = Some(TransactionType::Deferred);
        } else if self.consume_keyword(Keyword::Immediate).is_ok() {
            statement.transaction_type = Some(TransactionType::Immediate);
        } else if self.consume_keyword(Keyword::Exclusive).is_ok() {
            statement.transaction_type = Some(TransactionType::Exclusive);
        }

        // Consume the optional TRANSACTION keyword
        let _ = self.consume_keyword(Keyword::Transaction);

        self.finalize_statement_parsing()?;
        Ok(Statement::BeginTransaction(statement))
    }
}
