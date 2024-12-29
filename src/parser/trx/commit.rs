use crate::parser::errors::ParsingError;
use crate::{CommitTransactionStatement, Keyword, Parser, Statement};

pub trait CommitStatementParser {
    fn parse_commit_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> CommitStatementParser for Parser<'a> {
    fn parse_commit_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the COMMIT OR END keyword
        let _ = self.consume_as_keyword(Keyword::Commit);
        let _ = self.consume_as_keyword(Keyword::End);

        // Check if we've got only 'COMMIT;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::CommitTransaction(CommitTransactionStatement));
        }

        let _ = self.consume_as_keyword(Keyword::Transaction);

        self.finalize_statement_parsing()?;
        Ok(Statement::CommitTransaction(CommitTransactionStatement))
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::CommitTransactionStatement;

    pub fn commit_statement() -> CommitTransactionStatement {
        CommitTransactionStatement {}
    }
}

#[cfg(test)]
mod commit_statements_tests {
    use crate::parser::errors::ParsingError;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{CommitTransactionStatement, Parser, Statement};

    #[test]
    fn commit_transaction() {
        run_sunny_day_test(
            "COMMIT;",
            Statement::CommitTransaction(CommitTransactionStatement {}),
        );
    }

    #[test]
    fn commit_transaction_with_transaction_keyword() {
        run_sunny_day_test(
            "COMMIT TRANSACTION;",
            Statement::CommitTransaction(CommitTransactionStatement {}),
        );
    }

    #[test]
    fn commit_transaction_end_keyword() {
        run_sunny_day_test(
            "END;",
            Statement::CommitTransaction(CommitTransactionStatement {}),
        );
    }

    #[test]
    fn commit_transaction_end_with_transaction_keyword() {
        run_sunny_day_test(
            "END TRANSACTION;",
            Statement::CommitTransaction(CommitTransactionStatement {}),
        );
    }

    #[test]
    fn commit_transaction_missing_semicolon() {
        run_sunny_day_test(
            "COMMIT",
            Statement::CommitTransaction(CommitTransactionStatement {}),
        );
    }

    #[test]
    fn commit_transaction_with_comment() {
        let sql = "COMMIT -- end transaction\n;";
        run_sunny_day_test(
            sql,
            Statement::CommitTransaction(CommitTransactionStatement {}),
        );
    }

    #[test]
    fn commit_transaction_with_unexpected_token() {
        run_rainy_day_test(
            "COMMIT EXTRA;",
            ParsingError::UnexpectedToken("EXTRA at position 7".into()),
        );
    }

    #[test]
    fn commit_transaction_with_invalid_syntax() {
        let sql = "COMMIT TRANSACT;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("TRANSACT at position 7".into()),
        );
    }

    #[test]
    fn commit_transaction_with_transaction_name() {
        run_rainy_day_test(
            "COMMIT TRANSACTION transaction_name;",
            ParsingError::UnexpectedToken("transaction_name at position 19".into()),
        );
    }

    #[test]
    fn multiple_commit_transaction_commands() {
        let sql = "COMMIT; END TRANSACTION;";
        let mut parser = Parser::from(sql);

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let first_expected_statement = Statement::CommitTransaction(CommitTransactionStatement {});
        assert_eq!(
            first_actual_statement, first_expected_statement,
            "Expected statement {:?}, got {:?}",
            first_expected_statement, first_actual_statement
        );

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let second_expected_statement = Statement::CommitTransaction(CommitTransactionStatement {});
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }
}
