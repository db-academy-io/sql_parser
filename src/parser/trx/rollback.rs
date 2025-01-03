use super::savepoint::SavepointStatementParser;
use crate::parser::errors::ParsingError;
use crate::{Keyword, Parser, RollbackTransactionStatement, Statement};

pub trait RollbackStatementParser {
    fn parse_rollback_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl RollbackStatementParser for Parser<'_> {
    fn parse_rollback_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Rollback)?;

        // Consume the optional TRANSACTION keyword
        let _ = self.consume_as_keyword(Keyword::Transaction);

        if self.consume_as_keyword(Keyword::To).is_ok() {
            let _ = self.consume_as_keyword(Keyword::Savepoint);
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

#[cfg(test)]
pub mod test_utils {
    use crate::ast::RollbackTransactionStatement;

    pub fn rollback_statement() -> RollbackTransactionStatement {
        RollbackTransactionStatement::default()
    }
}

#[cfg(test)]
mod rollback_statements_tests {
    use crate::ast::RollbackTransactionStatement;
    use crate::parser::errors::ParsingError;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Parser, Statement};

    #[test]
    fn rollback_transaction() {
        let sql = "ROLLBACK;";
        run_sunny_day_test(
            sql,
            Statement::RollbackTransaction(RollbackTransactionStatement::default()),
        );
    }

    #[test]
    fn rollback_transaction_with_transaction_keyword() {
        let sql = "ROLLBACK TRANSACTION;";
        run_sunny_day_test(
            sql,
            Statement::RollbackTransaction(RollbackTransactionStatement::default()),
        );
    }

    #[test]
    fn rollback_transaction_to_savepoint() {
        run_sunny_day_test(
            "ROLLBACK TO SAVEPOINT sp_name;",
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: Some("sp_name".to_string()),
            }),
        );

        run_sunny_day_test(
            "ROLLBACK TO sp_name;",
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: Some("sp_name".to_string()),
            }),
        );
    }

    #[test]
    fn rollback_transaction_with_transaction_keyword_to_savepoint() {
        let sql = "ROLLBACK TRANSACTION TO SAVEPOINT sp_name;";
        run_sunny_day_test(
            sql,
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: Some("sp_name".to_string()),
            }),
        );

        let sql = "ROLLBACK TRANSACTION TO sp_name;";
        run_sunny_day_test(
            sql,
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: Some("sp_name".to_string()),
            }),
        );
    }

    #[test]
    fn rollback_transaction_with_unexpected_token() {
        let sql = "ROLLBACK EXTRA;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("EXTRA at position 9".into()),
        );
    }

    #[test]
    fn rollback_transaction_to_missing_savepoint_name() {
        let sql = "ROLLBACK TO;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("; at position 11".into()),
        );
    }

    #[test]
    fn rollback_transaction_to_unexpected_token() {
        let sql = "ROLLBACK TO 123;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("123 at position 12".into()),
        );
    }

    #[test]
    fn rollback_transaction_to_savepoint_with_single_quoted_name() {
        let sql = "ROLLBACK TO SAVEPOINT 'sp_name';";
        run_sunny_day_test(
            sql,
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: Some("'sp_name'".to_string()),
            }),
        );
    }

    #[test]
    fn rollback_transaction_to_savepoint_with_double_quoted_name() {
        let sql = "ROLLBACK TO SAVEPOINT \"sp_name\";";
        run_sunny_day_test(
            sql,
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: Some("\"sp_name\"".to_string()),
            }),
        );
    }

    #[test]
    fn rollback_transaction_to_savepoint_with_numeric_name_in_quotes() {
        let sql = "ROLLBACK TO SAVEPOINT '123';";
        run_sunny_day_test(
            sql,
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: Some("'123'".to_string()),
            }),
        );
    }

    #[test]
    fn rollback_transaction_to_savepoint_with_special_chars_in_name() {
        let sql = "ROLLBACK TO SAVEPOINT '[email protected]!';";
        run_sunny_day_test(
            sql,
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: Some("'[email protected]!'".to_string()),
            }),
        );
    }

    #[test]
    fn rollback_transaction_unterminated_string() {
        let sql = "ROLLBACK TO SAVEPOINT 'sp_name;";
        run_rainy_day_test(
            sql,
            ParsingError::TokenizerError("UnterminatedLiteral: 'sp_name;".into()),
        );
    }

    #[test]
    fn rollback_transaction_with_escaped_quotes_in_name() {
        let sql = "ROLLBACK TO SAVEPOINT 'sp''name';";
        run_sunny_day_test(
            sql,
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: Some("'sp''name'".to_string()),
            }),
        );
    }

    #[test]
    fn rollback_transaction_with_double_escaped_quotes_in_name() {
        let sql = "ROLLBACK TO SAVEPOINT \"sp\"\"name\";";
        run_sunny_day_test(
            sql,
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: Some("\"sp\"\"name\"".to_string()),
            }),
        );
    }

    #[test]
    fn rollback_transaction_with_backticks_name() {
        let sql = "ROLLBACK TO SAVEPOINT `sp_name`;";
        run_sunny_day_test(
            sql,
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: Some("`sp_name`".to_string()),
            }),
        );
    }

    #[test]
    fn rollback_transaction_case_insensitive() {
        run_sunny_day_test(
            "rollback transaction;",
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: None,
            }),
        );
    }

    #[test]
    fn rollback_transaction_with_comment() {
        let sql = "ROLLBACK -- rollback transaction\n;";
        run_sunny_day_test(
            sql,
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: None,
            }),
        );
    }

    #[test]
    fn rollback_transaction_with_invalid_syntax_extra_token() {
        run_rainy_day_test(
            "ROLLBACK TO SAVEPOINT sp_name EXTRA;",
            ParsingError::UnexpectedToken("EXTRA at position 30".into()),
        );
    }

    #[test]
    fn rollback_transaction_with_invalid_savepoint_name() {
        let sql = "ROLLBACK TO SAVEPOINT 123invalid;";
        run_rainy_day_test(sql, ParsingError::TokenizerError("BadNumber".into()));
    }

    #[test]
    fn rollback_transaction_with_savepoint_missing_name() {
        let sql = "ROLLBACK TO SAVEPOINT;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("; at position 21".into()),
        );
    }

    #[test]
    fn rollback_transaction_with_missing_to_keyword() {
        let sql = "ROLLBACK SAVEPOINT sp_name;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Savepoint at position 9".into()),
        );
    }

    #[test]
    fn multiple_rollback_transaction_commands() {
        let sql = "ROLLBACK; ROLLBACK TO SAVEPOINT sp_name;";
        let mut parser = Parser::from(sql);

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let first_expected_statement =
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: None,
            });
        assert_eq!(
            first_actual_statement, first_expected_statement,
            "Expected statement {:?}, got {:?}",
            first_expected_statement, first_actual_statement
        );

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let second_expected_statement =
            Statement::RollbackTransaction(RollbackTransactionStatement {
                savepoint_name: Some("sp_name".to_string()),
            });
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }
}
