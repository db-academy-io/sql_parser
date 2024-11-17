use super::Parser;
use crate::{
    BeginTransactionStatement, CommitTransactionStatement, Keyword, ParsingError,
    SavepointStatement, Statement, TransactionType,
};

pub trait TransactionStatementParser {
    fn parse_begin_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_commit_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_rollback_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_release_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_savepoint_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> TransactionStatementParser for Parser<'a> {
    fn parse_begin_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the BEGIN keyword
        self.consume_token()?;

        let mut statement = BeginTransactionStatement::default();

        // Check if we've got only 'BEGIN;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::BeginTransaction(statement));
        }

        if let Ok(keyword) = self.peek_as_keyword() {
            match keyword {
                Keyword::Deferred => {
                    statement.transaction_type = Some(TransactionType::Deferred);
                }
                Keyword::Immediate => {
                    statement.transaction_type = Some(TransactionType::Immediate);
                }
                Keyword::Exclusive => {
                    statement.transaction_type = Some(TransactionType::Exclusive);
                }
                Keyword::Transaction => {
                    // Consume the TRANSACTION keyword
                    self.consume_token()?;

                    self.finalize_statement_parsing()?;
                    return Ok(Statement::BeginTransaction(statement));
                }
                _ => {
                    return Err(ParsingError::UnexpectedToken(
                        self.peek_token()?.to_string(),
                    ))
                }
            }
            // Consume transaction type token
            self.consume_token()?;
        }

        // Check if we've got only BEGIN $TYPE; command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::BeginTransaction(statement));
        }

        // Expected the TRANSACTION keyword
        let keyword = self.peek_as_keyword()?;
        if keyword != Keyword::Transaction {
            let current_token = self.peek_token()?;
            return Err(ParsingError::UnexpectedToken(current_token.to_string()));
        }
        // Consume the TRANSACTION keyword
        self.consume_token()?;

        self.finalize_statement_parsing()?;
        Ok(Statement::BeginTransaction(statement))
    }

    fn parse_commit_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the COMMIT OR END keyword
        self.consume_token()?;

        // Check if we've got only 'COMMIT;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::CommitTransaction(CommitTransactionStatement));
        }

        // Expected the TRANSACTION keyword
        let keyword = self.peek_as_keyword()?;
        if keyword != Keyword::Transaction {
            let current_token = self.peek_token()?;
            return Err(ParsingError::UnexpectedToken(current_token.to_string()));
        }
        // Consume the TRANSACTION keyword
        self.consume_token()?;

        self.finalize_statement_parsing()?;
        Ok(Statement::CommitTransaction(CommitTransactionStatement))
    }

    fn parse_rollback_statement(&mut self) -> Result<Statement, ParsingError> {
        todo!()
    }

    fn parse_release_statement(&mut self) -> Result<Statement, ParsingError> {
        todo!()
    }

    fn parse_savepoint_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the SAVEPOINT keyword
        self.consume_token()?;

        if let Ok(id) = self.peek_as_id() {
            // Consume the id token
            self.consume_token()?;

            self.finalize_statement_parsing()?;
            return Ok(Statement::Savepoint(SavepointStatement {
                savepoint_name: id.to_string(),
            }));
        }

        if let Ok(string) = self.peek_as_string() {
            // Consume the string token
            self.consume_token()?;

            self.finalize_statement_parsing()?;
            return Ok(Statement::Savepoint(SavepointStatement {
                savepoint_name: string.to_string(),
            }));
        }

        let token = self.peek_token()?;
        Err(ParsingError::UnexpectedToken(token.to_string()))
    }
}

#[cfg(test)]
mod begin_statements_tests {
    use crate::ast::BeginTransactionStatement;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Parser, ParsingError, Statement, TransactionType};

    #[test]
    fn test_begin_transaction_basic() {
        run_sunny_day_test(
            "BEGIN;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: None,
            }),
        );
    }

    #[test]
    fn test_begin_transaction_with_transaction_keyword() {
        run_sunny_day_test(
            "BEGIN TRANSACTION;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: None,
            }),
        );
    }

    #[test]
    fn test_begin_transaction_deferred() {
        run_sunny_day_test(
            "BEGIN DEFERRED;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: Some(TransactionType::Deferred),
            }),
        );
    }

    #[test]
    fn test_begin_transaction_immediate() {
        run_sunny_day_test(
            "BEGIN IMMEDIATE;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: Some(TransactionType::Immediate),
            }),
        );
    }

    #[test]
    fn test_begin_transaction_exclusive() {
        run_sunny_day_test(
            "BEGIN EXCLUSIVE;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: Some(TransactionType::Exclusive),
            }),
        );
    }

    #[test]
    fn test_begin_transaction_deferred_with_transaction_keyword() {
        run_sunny_day_test(
            "BEGIN DEFERRED TRANSACTION;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: Some(TransactionType::Deferred),
            }),
        );
    }

    #[test]
    fn test_begin_transaction_immediate_with_transaction_keyword() {
        run_sunny_day_test(
            "BEGIN IMMEDIATE TRANSACTION;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: Some(TransactionType::Immediate),
            }),
        );
    }

    #[test]
    fn test_begin_transaction_exclusive_with_transaction_keyword() {
        run_sunny_day_test(
            "BEGIN EXCLUSIVE TRANSACTION;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: Some(TransactionType::Exclusive),
            }),
        );
    }

    #[test]
    fn test_begin_transaction_missing_semicolon() {
        run_sunny_day_test(
            "BEGIN",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: None,
            }),
        );
    }

    #[test]
    fn test_begin_transaction_with_unexpected_token() {
        let sql = "BEGIN UNKNOWN;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("UNKNOWN".into()));
    }

    #[test]
    fn test_begin_transaction_with_invalid_transaction_type() {
        let sql = "BEGIN INVALID TRANSACTION;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("INVALID".into()));
    }

    #[test]
    fn test_begin_transaction_with_extra_tokens() {
        let sql = "BEGIN TRANSACTION EXTRA;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("EXTRA".into()));
    }

    #[test]
    fn test_begin_transaction_case_insensitive() {
        run_sunny_day_test(
            "begin transaction;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: None,
            }),
        );
    }

    #[test]
    fn test_begin_transaction_with_comment() {
        run_sunny_day_test(
            "BEGIN -- Start transaction\n;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: None,
            }),
        );
    }

    #[test]
    fn test_begin_transaction_with_multiple_transaction_types() {
        let sql = "BEGIN DEFERRED IMMEDIATE;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("Immediate".into()));
    }

    #[test]
    fn test_begin_transaction_with_transaction_name() {
        run_rainy_day_test(
            "BEGIN TRANSACTION transaction_name;",
            ParsingError::UnexpectedToken("transaction_name".into()),
        );
    }

    #[test]
    fn test_begin_transaction_with_keyword() {
        run_rainy_day_test(
            "BEGIN COMMIT;",
            ParsingError::UnexpectedToken("Commit".into()),
        );

        run_rainy_day_test(
            "BEGIN ROLLBACK;",
            ParsingError::UnexpectedToken("Rollback".into()),
        );
    }

    #[test]
    fn test_multiple_begin_transaction_commands() {
        let sql = "BEGIN; BEGIN IMMEDIATE TRANSACTION;";
        let mut parser = Parser::from(sql);

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let first_expected_statement = Statement::BeginTransaction(BeginTransactionStatement {
            transaction_type: None,
        });
        assert_eq!(
            first_actual_statement, first_expected_statement,
            "Expected statement {:?}, got {:?}",
            first_expected_statement, first_actual_statement
        );

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let second_expected_statement = Statement::BeginTransaction(BeginTransactionStatement {
            transaction_type: Some(TransactionType::Immediate),
        });
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }
}

#[cfg(test)]
mod commit_statements_tests {
    use crate::ast::CommitTransactionStatement;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Parser, ParsingError, Statement};

    #[test]
    fn test_commit_transaction_basic() {
        run_sunny_day_test(
            "COMMIT;",
            Statement::CommitTransaction(CommitTransactionStatement {}),
        );
    }

    #[test]
    fn test_commit_transaction_with_transaction_keyword() {
        run_sunny_day_test(
            "COMMIT TRANSACTION;",
            Statement::CommitTransaction(CommitTransactionStatement {}),
        );
    }

    #[test]
    fn test_commit_transaction_end_keyword() {
        run_sunny_day_test(
            "END;",
            Statement::CommitTransaction(CommitTransactionStatement {}),
        );
    }

    #[test]
    fn test_commit_transaction_end_with_transaction_keyword() {
        run_sunny_day_test(
            "END TRANSACTION;",
            Statement::CommitTransaction(CommitTransactionStatement {}),
        );
    }

    #[test]
    fn test_commit_transaction_missing_semicolon() {
        run_sunny_day_test(
            "COMMIT",
            Statement::CommitTransaction(CommitTransactionStatement {}),
        );
    }

    #[test]
    fn test_commit_transaction_with_comment() {
        let sql = "COMMIT -- end transaction\n;";
        run_sunny_day_test(
            sql,
            Statement::CommitTransaction(CommitTransactionStatement {}),
        );
    }

    #[test]
    fn test_commit_transaction_with_unexpected_token() {
        run_rainy_day_test(
            "COMMIT EXTRA;",
            ParsingError::UnexpectedToken("EXTRA".into()),
        );
    }

    #[test]
    fn test_commit_transaction_with_invalid_syntax() {
        let sql = "COMMIT TRANSACT;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("TRANSACT".into()));
    }

    #[test]
    fn test_commit_transaction_with_transaction_name() {
        run_rainy_day_test(
            "COMMIT TRANSACTION transaction_name;",
            ParsingError::UnexpectedToken("transaction_name".into()),
        );
    }

    #[test]
    fn test_multiple_commit_transaction_commands() {
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

#[cfg(test)]
mod savepoint_statements_tests {
    use crate::ast::SavepointStatement;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Parser, ParsingError, Statement};

    #[test]
    fn test_savepoint_basic() {
        let sql = "SAVEPOINT sp_name;";
        run_sunny_day_test(
            sql,
            Statement::Savepoint(SavepointStatement {
                savepoint_name: "sp_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_savepoint_with_single_quoted_name() {
        let sql = "SAVEPOINT 'sp_name';";
        run_sunny_day_test(
            sql,
            Statement::Savepoint(SavepointStatement {
                savepoint_name: "'sp_name'".to_string(),
            }),
        );
    }

    #[test]
    fn test_savepoint_with_double_quoted_name() {
        let sql = "SAVEPOINT \"sp_name\";";
        run_sunny_day_test(
            sql,
            Statement::Savepoint(SavepointStatement {
                savepoint_name: "\"sp_name\"".to_string(),
            }),
        );
    }

    #[test]
    fn test_savepoint_missing_name() {
        let sql = "SAVEPOINT;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_savepoint_unexpected_token() {
        let sql = "SAVEPOINT 123;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("123".into()));
    }

    #[test]
    fn test_savepoint_reserved_keyword_as_name() {
        let sql = "SAVEPOINT select;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("Select".into()));
    }

    #[test]
    fn test_savepoint_with_numeric_name_in_quotes() {
        let sql = "SAVEPOINT '123';";
        run_sunny_day_test(
            sql,
            Statement::Savepoint(SavepointStatement {
                savepoint_name: "'123'".to_string(),
            }),
        );
    }

    #[test]
    fn test_savepoint_with_special_chars_in_name() {
        let sql = "SAVEPOINT '[email protected]!';";
        run_sunny_day_test(
            sql,
            Statement::Savepoint(SavepointStatement {
                savepoint_name: "'[email protected]!'".to_string(),
            }),
        );
    }

    #[test]
    fn test_savepoint_unterminated_string() {
        let sql = "SAVEPOINT 'sp_name;";
        run_rainy_day_test(
            sql,
            ParsingError::TokenizerError("UnterminatedLiteral: 'sp_name;".into()),
        );
    }

    #[test]
    fn test_savepoint_with_escaped_quotes_in_name() {
        let sql = "SAVEPOINT 'sp''name';";
        run_sunny_day_test(
            sql,
            Statement::Savepoint(SavepointStatement {
                savepoint_name: "'sp''name'".to_string(),
            }),
        );
    }

    #[test]
    fn test_savepoint_with_double_escaped_quotes_in_name() {
        let sql = "SAVEPOINT \"sp\"\"name\";";
        run_sunny_day_test(
            sql,
            Statement::Savepoint(SavepointStatement {
                savepoint_name: "\"sp\"\"name\"".to_string(),
            }),
        );
    }

    #[test]
    fn test_savepoint_with_backticks_name() {
        let sql = "SAVEPOINT `sp_name`;";
        run_sunny_day_test(
            sql,
            Statement::Savepoint(SavepointStatement {
                savepoint_name: "`sp_name`".to_string(),
            }),
        );
    }

    #[test]
    fn test_savepoint_missing_semicolon() {
        let sql = "SAVEPOINT sp_name";
        run_sunny_day_test(
            sql,
            Statement::Savepoint(SavepointStatement {
                savepoint_name: "sp_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_savepoint_with_extra_tokens() {
        let sql = "SAVEPOINT sp_name EXTRA;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("EXTRA".into()));
    }

    #[test]
    fn test_multiple_savepoint_commands() {
        let sql = "SAVEPOINT sp1; SAVEPOINT 'sp2';";
        let mut parser = Parser::from(sql);

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let first_expected_statement = Statement::Savepoint(SavepointStatement {
            savepoint_name: "sp1".to_string(),
        });
        assert_eq!(
            first_actual_statement, first_expected_statement,
            "Expected statement {:?}, got {:?}",
            first_expected_statement, first_actual_statement
        );

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let second_expected_statement = Statement::Savepoint(SavepointStatement {
            savepoint_name: "'sp2'".to_string(),
        });
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }

    #[test]
    fn test_savepoint_with_invalid_name() {
        let sql = "SAVEPOINT 123invalid;";
        run_rainy_day_test(sql, ParsingError::TokenizerError("BadNumber".into()));
    }
}
