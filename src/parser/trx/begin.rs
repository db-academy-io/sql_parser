use crate::parser::ParsingError;
use crate::{BeginTransactionStatement, Keyword, Parser, Statement, TransactionType};

pub trait BeginStatementParser {
    fn parse_begin_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl BeginStatementParser for Parser<'_> {
    fn parse_begin_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Begin)?;

        let mut statement = BeginTransactionStatement::default();

        // Check if we've got only 'BEGIN;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::BeginTransaction(statement));
        }

        if self.consume_as_keyword(Keyword::Deferred).is_ok() {
            statement.transaction_type = Some(TransactionType::Deferred);
        } else if self.consume_as_keyword(Keyword::Immediate).is_ok() {
            statement.transaction_type = Some(TransactionType::Immediate);
        } else if self.consume_as_keyword(Keyword::Exclusive).is_ok() {
            statement.transaction_type = Some(TransactionType::Exclusive);
        }

        // Consume the optional TRANSACTION keyword
        let _ = self.consume_as_keyword(Keyword::Transaction);

        self.finalize_statement_parsing()?;
        Ok(Statement::BeginTransaction(statement))
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::BeginTransactionStatement;

    pub fn begin_statement() -> BeginTransactionStatement {
        BeginTransactionStatement {
            transaction_type: None,
        }
    }
}

#[cfg(test)]
mod begin_statements_tests {
    use crate::parser::errors::ParsingError;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{BeginTransactionStatement, Parser, Statement, TransactionType};

    #[test]
    fn begin_transaction() {
        run_sunny_day_test(
            "BEGIN;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: None,
            }),
        );
    }

    #[test]
    fn begin_with_transaction_keyword() {
        run_sunny_day_test(
            "BEGIN TRANSACTION;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: None,
            }),
        );
    }

    #[test]
    fn begin_transaction_deferred() {
        run_sunny_day_test(
            "BEGIN DEFERRED;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: Some(TransactionType::Deferred),
            }),
        );
    }

    #[test]
    fn begin_transaction_immediate() {
        run_sunny_day_test(
            "BEGIN IMMEDIATE;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: Some(TransactionType::Immediate),
            }),
        );
    }

    #[test]
    fn begin_transaction_exclusive() {
        run_sunny_day_test(
            "BEGIN EXCLUSIVE;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: Some(TransactionType::Exclusive),
            }),
        );
    }

    #[test]
    fn begin_transaction_deferred_with_transaction_keyword() {
        run_sunny_day_test(
            "BEGIN DEFERRED TRANSACTION;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: Some(TransactionType::Deferred),
            }),
        );
    }

    #[test]
    fn begin_transaction_immediate_with_transaction_keyword() {
        run_sunny_day_test(
            "BEGIN IMMEDIATE TRANSACTION;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: Some(TransactionType::Immediate),
            }),
        );
    }

    #[test]
    fn begin_transaction_exclusive_with_transaction_keyword() {
        run_sunny_day_test(
            "BEGIN EXCLUSIVE TRANSACTION;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: Some(TransactionType::Exclusive),
            }),
        );
    }

    #[test]
    fn begin_transaction_missing_semicolon() {
        run_sunny_day_test(
            "BEGIN",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: None,
            }),
        );
    }

    #[test]
    fn begin_transaction_with_unexpected_token() {
        let sql = "BEGIN UNKNOWN;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("UNKNOWN at position 6".into()),
        );
    }

    #[test]
    fn begin_transaction_with_invalid_transaction_type() {
        let sql = "BEGIN INVALID TRANSACTION;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("INVALID at position 6".into()),
        );
    }

    #[test]
    fn begin_transaction_with_extra_tokens() {
        let sql = "BEGIN TRANSACTION EXTRA;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("EXTRA at position 18".into()),
        );
    }

    #[test]
    fn begin_transaction_case_insensitive() {
        run_sunny_day_test(
            "begin transaction;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: None,
            }),
        );
    }

    #[test]
    fn begin_transaction_with_comment() {
        run_sunny_day_test(
            "BEGIN -- Start transaction\n;",
            Statement::BeginTransaction(BeginTransactionStatement {
                transaction_type: None,
            }),
        );
    }

    #[test]
    fn begin_transaction_with_multiple_transaction_types() {
        let sql = "BEGIN DEFERRED IMMEDIATE;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Immediate at position 15".into()),
        );
    }

    #[test]
    fn begin_transaction_with_transaction_name() {
        run_rainy_day_test(
            "BEGIN TRANSACTION transaction_name;",
            ParsingError::UnexpectedToken("transaction_name at position 18".into()),
        );
    }

    #[test]
    fn begin_transaction_with_keyword() {
        run_rainy_day_test(
            "BEGIN COMMIT;",
            ParsingError::UnexpectedToken("Commit at position 6".into()),
        );

        run_rainy_day_test(
            "BEGIN ROLLBACK;",
            ParsingError::UnexpectedToken("Rollback at position 6".into()),
        );
    }

    #[test]
    fn multiple_begin_transaction_commands() {
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
