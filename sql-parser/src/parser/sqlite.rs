use super::Parser;
use crate::{Keyword, ParsingError, Statement, TokenType, VacuumStatement};

pub trait SQLite3StatementParser {
    fn parse_vacuum_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> SQLite3StatementParser for Parser<'a> {
    fn parse_vacuum_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the VACUUM keyword
        self.consume_token()?;

        // Check if we've got only VACUUM; command
        if self.peek_as(TokenType::Semi).is_ok() {
            // Consume ';' token
            self.consume_token()?;
            return Ok(Statement::Vacuum(VacuumStatement::default()));
        }

        let mut schema: Option<String> = None;

        if let Ok(token) = self.peek_as_id() {
            schema = Some(token.to_string());
            // Consume the schema token
            self.consume_token()?;
        }

        // Check if we've got only VACUUM $SCHEMA; command
        if self.peek_as(TokenType::Semi).is_ok() {
            // Consume ';' token
            self.consume_token()?;
            return Ok(Statement::Vacuum(VacuumStatement {
                schema_name: schema,
                file_name: None,
            }));
        }

        // Parsing "INTO $filename;" statement
        // At this point we expect to get INTO keyword and the filename token
        let keyword = self.peek_as_keyword()?;
        if keyword != Keyword::Into {
            let current_token = self.peek_token()?;
            return Err(ParsingError::UnexpectedToken(current_token.to_string()));
        }

        // Consume the INTO keyword
        self.consume_token()?;

        let filename = self.peek_as_string()?;
        // Consume 'file_name' token
        self.consume_token()?;

        if self.peek_as(TokenType::Semi).is_ok() {
            // Consume ';' token
            self.consume_token()?;
            Ok(Statement::Vacuum(VacuumStatement {
                schema_name: schema,
                file_name: Some(filename),
            }))
        } else {
            let current_token = self.peek_token()?;
            Err(ParsingError::UnexpectedToken(current_token.to_string()))
        }
    }
}

#[cfg(test)]
mod vacuum_statement_parser {
    use crate::ast::VacuumStatement;
    use crate::{Parser, ParsingError, Statement};

    fn run_sunny_day_test(sql: &str, expected_statement: Statement) {
        let mut parser = Parser::from(sql);
        let actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        // Verify that the statements match
        assert_eq!(
            actual_statement, expected_statement,
            "Expected statement {:?}, got {:?}",
            expected_statement, actual_statement
        );
    }

    fn run_rainy_day_test(sql: &str, expected_error: ParsingError) {
        let mut parser = Parser::from(sql);
        let actual_error = parser
            .parse_statement()
            .expect_err("Expected Parsing Error, got parsed Statement");

        assert_eq!(
            expected_error, actual_error,
            "Expected error {:?}, got {:?}",
            expected_error, actual_error,
        );
    }

    #[test]
    fn test_vacuum_basic() {
        let sql = "VACUUM;";
        run_sunny_day_test(
            sql,
            Statement::Vacuum(VacuumStatement {
                schema_name: None,
                file_name: None,
            }),
        );
    }

    #[test]
    fn test_vacuum_with_schema() {
        let sql = "VACUUM main;";
        run_sunny_day_test(
            sql,
            Statement::Vacuum(VacuumStatement {
                schema_name: Some("main".to_string()),
                file_name: None,
            }),
        );
    }

    #[test]
    fn test_vacuum_into_file() {
        let sql = "VACUUM INTO 'backup.db';";
        run_sunny_day_test(
            sql,
            Statement::Vacuum(VacuumStatement {
                schema_name: None,
                file_name: Some("'backup.db'".to_string()),
            }),
        );
    }

    #[test]
    fn test_vacuum_schema_into_file() {
        let sql = "VACUUM main INTO 'backup.db';";
        run_sunny_day_test(
            sql,
            Statement::Vacuum(VacuumStatement {
                schema_name: Some("main".to_string()),
                file_name: Some("'backup.db'".to_string()),
            }),
        );
    }

    #[test]
    fn test_vacuum_missing_semicolon() {
        let sql = "VACUUM";
        run_rainy_day_test(sql, ParsingError::UnexpectedEOF);
    }

    #[test]
    fn test_vacuum_invalid_syntax() {
        let sql = "VACUUM INTO;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".to_string()));
    }

    #[test]
    fn test_vacuum_missing_filename() {
        let sql = "VACUUM INTO";
        run_rainy_day_test(sql, ParsingError::UnexpectedEOF);
    }

    #[test]
    fn test_vacuum_invalid_filename() {
        let sql = "VACUUM INTO backup.db;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("backup".to_string()));
    }

    #[test]
    fn test_vacuum_unexpected_token() {
        let sql = "VACUUM 123;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("123".into()));
    }

    #[test]
    fn test_vacuum_extra_tokens() {
        let sql = "VACUUM main INTO 'backup.db' extra;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("extra".to_string()));
    }

    #[test]
    fn test_vacuum_schema_missing_into() {
        let sql = "VACUUM main 'backup.db';";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("'backup.db'".to_string()),
        );
    }

    #[test]
    fn test_multiple_vacuum_commands() {
        let sql = "VACUUM; VACUUM main INTO 'backup.db';";

        let mut parser = Parser::from(sql);
        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        let first_expected_statement = Statement::Vacuum(VacuumStatement {
            schema_name: None,
            file_name: None,
        });

        // Verify that the statements match
        assert_eq!(
            first_actual_statement, first_expected_statement,
            "Expected statement {:?}, got {:?}",
            first_expected_statement, first_actual_statement
        );

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        let second_expected_statement = Statement::Vacuum(VacuumStatement {
            schema_name: Some("main".to_string()),
            file_name: Some("'backup.db'".to_string()),
        });

        // Verify that the statements match
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }
}
