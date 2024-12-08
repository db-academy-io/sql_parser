use crate::{Keyword, Parser, ParsingError, Statement, VacuumStatement};

pub trait VacuumStatementParser {
    /// Parses a VACUUM statement
    fn parse_vacuum_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> VacuumStatementParser for Parser<'a> {
    fn parse_vacuum_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Vacuum)?;

        // Check if we've got only 'VACUUM;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::Vacuum(VacuumStatement::default()));
        }

        let schema: Option<String> = if let Ok(id) = self.peek_as_id() {
            let schema = Some(id.to_string());
            // Consume the schema token
            self.consume_as_id()?;
            schema
        } else {
            None
        };

        let vacuum_statement = if self.consume_as_keyword(Keyword::Into).is_ok() {
            let value = self.peek_as_string()?;
            // Consume the schema token
            self.consume_token()?;
            VacuumStatement {
                schema_name: schema,
                file_name: Some(value),
            }
        } else {
            VacuumStatement {
                schema_name: schema,
                file_name: None,
            }
        };
        dbg!(&self.peek_token()?);
        self.finalize_statement_parsing()?;
        Ok(Statement::Vacuum(vacuum_statement))
    }
}

#[cfg(test)]
mod vacuum_statements_tests {
    use crate::ast::VacuumStatement;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Parser, ParsingError, Statement};

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
    fn test_vacuum_without_semicolon() {
        let sql = "VACUUM";
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
    fn test_vacuum_invalid_syntax() {
        let sql = "VACUUM INTO;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".to_string()));
    }

    #[test]
    fn test_vacuum_missing_filename() {
        let sql = "VACUUM INTO";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_vacuum_invalid_filename() {
        let sql = "VACUUM INTO backup.db;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(".".to_string()));
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
