use crate::parser::errors::ParsingError;
use crate::{Keyword, Parser, Statement, VacuumStatement};

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

        let schema: Option<String> = self.consume_as_id().ok();

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
        self.finalize_statement_parsing()?;
        Ok(Statement::Vacuum(vacuum_statement))
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::ast::VacuumStatement;

    pub fn vacuum_statement() -> VacuumStatement {
        VacuumStatement::default()
    }
}

#[cfg(test)]
mod vacuum_statements_tests {
    use crate::ast::VacuumStatement;
    use crate::parser::errors::ParsingError;
    use crate::parser::test_utils::{
        assert_statements_equal, run_rainy_day_test, run_sunny_day_test,
    };
    use crate::{Parser, Statement};

    use super::test_utils::vacuum_statement;

    #[test]
    fn test_vacuum_basic() {
        run_sunny_day_test("VACUUM;", Statement::Vacuum(vacuum_statement()));
    }

    #[test]
    fn test_vacuum_without_semicolon() {
        run_sunny_day_test("VACUUM", Statement::Vacuum(vacuum_statement()));
    }

    #[test]
    fn test_vacuum_with_schema() {
        let mut expected_statement = vacuum_statement();
        expected_statement.schema_name = Some("main".to_string());
        run_sunny_day_test("VACUUM main;", Statement::Vacuum(expected_statement));
    }

    #[test]
    fn test_vacuum_into_file() {
        let mut expected_statement = vacuum_statement();
        expected_statement.file_name = Some("'backup.db'".to_string());

        run_sunny_day_test(
            "VACUUM INTO 'backup.db';",
            Statement::Vacuum(expected_statement),
        );
    }

    #[test]
    fn test_vacuum_schema_into_file() {
        let mut expected_statement = vacuum_statement();
        expected_statement.schema_name = Some("main".to_string());
        expected_statement.file_name = Some("'backup.db'".to_string());

        run_sunny_day_test(
            "VACUUM main INTO 'backup.db';",
            Statement::Vacuum(expected_statement),
        );
    }

    #[test]
    fn test_vacuum_invalid_syntax() {
        run_rainy_day_test(
            "VACUUM INTO;",
            ParsingError::UnexpectedToken(";".to_string()),
        );
    }

    #[test]
    fn test_vacuum_missing_filename() {
        run_rainy_day_test("VACUUM INTO", ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_vacuum_invalid_filename() {
        run_rainy_day_test(
            "VACUUM INTO backup.db;",
            ParsingError::UnexpectedToken(".".to_string()),
        );
    }

    #[test]
    fn test_vacuum_unexpected_token() {
        run_rainy_day_test("VACUUM 123;", ParsingError::UnexpectedToken("123".into()));
    }

    #[test]
    fn test_vacuum_extra_tokens() {
        run_rainy_day_test(
            "VACUUM main INTO 'backup.db' extra;",
            ParsingError::UnexpectedToken("extra".to_string()),
        );
    }

    #[test]
    fn test_vacuum_schema_missing_into() {
        run_rainy_day_test(
            "VACUUM main 'backup.db';",
            ParsingError::UnexpectedToken("'backup.db'".to_string()),
        );
    }

    #[test]
    fn test_multiple_vacuum_commands() {
        let sql = "VACUUM; VACUUM main INTO 'backup.db';";

        let mut parser = Parser::from(sql);

        let first_expected_statement = Statement::Vacuum(vacuum_statement());
        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(first_actual_statement, first_expected_statement);

        let second_expected_statement = Statement::Vacuum(VacuumStatement {
            schema_name: Some("main".to_string()),
            file_name: Some("'backup.db'".to_string()),
        });

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(second_actual_statement, second_expected_statement);
    }
}
