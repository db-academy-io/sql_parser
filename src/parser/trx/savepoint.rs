use crate::{Keyword, Parser, ParsingError, SavepointStatement, Statement};

pub trait SavepointStatementParser {
    fn parse_savepoint_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_savepoint_name(&mut self) -> Result<String, ParsingError>;
}

impl<'a> SavepointStatementParser for Parser<'a> {
    fn parse_savepoint_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the SAVEPOINT keyword
        self.consume_as_keyword(Keyword::Savepoint)?;

        let savepoint_name = self.parse_savepoint_name()?;

        self.finalize_statement_parsing()?;
        Ok(Statement::Savepoint(SavepointStatement { savepoint_name }))
    }

    fn parse_savepoint_name(&mut self) -> Result<String, ParsingError> {
        if let Ok(id) = self.peek_as_id() {
            // Consume the id token
            self.consume_as_id()?;
            return Ok(id.to_string());
        }

        if let Ok(string) = self.peek_as_string() {
            // Consume the id token
            self.consume_as_string()?;
            return Ok(string.to_string());
        }

        let token = self.peek_token()?;
        Err(ParsingError::UnexpectedToken(token.to_string()))
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::ast::SavepointStatement;

    pub fn savepoint_statement() -> SavepointStatement {
        SavepointStatement {
            savepoint_name: "my_savepoint".to_string(),
        }
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
