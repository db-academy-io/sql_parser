use crate::{Keyword, Parser, ParsingError, ReleaseStatement, Statement};

use super::savepoint::SavepointStatementParser;

pub trait ReleaseStatementParser {
    fn parse_release_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> ReleaseStatementParser for Parser<'a> {
    fn parse_release_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_keyword(Keyword::Release)?;

        let _ = self.consume_keyword(Keyword::Savepoint);

        let savepoint_name = self.parse_savepoint_name()?;
        self.finalize_statement_parsing()?;
        Ok(Statement::Release(ReleaseStatement { savepoint_name }))
    }
}

#[cfg(test)]
mod release_statements_tests {
    use crate::ast::ReleaseStatement;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Parser, ParsingError, Statement};

    #[test]
    fn test_release_savepoint_basic() {
        let sql = "RELEASE SAVEPOINT sp_name;";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "sp_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_without_keyword() {
        let sql = "RELEASE sp_name;";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "sp_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_with_single_quoted_name() {
        let sql = "RELEASE SAVEPOINT 'sp_name';";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "'sp_name'".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_with_double_quoted_name() {
        let sql = "RELEASE SAVEPOINT \"sp_name\";";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "\"sp_name\"".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_missing_name() {
        let sql = "RELEASE SAVEPOINT;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_release_savepoint_missing_name_without_keyword() {
        let sql = "RELEASE;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_release_savepoint_unexpected_token() {
        let sql = "RELEASE SAVEPOINT 123;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("123".into()));
    }

    #[test]
    fn test_release_savepoint_with_reserved_keyword_as_name() {
        let sql = "RELEASE SAVEPOINT select;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("Select".into()));
    }

    #[test]
    fn test_release_savepoint_with_numeric_name_in_quotes() {
        let sql = "RELEASE SAVEPOINT '123';";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "'123'".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_with_special_chars_in_name() {
        let sql = "RELEASE SAVEPOINT '[email protected]!';";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "'[email protected]!'".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_unterminated_string() {
        let sql = "RELEASE SAVEPOINT 'sp_name;";
        run_rainy_day_test(
            sql,
            ParsingError::TokenizerError("UnterminatedLiteral: 'sp_name;".into()),
        );
    }

    #[test]
    fn test_release_savepoint_with_escaped_quotes_in_name() {
        let sql = "RELEASE SAVEPOINT 'sp''name';";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "'sp''name'".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_with_double_escaped_quotes_in_name() {
        let sql = "RELEASE SAVEPOINT \"sp\"\"name\";";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "\"sp\"\"name\"".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_with_backticks_name() {
        let sql = "RELEASE SAVEPOINT `sp_name`;";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "`sp_name`".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_missing_semicolon() {
        let sql = "RELEASE SAVEPOINT sp_name";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "sp_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_with_extra_tokens() {
        let sql = "RELEASE SAVEPOINT sp_name EXTRA;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("EXTRA".into()));
    }

    #[test]
    fn test_release_savepoint_case_insensitive() {
        let sql = "release savepoint sp_name;";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "sp_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_with_comment() {
        let sql = "RELEASE SAVEPOINT -- release savepoint\nsp_name;";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "sp_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_with_invalid_name() {
        let sql = "RELEASE SAVEPOINT 123invalid;";
        run_rainy_day_test(sql, ParsingError::TokenizerError("BadNumber".into()));
    }

    #[test]
    fn test_release_savepoint_with_reserved_keyword_as_name_in_quotes() {
        let sql = "RELEASE SAVEPOINT 'select';";
        run_sunny_day_test(
            sql,
            Statement::Release(ReleaseStatement {
                savepoint_name: "'select'".to_string(),
            }),
        );
    }

    #[test]
    fn test_release_savepoint_with_numeric_name_without_quotes() {
        let sql = "RELEASE SAVEPOINT 123;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("123".into()));
    }

    #[test]
    fn test_release_savepoint_multiple_commands() {
        let sql = "RELEASE SAVEPOINT sp1; RELEASE sp2;";
        let mut parser = Parser::from(sql);

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let first_expected_statement = Statement::Release(ReleaseStatement {
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
        let second_expected_statement = Statement::Release(ReleaseStatement {
            savepoint_name: "sp2".to_string(),
        });
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }
}
