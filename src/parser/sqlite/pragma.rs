use crate::parser::errors::ParsingError;
use crate::{Keyword, Parser, PragmaStatement, Statement, TokenType};

pub trait PragmaStatementParser {
    /// Parses a PRAGMA statement
    fn parse_pragma_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a pragma value
    fn parse_pragma_value(&mut self) -> Result<String, ParsingError>;
}

impl<'a> PragmaStatementParser for Parser<'a> {
    fn parse_pragma_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Pragma)?;

        let mut pragma_name: String = self.peek_as_string()?;
        // Consume the pragma name token
        self.consume_token()?;

        let mut schema_name = None;

        if self.consume_as(TokenType::Dot).is_ok() {
            // The pragma name is the next name after the '.' token
            schema_name = Some(pragma_name);
            pragma_name = self.peek_as_string()?;
            // Consume the pragma name token
            self.consume_token()?;
        }

        // Check if we've got 'PRAGMA $pragma_name;' or
        // 'PRAGMA $schema_name.$pragma_name;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::Pragma(PragmaStatement {
                schema_name,
                pragma_name,
                pragma_value: None,
            }));
        }

        let token = self.peek_token()?;
        match token.token_type {
            TokenType::Equals => {
                self.consume_as(TokenType::Equals)?;

                let pragma_value = Some(self.parse_pragma_value()?);

                self.finalize_statement_parsing()?;
                Ok(Statement::Pragma(PragmaStatement {
                    schema_name,
                    pragma_name,
                    pragma_value,
                }))
            }
            TokenType::LeftParen => {
                self.consume_as(TokenType::LeftParen)?;

                let pragma_value = Some(self.parse_pragma_value()?);
                let statement = Ok(Statement::Pragma(PragmaStatement {
                    schema_name,
                    pragma_name,
                    pragma_value,
                }));
                if self.peek_as(TokenType::RightParen).is_ok() {
                    self.consume_as(TokenType::RightParen)?;
                }
                self.finalize_statement_parsing()?;

                statement
            }
            _ => Err(ParsingError::UnexpectedToken(token.to_string())),
        }
    }

    fn parse_pragma_value(&mut self) -> Result<String, ParsingError> {
        if let Ok(value) = self.parse_signed_number() {
            return Ok(value);
        }

        let value = self.peek_as_string()?;
        self.consume_token()?;

        Ok(value)
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::ast::PragmaStatement;

    pub fn pragma_statement() -> PragmaStatement {
        PragmaStatement {
            schema_name: None,
            pragma_name: "cache_size".to_string(),
            pragma_value: None,
        }
    }
}

#[cfg(test)]
mod pragma_statements_tests {
    use crate::parser::errors::ParsingError;
    use crate::parser::test_utils::{
        assert_statements_equal, run_rainy_day_test, run_sunny_day_test,
    };
    use crate::{Parser, PragmaStatement, Statement};

    use super::test_utils::pragma_statement;

    #[test]
    fn test_pragma_basic() {
        run_sunny_day_test("PRAGMA cache_size;", Statement::Pragma(pragma_statement()));
    }

    #[test]
    fn test_pragma_with_schema() {
        let mut expected = pragma_statement();
        expected.schema_name = Some("main".to_string());

        run_sunny_day_test("PRAGMA main.cache_size;", Statement::Pragma(expected));
    }

    #[test]
    fn test_pragma_with_value() {
        let mut expected = pragma_statement();
        expected.pragma_value = Some("2000".to_string());

        run_sunny_day_test("PRAGMA cache_size = 2000;", Statement::Pragma(expected));
    }

    #[test]
    fn test_pragma_with_schema_and_value() {
        let mut expected = pragma_statement();
        expected.schema_name = Some("main".to_string());
        expected.pragma_value = Some("2000".to_string());

        run_sunny_day_test(
            "PRAGMA main.cache_size = 2000;",
            Statement::Pragma(expected),
        );
    }

    #[test]
    fn test_pragma_with_string_value() {
        let expected = PragmaStatement {
            schema_name: Some("main".to_string()),
            pragma_name: "journal_mode".to_string(),
            pragma_value: Some("WAL".to_string()),
        };

        run_sunny_day_test(
            "PRAGMA main.journal_mode = WAL;",
            Statement::Pragma(expected),
        );
    }

    #[test]
    fn test_pragma_with_quoted_value() {
        let expected = PragmaStatement {
            schema_name: Some("main".to_string()),
            pragma_name: "journal_mode".to_string(),
            pragma_value: Some("'WAL'".to_string()),
        };

        run_sunny_day_test(
            "PRAGMA main.journal_mode = 'WAL';",
            Statement::Pragma(expected),
        );
    }

    #[test]
    fn test_pragma_without_semicolon() {
        run_sunny_day_test("PRAGMA cache_size", Statement::Pragma(pragma_statement()));
    }

    #[test]
    fn test_pragma_with_invalid_schema() {
        let sql = "PRAGMA invalid..cache_size;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(".".into()));
    }

    #[test]
    fn test_pragma_with_multiple_dots() {
        let sql = "PRAGMA main.db.cache_size;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(".".into()));
    }

    #[test]
    fn test_pragma_missing_name() {
        let sql = "PRAGMA;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_pragma_missing_value_after_equals() {
        let sql = "PRAGMA cache_size =;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_pragma_set_string_value() {
        let mut expected = pragma_statement();
        expected.pragma_name = "encoding".to_string();
        expected.pragma_value = Some("'UTF-8'".to_string());

        run_sunny_day_test("PRAGMA encoding = 'UTF-8';", Statement::Pragma(expected));
    }

    #[test]
    fn test_pragma_call_syntax() {
        let mut expected = pragma_statement();
        expected.pragma_value = Some("1000".to_string());

        run_sunny_day_test("PRAGMA cache_size(1000);", Statement::Pragma(expected));
    }

    #[test]
    fn test_pragma_call_syntax_with_schema() {
        let mut expected = pragma_statement();
        expected.schema_name = Some("main".to_string());
        expected.pragma_value = Some("1000".to_string());

        run_sunny_day_test("PRAGMA main.cache_size(1000);", Statement::Pragma(expected));
    }

    #[test]
    fn test_pragma_with_single_quoted_value() {
        let mut expected = pragma_statement();
        expected.pragma_value = Some("'1000'".to_string());

        run_sunny_day_test("PRAGMA cache_size = '1000';", Statement::Pragma(expected));
    }

    #[test]
    fn test_pragma_with_double_quoted_value() {
        let mut expected = pragma_statement();
        expected.pragma_value = Some("\"1000\"".to_string());

        run_sunny_day_test("PRAGMA cache_size = \"1000\";", Statement::Pragma(expected));
    }

    #[test]
    fn test_pragma_with_reserved_keyword_as_name() {
        run_rainy_day_test(
            "PRAGMA select;",
            ParsingError::UnexpectedToken("Select".into()),
        );
    }

    #[test]
    fn test_pragma_with_numeric_pragma_name() {
        let sql = "PRAGMA 123;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("123".into()));
    }

    #[test]
    fn test_pragma_with_invalid_syntax_extra_token() {
        let sql = "PRAGMA cache_size = 1000 EXTRA;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("EXTRA".into()));
    }

    #[test]
    fn test_pragma_with_unterminated_string_value() {
        let sql = "PRAGMA cache_size = '1000;";
        run_rainy_day_test(
            sql,
            ParsingError::TokenizerError("UnterminatedLiteral: '1000;".into()),
        );
    }

    #[test]
    fn test_pragma_with_special_chars_in_value() {
        let mut expected = pragma_statement();
        expected.pragma_value = Some("'[email protected]!'".to_string());

        run_sunny_day_test(
            "PRAGMA cache_size = '[email protected]!';",
            Statement::Pragma(expected),
        );
    }

    #[test]
    fn test_pragma_with_numeric_value() {
        let mut expected = pragma_statement();
        expected.pragma_value = Some("123".to_string());

        run_sunny_day_test("PRAGMA cache_size = 123;", Statement::Pragma(expected));
    }

    #[test]
    fn test_pragma_with_positive_numeric_value() {
        let mut expected = pragma_statement();
        expected.pragma_value = Some("123".to_string());

        run_sunny_day_test("PRAGMA cache_size = +123;", Statement::Pragma(expected));
    }

    #[test]
    fn test_pragma_with_negative_numeric_value() {
        let mut expected = pragma_statement();
        expected.pragma_value = Some("-123".to_string());

        run_sunny_day_test("PRAGMA cache_size = -123;", Statement::Pragma(expected));
    }

    #[test]
    fn test_pragma_with_reserved_keyword_as_value() {
        let sql = "PRAGMA cache_size = select;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("Select".into()));
    }

    #[test]
    fn test_pragma_with_no_value_after_equal() {
        let sql = "PRAGMA cache_size =;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_pragma_with_no_value_in_parentheses() {
        let sql = "PRAGMA cache_size();";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(")".into()));
    }

    #[test]
    fn test_pragma_with_schema_missing_pragma_name() {
        let sql = "PRAGMA main.;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_pragma_missing_pragma_name() {
        let sql = "PRAGMA;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_pragma_with_unexpected_token_after_parentheses() {
        let sql = "PRAGMA cache_size(1000) EXTRA;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("EXTRA".into()));
    }

    #[test]
    fn test_multiple_pragma_commands() {
        let sql = "PRAGMA cache_size = 1000; PRAGMA encoding = 'UTF-8';";
        let mut parser = Parser::from(sql);

        let first_expected_statement = Statement::Pragma(PragmaStatement {
            schema_name: None,
            pragma_name: "cache_size".to_string(),
            pragma_value: Some("1000".to_string()),
        });

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(first_actual_statement, first_expected_statement);

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        let second_expected_statement = Statement::Pragma(PragmaStatement {
            schema_name: None,
            pragma_name: "encoding".to_string(),
            pragma_value: Some("'UTF-8'".to_string()),
        });
        assert_statements_equal(second_actual_statement, second_expected_statement);
    }

    #[test]
    fn test_pragma_with_comment() {
        let mut expected = pragma_statement();
        expected.pragma_value = Some("1000".to_string());

        run_sunny_day_test(
            "PRAGMA /* set cache size */ cache_size = 1000; ",
            Statement::Pragma(expected),
        );
    }
}
