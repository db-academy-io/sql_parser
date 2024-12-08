use crate::{AnalyzeStatement, Keyword, Parser, ParsingError, Statement, TokenType};

use super::sqlite3_name::SQLite3NameParser;

pub trait AnalyzeStatementParser {
    /// Parses an ANALYZE statement
    fn parse_analyze_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> AnalyzeStatementParser for Parser<'a> {
    fn parse_analyze_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Analyze)?;

        // Check if we've got only 'ANALYZE;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::Analyze(AnalyzeStatement::default()));
        }
        let schema_name = self.parse_sqlite3_name()?;

        // Check if we've got only 'ANALYZE $SCHEMA;' command
        if self.finalize_statement_parsing().is_ok() {
            // the schema name takes precedence over table or index names
            return Ok(Statement::Analyze(AnalyzeStatement {
                schema_name: Some(schema_name),
                table_or_index_name: None,
            }));
        }

        let mut table_or_index_name: Option<String> = None;
        if self.peek_as(TokenType::Dot).is_ok() {
            self.consume_as(TokenType::Dot)?;

            table_or_index_name = Some(self.parse_sqlite3_name()?);
        }

        self.finalize_statement_parsing()?;

        Ok(Statement::Analyze(AnalyzeStatement {
            schema_name: Some(schema_name),
            table_or_index_name,
        }))
    }
}

#[cfg(test)]
mod analyze_statements_tests {
    use crate::ast::AnalyzeStatement;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Parser, ParsingError, Statement};

    #[test]
    fn test_analyze_basic() {
        let sql = "ANALYZE;";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: None,
                table_or_index_name: None,
            }),
        );
    }

    #[test]
    fn test_analyze_with_schema_name() {
        let sql = "ANALYZE main;";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("main".to_string()),
                table_or_index_name: None,
            }),
        );
    }

    #[test]
    fn test_analyze_with_schema_and_table() {
        let sql = "ANALYZE main.my_table;";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("main".to_string()),
                table_or_index_name: Some("my_table".to_string()),
            }),
        );
    }

    #[test]
    fn test_analyze_with_single_quoted_schema() {
        let sql = "ANALYZE 'main';";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("'main'".to_string()),
                table_or_index_name: None,
            }),
        );
    }

    #[test]
    fn test_analyze_with_double_quoted_schema() {
        let sql = "ANALYZE \"main\";";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("\"main\"".to_string()),
                table_or_index_name: None,
            }),
        );
    }

    #[test]
    fn test_analyze_with_single_quoted_schema_and_table() {
        let sql = "ANALYZE 'main'.'my_table';";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("'main'".to_string()),
                table_or_index_name: Some("'my_table'".to_string()),
            }),
        );
    }

    #[test]
    fn test_analyze_with_double_quoted_schema_and_table() {
        let sql = "ANALYZE \"main\".\"my_table\";";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("\"main\"".to_string()),
                table_or_index_name: Some("\"my_table\"".to_string()),
            }),
        );
    }

    #[test]
    fn test_analyze_missing_semicolon() {
        let sql = "ANALYZE";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: None,
                table_or_index_name: None,
            }),
        );
    }

    #[test]
    fn test_analyze_with_invalid_schema_name() {
        let sql = "ANALYZE 'unclosed_schema;";
        run_rainy_day_test(
            sql,
            ParsingError::TokenizerError("UnterminatedLiteral: 'unclosed_schema;".into()),
        );
    }

    #[test]
    fn test_analyze_with_invalid_table_name() {
        let sql = "ANALYZE main.'unclosed_table;";
        run_rainy_day_test(
            sql,
            ParsingError::TokenizerError("UnterminatedLiteral: 'unclosed_table;".into()),
        );
    }

    #[test]
    fn test_analyze_with_numeric_schema_name() {
        let sql = "ANALYZE '123';";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("'123'".to_string()),
                table_or_index_name: None,
            }),
        );
    }

    #[test]
    fn test_analyze_with_numeric_table_name() {
        let sql = "ANALYZE main.'123';";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("main".to_string()),
                table_or_index_name: Some("'123'".to_string()),
            }),
        );
    }

    #[test]
    fn test_analyze_with_escaped_quotes_in_schema_name() {
        let sql = "ANALYZE 'main''db';";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("'main''db'".to_string()),
                table_or_index_name: None,
            }),
        );
    }

    #[test]
    fn test_analyze_with_escaped_quotes_in_table_name() {
        let sql = "ANALYZE main.'table''name';";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("main".to_string()),
                table_or_index_name: Some("'table''name'".to_string()),
            }),
        );
    }

    #[test]
    fn test_analyze_with_backticks_schema_name() {
        let sql = "ANALYZE `main`;";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("`main`".to_string()),
                table_or_index_name: None,
            }),
        );
    }

    #[test]
    fn test_analyze_with_special_chars_in_table_name() {
        let sql = "ANALYZE main.'[email protected]!';";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("main".to_string()),
                table_or_index_name: Some("'[email protected]!'".to_string()),
            }),
        );
    }

    #[test]
    fn test_analyze_multiple_statements() {
        let sql = "ANALYZE; ANALYZE main.my_table;";
        let mut parser = Parser::from(sql);

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let first_expected_statement = Statement::Analyze(AnalyzeStatement {
            schema_name: None,
            table_or_index_name: None,
        });
        assert_eq!(
            first_actual_statement, first_expected_statement,
            "Expected statement {:?}, got {:?}",
            first_expected_statement, first_actual_statement
        );

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let second_expected_statement = Statement::Analyze(AnalyzeStatement {
            schema_name: Some("main".to_string()),
            table_or_index_name: Some("my_table".to_string()),
        });
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }
}
