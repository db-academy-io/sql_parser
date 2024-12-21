use crate::parser::errors::ParsingError;
use crate::{AnalyzeStatement, Keyword, Parser, Statement, TokenType};

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

        let table_or_index_name: Option<String> = if self.consume_as(TokenType::Dot).is_ok() {
            Some(self.parse_sqlite3_name()?)
        } else {
            None
        };

        self.finalize_statement_parsing()?;

        Ok(Statement::Analyze(AnalyzeStatement {
            schema_name: Some(schema_name),
            table_or_index_name,
        }))
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::AnalyzeStatement;

    pub fn analyze_statement() -> AnalyzeStatement {
        AnalyzeStatement {
            schema_name: None,
            table_or_index_name: None,
        }
    }
}

#[cfg(test)]
mod analyze_statements_tests {
    use crate::ast::AnalyzeStatement;
    use crate::parser::errors::ParsingError;
    use crate::parser::test_utils::{
        assert_statements_equal, run_rainy_day_test, run_sunny_day_test,
    };
    use crate::{Parser, Statement};

    use super::test_utils::analyze_statement;

    #[test]
    fn test_analyze_basic() {
        run_sunny_day_test("ANALYZE;", Statement::Analyze(analyze_statement()));
    }

    #[test]
    fn test_analyze_with_schema_name() {
        let mut expected_statement = analyze_statement();
        expected_statement.schema_name = Some("main".to_string());

        run_sunny_day_test("ANALYZE main;", Statement::Analyze(expected_statement));
    }

    #[test]
    fn test_analyze_with_schema_and_table() {
        let mut expected_statement = analyze_statement();
        expected_statement.schema_name = Some("main".to_string());
        expected_statement.table_or_index_name = Some("my_table".to_string());

        run_sunny_day_test(
            "ANALYZE main.my_table;",
            Statement::Analyze(expected_statement),
        );
    }

    #[test]
    fn test_analyze_with_single_quoted_schema() {
        let mut expected_statement = analyze_statement();
        expected_statement.schema_name = Some("'main'".to_string());

        run_sunny_day_test("ANALYZE 'main';", Statement::Analyze(expected_statement));
    }

    #[test]
    fn test_analyze_with_double_quoted_schema() {
        let mut expected_statement = analyze_statement();
        expected_statement.schema_name = Some("\"main\"".to_string());

        run_sunny_day_test("ANALYZE \"main\";", Statement::Analyze(expected_statement));
    }

    #[test]
    fn test_analyze_with_single_quoted_schema_and_table() {
        let mut expected_statement = analyze_statement();
        expected_statement.schema_name = Some("'main'".to_string());
        expected_statement.table_or_index_name = Some("'my_table'".to_string());

        run_sunny_day_test(
            "ANALYZE 'main'.'my_table';",
            Statement::Analyze(expected_statement),
        );
    }

    #[test]
    fn test_analyze_with_double_quoted_schema_and_table() {
        let mut expected_statement = analyze_statement();
        expected_statement.schema_name = Some("\"main\"".to_string());
        expected_statement.table_or_index_name = Some("\"my_table\"".to_string());

        run_sunny_day_test(
            "ANALYZE \"main\".\"my_table\";",
            Statement::Analyze(expected_statement),
        );
    }

    #[test]
    fn test_analyze_missing_semicolon() {
        let sql = "ANALYZE";
        run_sunny_day_test(sql, Statement::Analyze(analyze_statement()));
    }

    #[test]
    fn test_analyze_with_invalid_schema_name() {
        run_rainy_day_test(
            "ANALYZE 'unclosed_schema;",
            ParsingError::TokenizerError("UnterminatedLiteral: 'unclosed_schema;".into()),
        );
    }

    #[test]
    fn test_analyze_with_invalid_table_name() {
        run_rainy_day_test(
            "ANALYZE main.'unclosed_table;",
            ParsingError::TokenizerError("UnterminatedLiteral: 'unclosed_table;".into()),
        );
    }

    #[test]
    fn test_analyze_with_numeric_schema_name() {
        run_sunny_day_test(
            "ANALYZE '123';",
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("'123'".to_string()),
                table_or_index_name: None,
            }),
        );
    }

    #[test]
    fn test_analyze_with_numeric_table_name() {
        run_sunny_day_test(
            "ANALYZE main.'123';",
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("main".to_string()),
                table_or_index_name: Some("'123'".to_string()),
            }),
        );
    }

    #[test]
    fn test_analyze_with_escaped_quotes_in_schema_name() {
        run_sunny_day_test(
            "ANALYZE 'main''db';",
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("'main''db'".to_string()),
                table_or_index_name: None,
            }),
        );
    }

    #[test]
    fn test_analyze_with_escaped_quotes_in_table_name() {
        run_sunny_day_test(
            "ANALYZE main.'table''name';",
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("main".to_string()),
                table_or_index_name: Some("'table''name'".to_string()),
            }),
        );
    }

    #[test]
    fn test_analyze_with_backticks_schema_name() {
        run_sunny_day_test(
            "ANALYZE `main`;",
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("`main`".to_string()),
                table_or_index_name: None,
            }),
        );
    }

    #[test]
    fn test_analyze_with_special_chars_in_table_name() {
        run_sunny_day_test(
            "ANALYZE main.'[email protected]!';",
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

        let first_expected_statement = Statement::Analyze(analyze_statement());

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(first_expected_statement, first_actual_statement);
        let second_expected_statement = Statement::Analyze(AnalyzeStatement {
            schema_name: Some("main".to_string()),
            table_or_index_name: Some("my_table".to_string()),
        });

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(second_expected_statement, second_actual_statement);
    }
}
