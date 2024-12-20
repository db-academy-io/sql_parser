use crate::{Keyword, Parser, ParsingError, ReindexStatement, Statement, TokenType};

use super::sqlite3_name::SQLite3NameParser;

pub trait ReindexStatementParser {
    /// Parses a REINDEX statement
    fn parse_reindex_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> ReindexStatementParser for Parser<'a> {
    fn parse_reindex_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Reindex)?;

        // Check if we've got only 'REINDEX;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::Reindex(crate::ReindexStatement::default()));
        }

        let schema_name = Some(self.parse_sqlite3_name()?);

        // Check if we've got only 'REINDEX $collation_name;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::Reindex(ReindexStatement {
                // This swap was made intensionaly. The collation_name takes precedence
                // over table or index names
                schema_name: None,
                target_name: schema_name,
            }));
        }

        let mut target_name: Option<String> = None;
        if self.peek_as(TokenType::Dot).is_ok() {
            self.consume_as(TokenType::Dot)?;

            target_name = Some(self.parse_sqlite3_name()?);
        }

        self.finalize_statement_parsing()?;

        Ok(Statement::Reindex(ReindexStatement {
            schema_name,
            target_name,
        }))
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::ast::ReindexStatement;

    pub fn reindex_statement() -> ReindexStatement {
        ReindexStatement::default()
    }
}

#[cfg(test)]
mod reindex_statements_tests {
    use crate::ast::ReindexStatement;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Parser, ParsingError, Statement};

    #[test]
    fn test_reindex_basic() {
        let sql = "REINDEX;";
        run_sunny_day_test(sql, Statement::Reindex(ReindexStatement::default()));
    }

    #[test]
    fn test_reindex_collation_name() {
        let sql = "REINDEX my_collation;";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("my_collation".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_table_name() {
        let sql = "REINDEX my_table;";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("my_table".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_index_name() {
        let sql = "REINDEX my_index;";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("my_index".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_with_schema_and_table() {
        let sql = "REINDEX main.my_table;";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: Some("main".to_string()),
                target_name: Some("my_table".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_with_single_quoted_name() {
        let sql = "REINDEX 'my_table';";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("'my_table'".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_with_double_quoted_name() {
        let sql = "REINDEX \"my_table\";";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("\"my_table\"".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_with_single_quoted_schema_and_name() {
        let sql = "REINDEX 'main'.'my_table';";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: Some("'main'".to_string()),
                target_name: Some("'my_table'".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_missing_semicolon() {
        let sql = "REINDEX";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: None,
            }),
        );
    }

    #[test]
    fn test_reindex_invalid_syntax_extra_token() {
        let sql = "REINDEX extra_token extra;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("extra".into()));
    }

    #[test]
    fn test_reindex_unexpected_token() {
        let sql = "REINDEX 123;";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("123".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_with_invalid_schema_name() {
        let sql = "REINDEX 'unclosed_schema;";
        run_rainy_day_test(
            sql,
            ParsingError::TokenizerError("UnterminatedLiteral: 'unclosed_schema;".into()),
        );
    }

    #[test]
    fn test_reindex_with_invalid_target_name() {
        let sql = "REINDEX main.'unclosed_name;";
        run_rainy_day_test(
            sql,
            ParsingError::TokenizerError("UnterminatedLiteral: 'unclosed_name;".into()),
        );
    }

    #[test]
    fn test_reindex_with_numeric_name() {
        let sql = "REINDEX '123';";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("'123'".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_with_numeric_schema_and_name() {
        let sql = "REINDEX '123'.'456';";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: Some("'123'".to_string()),
                target_name: Some("'456'".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_with_backticks_schema_and_name() {
        let sql = "REINDEX `main`.`my_table`;";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: Some("`main`".to_string()),
                target_name: Some("`my_table`".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_with_special_chars_in_schema_and_name() {
        let sql = "REINDEX '[email protected]!'.'[email protected]!';";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: Some("'[email protected]!'".to_string()),
                target_name: Some("'[email protected]!'".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_multiple_statements() {
        let sql = "REINDEX; REINDEX main.my_table;";
        let mut parser = Parser::from(sql);

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let first_expected_statement = Statement::Reindex(ReindexStatement {
            schema_name: None,
            target_name: None,
        });
        assert_eq!(
            first_actual_statement, first_expected_statement,
            "Expected statement {:?}, got {:?}",
            first_expected_statement, first_actual_statement
        );

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let second_expected_statement = Statement::Reindex(ReindexStatement {
            schema_name: Some("main".to_string()),
            target_name: Some("my_table".to_string()),
        });
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }
}
