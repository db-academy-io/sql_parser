use crate::parser::errors::ParsingError;
use crate::{Keyword, Parser, ReindexStatement, Statement, TokenType};

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

        let target_name: Option<String> = if self.consume_as(TokenType::Dot).is_ok() {
            Some(self.parse_sqlite3_name()?)
        } else {
            None
        };

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
    use crate::parser::errors::ParsingError;
    use crate::parser::test_utils::{
        assert_statements_equal, run_rainy_day_test, run_sunny_day_test,
    };
    use crate::{Parser, Statement};

    use super::test_utils::reindex_statement;

    #[test]
    fn reindex_test() {
        run_sunny_day_test("REINDEX;", Statement::Reindex(reindex_statement()));
    }

    #[test]
    fn reindex_collation_name() {
        run_sunny_day_test(
            "REINDEX my_collation;",
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("my_collation".to_string()),
            }),
        );
    }

    #[test]
    fn reindex_table_name() {
        run_sunny_day_test(
            "REINDEX my_table;",
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("my_table".to_string()),
            }),
        );
    }

    #[test]
    fn reindex_index_name() {
        run_sunny_day_test(
            "REINDEX my_index;",
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("my_index".to_string()),
            }),
        );
    }

    #[test]
    fn reindex_with_schema_and_table() {
        run_sunny_day_test(
            "REINDEX main.my_table;",
            Statement::Reindex(ReindexStatement {
                schema_name: Some("main".to_string()),
                target_name: Some("my_table".to_string()),
            }),
        );
    }

    #[test]
    fn reindex_with_single_quoted_name() {
        run_sunny_day_test(
            "REINDEX 'my_table';",
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("'my_table'".to_string()),
            }),
        );
    }

    #[test]
    fn reindex_with_double_quoted_name() {
        run_sunny_day_test(
            "REINDEX \"my_table\";",
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("\"my_table\"".to_string()),
            }),
        );
    }

    #[test]
    fn reindex_with_single_quoted_schema_and_name() {
        run_sunny_day_test(
            "REINDEX 'main'.'my_table';",
            Statement::Reindex(ReindexStatement {
                schema_name: Some("'main'".to_string()),
                target_name: Some("'my_table'".to_string()),
            }),
        );
    }

    #[test]
    fn reindex_missing_semicolon() {
        run_sunny_day_test(
            "REINDEX",
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: None,
            }),
        );
    }

    #[test]
    fn reindex_invalid_syntax_extra_token() {
        run_rainy_day_test(
            "REINDEX extra_token extra;",
            ParsingError::UnexpectedToken("extra at position 20".to_string()),
        );
    }

    #[test]
    fn reindex_with_invalid_target_name() {
        run_rainy_day_test(
            "REINDEX main.'unclosed_name;",
            ParsingError::TokenizerError("UnterminatedLiteral: 'unclosed_name;".into()),
        );
    }

    #[test]
    fn reindex_with_numeric_name() {
        run_sunny_day_test(
            "REINDEX '123';",
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("'123'".to_string()),
            }),
        );
    }

    #[test]
    fn reindex_with_numeric_schema_and_name() {
        run_sunny_day_test(
            "REINDEX '123'.'456';",
            Statement::Reindex(ReindexStatement {
                schema_name: Some("'123'".to_string()),
                target_name: Some("'456'".to_string()),
            }),
        );
    }

    #[test]
    fn reindex_with_backticks_schema_and_name() {
        run_sunny_day_test(
            "REINDEX `main`.`my_table`;",
            Statement::Reindex(ReindexStatement {
                schema_name: Some("`main`".to_string()),
                target_name: Some("`my_table`".to_string()),
            }),
        );
    }

    #[test]
    fn reindex_with_special_chars_in_schema_and_name() {
        run_sunny_day_test(
            "REINDEX '[email protected]!'.'[email protected]!';",
            Statement::Reindex(ReindexStatement {
                schema_name: Some("'[email protected]!'".to_string()),
                target_name: Some("'[email protected]!'".to_string()),
            }),
        );
    }

    #[test]
    fn reindex_multiple_statements() {
        let sql = "REINDEX; REINDEX main.my_table;";

        let mut parser = Parser::from(sql);

        let first_expected_statement = Statement::Reindex(reindex_statement());
        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(first_expected_statement, first_actual_statement);

        let second_expected_statement = Statement::Reindex(ReindexStatement {
            schema_name: Some("main".to_string()),
            target_name: Some("my_table".to_string()),
        });

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(second_expected_statement, second_actual_statement);
    }
}
