use super::Parser;
use crate::{AnalyzeStatement, Keyword, ParsingError, Statement, TokenType, VacuumStatement};

pub trait SQLite3StatementParser {
    fn parse_vacuum_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_detach_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_analyze_statement(&mut self) -> Result<Statement, ParsingError>;
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

        self.finalize_statement_parsing()?;

        Ok(Statement::Vacuum(VacuumStatement {
            schema_name: schema,
            file_name: Some(filename),
        }))
    }

    fn parse_detach_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the VACUUM keyword
        self.consume_token()?;

        // Check if there is a DATABASE keyword
        if let Ok(keyword) = self.peek_as_keyword() {
            if keyword == Keyword::Database {
                // Consume the DATABASE keyword
                self.consume_token()?;
            }
        }

        let token = self.peek_token()?;
        let schema_name = match token.token_type {
            TokenType::Keyword(keyword_as_schema_name) => keyword_as_schema_name.to_string(),
            TokenType::String(schema_name)
            | TokenType::Id(schema_name)
            | TokenType::Variable(schema_name)
            | TokenType::Blob(schema_name)
            | TokenType::Integer(schema_name)
            | TokenType::Float(schema_name) => schema_name.to_string(),
            _ => return Err(ParsingError::UnexpectedEOF),
        };

        // Consume the `schema_name` token
        self.consume_token()?;

        self.finalize_statement_parsing()?;
        Ok(Statement::Detach(crate::DetachStatement { schema_name }))
    }

    fn parse_analyze_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the ANALYZE keyword
        self.consume_token()?;

        // Check if we've got only 'ANALYZE;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::Analyze(AnalyzeStatement::default()));
        }

        let token = self.peek_token()?;

        let schema_name = match token.token_type {
            TokenType::Keyword(keyword_as_schema_name) => keyword_as_schema_name.to_string(),
            TokenType::String(schema_name)
            | TokenType::Id(schema_name)
            | TokenType::Variable(schema_name)
            | TokenType::Blob(schema_name)
            | TokenType::Integer(schema_name)
            | TokenType::Float(schema_name) => schema_name.to_string(),
            _ => return Err(ParsingError::UnexpectedEOF),
        };
        // Consume the `schema_name` token
        self.consume_token()?;

        // Check if we've got only 'ANALYZE $SCHEMA;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::Analyze(AnalyzeStatement {
                schema_name: Some(schema_name),
                table_or_index_name: None,
            }));
        }

        let mut table_or_index_name: Option<String> = None;
        if self.peek_as(TokenType::Dot).is_ok() {
            // Consume the '.' token
            self.consume_token()?;

            let token = self.peek_token()?;
            table_or_index_name = Some(match token.token_type {
                TokenType::Keyword(keyword_as_schema_name) => keyword_as_schema_name.to_string(),
                TokenType::String(schema_name)
                | TokenType::Id(schema_name)
                | TokenType::Variable(schema_name)
                | TokenType::Blob(schema_name)
                | TokenType::Integer(schema_name)
                | TokenType::Float(schema_name) => schema_name.to_string(),
                _ => return Err(ParsingError::UnexpectedEOF),
            });
            // Consume the `table_name` token
            self.consume_token()?;
        }

        self.finalize_statement_parsing()?;

        Ok(Statement::Analyze(AnalyzeStatement {
            schema_name: Some(schema_name),
            table_or_index_name,
        }))
    }
}

#[cfg(test)]
mod vacuum_statements_tests {
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

#[cfg(test)]
mod detach_statements_tests {
    use crate::ast::DetachStatement;
    use crate::{Parser, ParsingError, Statement};

    fn run_sunny_day_test(sql: &str, expected_statement: Statement) {
        let mut parser = Parser::from(sql);
        let actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        // Verify that the statements match
        assert_eq!(
            expected_statement, actual_statement,
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
    fn test_detach_basic() {
        let sql = "DETACH DATABASE schema_name;";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "schema_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_without_database_keyword() {
        let sql = "DETACH schema_name;";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "schema_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_with_single_quoted_schema_name() {
        let sql = "DETACH DATABASE 'schema_name';";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "'schema_name'".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_with_double_quoted_schema_name() {
        let sql = "DETACH DATABASE \"schema_name\";";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "\"schema_name\"".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_missing_schema_name() {
        let sql = "DETACH DATABASE;";
        run_rainy_day_test(sql, ParsingError::UnexpectedEOF);
    }

    #[test]
    fn test_detach_missing_schema_name_no_database() {
        let sql = "DETACH;";
        run_rainy_day_test(sql, ParsingError::UnexpectedEOF);
    }

    #[test]
    fn test_detach_invalid_syntax_extra_token() {
        let sql = "DETACH DATABASE schema_name extra;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("extra".into()));
    }

    #[test]
    fn test_detach_invalid_schema_name_number() {
        let sql = "DETACH DATABASE 123;";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "123".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_unexpected_token() {
        let sql = "DETACH DATABASE INTO 'schema_name';";
        // In this case the keyword INTO becomes as a schema name
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("'schema_name'".into()));
    }

    #[test]
    fn test_detach_missing_semicolon() {
        let sql = "DETACH DATABASE schema_name";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "schema_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_multiple_statements() {
        let sql = "DETACH schema_name1; DETACH DATABASE 'schema_name2';";
        let mut parser = Parser::from(sql);

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let first_expected_statement = Statement::Detach(DetachStatement {
            schema_name: "schema_name1".to_string(),
        });
        assert_eq!(
            first_actual_statement, first_expected_statement,
            "Expected statement {:?}, got {:?}",
            first_expected_statement, first_actual_statement
        );

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let second_expected_statement = Statement::Detach(DetachStatement {
            schema_name: "'schema_name2'".to_string(),
        });
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }

    #[test]
    fn test_detach_schema_name_with_spaces() {
        let sql = "DETACH DATABASE 'schema name with spaces';";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "'schema name with spaces'".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_schema_name_with_special_chars() {
        let sql = "DETACH DATABASE '[email protected]!';";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "'[email protected]!'".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_invalid_schema_name_no_quotes() {
        let sql = "DETACH DATABASE schema name;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("name".into()));
    }

    #[test]
    fn test_detach_unterminated_string() {
        let sql = "DETACH DATABASE 'schema_name;";
        run_rainy_day_test(
            sql,
            ParsingError::TokenizerError("UnterminatedLiteral: 'schema_name;".into()),
        );
    }

    #[test]
    fn test_detach_with_numeric_schema_name_in_quotes() {
        let sql = "DETACH DATABASE '123';";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "'123'".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_with_escaped_quotes_in_schema_name() {
        let sql = "DETACH DATABASE 'schema''name';";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "'schema''name'".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_with_double_escaped_quotes_in_schema_name() {
        let sql = "DETACH DATABASE \"schema\"\"name\";";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "\"schema\"\"name\"".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_with_backticks_schema_name() {
        let sql = "DETACH DATABASE `schema_name`;";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "`schema_name`".to_string(),
            }),
        )
    }
}

#[cfg(test)]
mod analyze_statements_tests {
    use crate::ast::AnalyzeStatement;
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
    fn test_analyze_with_reserved_keyword_as_schema() {
        let sql = "ANALYZE SELECT;";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("Select".to_string()),
                table_or_index_name: None,
            }),
        );
    }

    #[test]
    fn test_analyze_with_reserved_keyword_as_table() {
        let sql = "ANALYZE main.select;";
        run_sunny_day_test(
            sql,
            Statement::Analyze(AnalyzeStatement {
                schema_name: Some("main".to_string()),
                table_or_index_name: Some("Select".to_string()),
            }),
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
