use super::Parser;
use crate::{
    AnalyzeStatement, Keyword, ParsingError, PragmaStatement, ReindexStatement, Statement,
    TokenType, VacuumStatement,
};

pub trait SQLite3StatementParser {
    /// Parses a VACUUM statement
    fn parse_vacuum_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a DETACH statement
    fn parse_detach_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses an ANALYZE statement
    fn parse_analyze_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a REINDEX statement
    fn parse_reindex_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a PRAGMA statement
    fn parse_pragma_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a pragma value
    fn parse_pragma_value(&mut self) -> Result<String, ParsingError>;

    /// Parses a name
    fn parse_name(&mut self) -> Result<String, ParsingError>;
}

impl<'a> SQLite3StatementParser for Parser<'a> {
    fn parse_vacuum_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the VACUUM keyword
        self.consume_token()?;

        // Check if we've got only 'VACUUM;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::Vacuum(VacuumStatement::default()));
        }

        let mut schema: Option<String> = None;
        if let Ok(value) = self.peek_as_string() {
            schema = Some(value);
            // Consume the schema token
            self.consume_token()?;
        }

        // Check if we've got only VACUUM $SCHEMA; command
        if self.finalize_statement_parsing().is_ok() {
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

        let filename = self.parse_name()?;
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

        let schema_name = self.parse_name()?;
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
        let schema_name = self.parse_name()?;

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

            table_or_index_name = Some(self.parse_name()?);
        }

        self.finalize_statement_parsing()?;

        Ok(Statement::Analyze(AnalyzeStatement {
            schema_name: Some(schema_name),
            table_or_index_name,
        }))
    }

    fn parse_reindex_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the REINDEX keyword
        self.consume_token()?;

        // Check if we've got only 'REINDEX;' command
        if self.finalize_statement_parsing().is_ok() {
            return Ok(Statement::Reindex(crate::ReindexStatement::default()));
        }

        let schema_name = Some(self.parse_name()?);

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
            // Consume the '.' token
            self.consume_token()?;

            target_name = Some(self.parse_name()?);
        }

        self.finalize_statement_parsing()?;

        Ok(Statement::Reindex(ReindexStatement {
            schema_name,
            target_name,
        }))
    }

    fn parse_pragma_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the PRAGMA keyword
        self.consume_token()?;

        let mut pragma_name: String = self.peek_as_string()?;
        // Consume the pragma name token
        self.consume_token()?;

        let mut schema_name = None;

        if self.peek_as(TokenType::Dot).is_ok() {
            // Consume the '.' token
            self.consume_token()?;

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
                // Consume the '=' token
                self.consume_token()?;

                let pragma_value = Some(self.parse_pragma_value()?);

                self.finalize_statement_parsing()?;
                Ok(Statement::Pragma(PragmaStatement {
                    schema_name,
                    pragma_name,
                    pragma_value,
                }))
            }
            TokenType::LeftParen => {
                // Consume the '(' token
                self.consume_token()?;

                let pragma_value = Some(self.parse_pragma_value()?);
                let statement = Ok(Statement::Pragma(PragmaStatement {
                    schema_name,
                    pragma_name,
                    pragma_value,
                }));
                if self.peek_as(TokenType::RightParen).is_ok() {
                    // Consume the ')' token
                    self.consume_token()?;
                }
                self.finalize_statement_parsing()?;

                statement
            }
            _ => Err(ParsingError::UnexpectedToken(token.to_string())),
        }
    }

    fn parse_pragma_value(&mut self) -> Result<String, ParsingError> {
        if self.peek_as(TokenType::Plus).is_ok() {
            // Consume the '+' token
            self.consume_token()?;

            let value = self.peek_as_number()?;
            // Consume the number token
            self.consume_token()?;

            return Ok(value);
        }

        if self.peek_as(TokenType::Minus).is_ok() {
            // Consume the '-' token
            self.consume_token()?;

            let value = self.peek_as_number()?;
            // Consume the number token
            self.consume_token()?;

            return Ok(format!("-{}", value));
        }

        if let Ok(value) = self.peek_as_number() {
            // Consume the number token
            self.consume_token()?;

            return Ok(value.to_string());
        }

        let value = self.peek_as_string()?;
        // Consume the string token
        self.consume_token()?;

        Ok(value)
    }

    fn parse_name(&mut self) -> Result<String, ParsingError> {
        if let Ok(keyword) = self.peek_as_keyword() {
            // consume keyword token
            self.consume_token()?;

            // return keyword as string
            return Ok(keyword.to_string());
        }

        if let Ok(value) = self.peek_as_number() {
            // consume keyword token
            self.consume_token()?;

            // return keyword as string
            return Ok(value.to_string());
        }

        let value = self.peek_as_string()?;
        // Consume the `name` token
        self.consume_token()?;
        Ok(value)
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

#[cfg(test)]
mod detach_statements_tests {
    use crate::ast::DetachStatement;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Parser, ParsingError, Statement};

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
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_detach_missing_schema_name_no_database() {
        let sql = "DETACH;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
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
    fn test_reindex_with_reserved_keyword_as_name() {
        let sql = "REINDEX select;";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: None,
                target_name: Some("Select".to_string()),
            }),
        );
    }

    #[test]
    fn test_reindex_with_reserved_keyword_as_schema() {
        let sql = "REINDEX select.my_table;";
        run_sunny_day_test(
            sql,
            Statement::Reindex(ReindexStatement {
                schema_name: Some("Select".to_string()),
                target_name: Some("my_table".to_string()),
            }),
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

#[cfg(test)]
mod pragma_statements_tests {
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Parser, ParsingError, PragmaStatement, Statement};

    #[test]
    fn test_pragma_basic() {
        let sql = "PRAGMA cache_size;";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "cache_size".to_string(),
                pragma_value: None,
            }),
        );
    }

    #[test]
    fn test_pragma_with_schema() {
        let sql = "PRAGMA main.cache_size;";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: Some("main".to_string()),
                pragma_name: "cache_size".to_string(),
                pragma_value: None,
            }),
        );
    }

    #[test]
    fn test_pragma_with_value() {
        let sql = "PRAGMA cache_size = 2000;";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "cache_size".to_string(),
                pragma_value: Some("2000".to_string()),
            }),
        );
    }

    #[test]
    fn test_pragma_with_schema_and_value() {
        let sql = "PRAGMA main.cache_size = 2000;";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: Some("main".to_string()),
                pragma_name: "cache_size".to_string(),
                pragma_value: Some("2000".to_string()),
            }),
        );
    }

    #[test]
    fn test_pragma_with_string_value() {
        let sql = "PRAGMA main.journal_mode = WAL;";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: Some("main".to_string()),
                pragma_name: "journal_mode".to_string(),
                pragma_value: Some("WAL".to_string()),
            }),
        );
    }

    #[test]
    fn test_pragma_with_quoted_value() {
        let sql = "PRAGMA main.journal_mode = 'WAL';";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: Some("main".to_string()),
                pragma_name: "journal_mode".to_string(),
                pragma_value: Some("'WAL'".to_string()),
            }),
        );
    }

    #[test]
    fn test_pragma_without_semicolon() {
        let sql = "PRAGMA cache_size";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "cache_size".to_string(),
                pragma_value: None,
            }),
        );
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
    fn test_pragma_set_value() {
        let sql = "PRAGMA cache_size = 1000;";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "cache_size".to_string(),
                pragma_value: Some("1000".to_string()),
            }),
        );
    }

    #[test]
    fn test_pragma_set_string_value() {
        let sql = "PRAGMA encoding = 'UTF-8';";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "encoding".to_string(),
                pragma_value: Some("'UTF-8'".to_string()),
            }),
        );
    }

    #[test]
    fn test_pragma_set_value_with_schema() {
        let sql = "PRAGMA main.cache_size = 1000;";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: Some("main".to_string()),
                pragma_name: "cache_size".to_string(),
                pragma_value: Some("1000".to_string()),
            }),
        );
    }

    #[test]
    fn test_pragma_call_syntax() {
        let sql = "PRAGMA cache_size(1000);";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "cache_size".to_string(),
                pragma_value: Some("1000".to_string()),
            }),
        );
    }

    #[test]
    fn test_pragma_call_syntax_with_schema() {
        let sql = "PRAGMA main.cache_size(1000);";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: Some("main".to_string()),
                pragma_name: "cache_size".to_string(),
                pragma_value: Some("1000".to_string()),
            }),
        );
    }

    #[test]
    fn test_pragma_missing_semicolon() {
        let sql = "PRAGMA cache_size";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "cache_size".to_string(),
                pragma_value: None,
            }),
        );
    }

    #[test]
    fn test_pragma_with_single_quoted_value() {
        let sql = "PRAGMA cache_size = '1000';";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "cache_size".to_string(),
                pragma_value: Some("'1000'".to_string()),
            }),
        );
    }

    #[test]
    fn test_pragma_with_double_quoted_value() {
        let sql = "PRAGMA cache_size = \"1000\";";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "cache_size".to_string(),
                pragma_value: Some("\"1000\"".to_string()),
            }),
        );
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
        let sql = "PRAGMA cache_size = '[email protected]!';";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "cache_size".to_string(),
                pragma_value: Some("'[email protected]!'".to_string()),
            }),
        );
    }

    #[test]
    fn test_pragma_with_numeric_value() {
        let sql = "PRAGMA cache_size = 123;";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "cache_size".to_string(),
                pragma_value: Some("123".to_string()),
            }),
        );
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

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let first_expected_statement = Statement::Pragma(PragmaStatement {
            schema_name: None,
            pragma_name: "cache_size".to_string(),
            pragma_value: Some("1000".to_string()),
        });
        assert_eq!(
            first_actual_statement, first_expected_statement,
            "Expected statement {:?}, got {:?}",
            first_expected_statement, first_actual_statement
        );

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let second_expected_statement = Statement::Pragma(PragmaStatement {
            schema_name: None,
            pragma_name: "encoding".to_string(),
            pragma_value: Some("'UTF-8'".to_string()),
        });
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }

    #[test]
    fn test_pragma_case_insensitive() {
        let sql = "pragma cache_size = 1000;";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "cache_size".to_string(),
                pragma_value: Some("1000".to_string()),
            }),
        );
    }

    #[test]
    fn test_pragma_with_comment() {
        let sql = "PRAGMA /* set cache size */ cache_size = 1000; ";
        run_sunny_day_test(
            sql,
            Statement::Pragma(PragmaStatement {
                schema_name: None,
                pragma_name: "cache_size".to_string(),
                pragma_value: Some("1000".to_string()),
            }),
        );
    }
}
