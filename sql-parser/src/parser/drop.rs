use crate::ast::{DropIndexStatement, DropTableStatement, DropTriggerStatement, DropViewStatement};
use crate::{Keyword, Parser, ParsingError, Statement, TokenType};

pub trait DropStatementParser {
    fn parse_drop_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_drop_table_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_drop_view_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_drop_index_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_drop_trigger_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_drop_statement_generic(
        &mut self,
    ) -> Result<(bool, Option<String>, String), ParsingError>;
}

impl<'a> DropStatementParser for Parser<'a> {
    /// Parse DROP statement
    /// The DROP statement consists of the
    ///   - DROP TABLE statements
    ///   - DROP VIEW statements
    ///   - DROP INDEX statements
    ///   - DROP TRIGGER statements
    fn parse_drop_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the DROP keyword
        self.consume_token()?;
        let keyword = self.peek_as_keyword()?;

        match keyword {
            Keyword::Table => self.parse_drop_table_statement(),
            Keyword::View => self.parse_drop_view_statement(),
            Keyword::Index => self.parse_drop_index_statement(),
            Keyword::Trigger => self.parse_drop_trigger_statement(),
            _ => Err(ParsingError::UnexpectedKeyword(keyword)),
        }
    }

    fn parse_drop_statement_generic(
        &mut self,
    ) -> Result<(bool, Option<String>, String), ParsingError> {
        let mut if_exists = false;
        // Parse optional [IF EXISTS] part of the statement
        if let Ok(Keyword::If) = self.peek_as_keyword() {
            // Consume the IF keyword
            self.consume_token()?;
            match self.peek_as_keyword()? {
                Keyword::Exists => {
                    if_exists = true;
                    // Consume the EXISTS keyword
                    self.consume_token()?;
                }
                _ => return Err(ParsingError::ExpectedKeyword(Keyword::Exists)),
            }
        }

        let mut schema_name: Option<String> = None;
        let mut name: String = self.peek_as_id()?.to_string();

        // consume the first id token (which might be the table name)
        self.consume_token()?;

        if let Ok(token) = self.peek_token() {
            if token.token_type == TokenType::Dot {
                // in this case we need to parse schema and name

                // consume the DOT token
                self.consume_token()?;

                // swap: The first ID token was for the schema name
                schema_name = Some(name);
                name = self.peek_as_id()?.to_string();

                // consume table_name token
                self.consume_token()?;
            }
        }

        // check if it is end-of-statement
        self.is_end_of_statement()?;

        Ok((if_exists, schema_name, name))
    }

    fn parse_drop_table_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the TABLE keyword
        self.consume_token()?;

        let (if_exists, schema_name, table_name) = self.parse_drop_statement_generic()?;

        Ok(Statement::DropTable(DropTableStatement {
            if_exists,
            schema_name,
            table_name,
        }))
    }

    fn parse_drop_view_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the VIEW keyword
        self.consume_token()?;

        let (if_exists, schema_name, view_name) = self.parse_drop_statement_generic()?;

        Ok(Statement::DropView(DropViewStatement {
            if_exists,
            schema_name,
            view_name,
        }))
    }

    fn parse_drop_index_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the INDEX keyword
        self.consume_token()?;

        let (if_exists, schema_name, index_name) = self.parse_drop_statement_generic()?;

        Ok(Statement::DropIndex(DropIndexStatement {
            if_exists,
            schema_name,
            index_name,
        }))
    }

    fn parse_drop_trigger_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the TRIGGER keyword
        self.consume_token()?;

        let (if_exists, schema_name, trigger_name) = self.parse_drop_statement_generic()?;

        Ok(Statement::DropTrigger(DropTriggerStatement {
            if_exists,
            schema_name,
            trigger_name,
        }))
    }
}

#[cfg(test)]
mod drop_table_tests {
    use crate::ast::DropTableStatement;
    use crate::{Parser, ParsingError, Statement};

    fn run_sunny_day_test(sql: &str, expected_statement: Statement) {
        let mut parser = Parser::from(sql);
        let actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        // Verify that the statements are matches
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
    fn test_drop_table_valid() {
        let sql = "DROP TABLE my_table;";
        run_sunny_day_test(
            sql,
            Statement::DropTable(DropTableStatement::name("my_table".to_string())),
        )
    }

    #[test]
    fn test_drop_table_if_exists() {
        let sql = "DROP TABLE IF EXISTS my_table;";
        run_sunny_day_test(
            sql,
            Statement::DropTable(DropTableStatement {
                if_exists: true,
                schema_name: None,
                table_name: "my_table".to_string(),
            }),
        )
    }

    #[test]
    fn test_drop_table_with_schema() {
        let sql = "DROP TABLE main.my_table;";
        run_sunny_day_test(
            sql,
            Statement::DropTable(DropTableStatement {
                if_exists: false,
                schema_name: Some("main".to_string()),
                table_name: "my_table".to_string(),
            }),
        )
    }

    #[test]
    fn test_drop_table_missing_table_name() {
        let sql = "DROP TABLE ;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_drop_table_missing_table_keyword() {
        let sql = "DROP my_table;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("my_table".to_string()));
    }

    #[test]
    fn test_drop_table_invalid_syntax() {
        let sql = "DROP TABLE IF my_table;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("my_table".to_string()))
    }

    #[test]
    fn test_drop_table_extra_tokens() {
        let sql = "DROP TABLE my_table extra;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("extra".to_string()));
    }

    #[test]
    fn test_drop_table_invalid_name() {
        let sql = "DROP TABLE 123invalid;";
        run_rainy_day_test(sql, ParsingError::TokenizerError("BadNumber".to_string()));
    }

    #[test]
    fn test_drop_table_if_exists_missing_name() {
        let sql = "DROP TABLE IF EXISTS;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_drop_table_missing_semicolon() {
        let sql = "DROP TABLE my_table";
        run_sunny_day_test(
            sql,
            Statement::DropTable(DropTableStatement {
                if_exists: false,
                schema_name: None,
                table_name: "my_table".to_string(),
            }),
        );
    }

    #[test]
    fn test_multiple_drop_table_commands() {
        let sql = "DROP TABLE my_table; DROP TABLE IF EXISTS schema.my_second_table;";

        let mut parser = Parser::from(sql);
        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        let first_expected_statement =
            Statement::DropTable(DropTableStatement::name("my_table".to_string()));

        // Verify that the statements are matches
        assert_eq!(
            first_actual_statement, first_expected_statement,
            "Expected statement {:?}, got {:?}",
            first_actual_statement, first_expected_statement
        );

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        let secont_expected_statement = Statement::DropTable(DropTableStatement {
            if_exists: true,
            schema_name: Some("schema".to_string()),
            table_name: "my_second_table".to_string(),
        });

        // Verify that the statements are matches
        assert_eq!(
            second_actual_statement, secont_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_actual_statement, secont_expected_statement
        );
    }
}

#[cfg(test)]
mod drop_index_tests {
    use crate::ast::DropIndexStatement;
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
    fn test_drop_index_valid() {
        let sql = "DROP INDEX my_index;";
        run_sunny_day_test(
            sql,
            Statement::DropIndex(DropIndexStatement {
                if_exists: false,
                schema_name: None,
                index_name: "my_index".to_string(),
            }),
        )
    }

    #[test]
    fn test_drop_index_if_exists() {
        let sql = "DROP INDEX IF EXISTS my_index;";
        run_sunny_day_test(
            sql,
            Statement::DropIndex(DropIndexStatement {
                if_exists: true,
                schema_name: None,
                index_name: "my_index".to_string(),
            }),
        )
    }

    #[test]
    fn test_drop_index_with_schema() {
        let sql = "DROP INDEX main.my_index;";
        run_sunny_day_test(
            sql,
            Statement::DropIndex(DropIndexStatement {
                if_exists: false,
                schema_name: Some("main".to_string()),
                index_name: "my_index".to_string(),
            }),
        )
    }

    #[test]
    fn test_drop_index_missing_index_name() {
        let sql = "DROP INDEX ;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));

        let sql = "DROP INDEX"; // TODO: Improve the error reporting
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_drop_index_missing_index_keyword() {
        let sql = "DROP my_index;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("my_index".into()));
    }

    #[test]
    fn test_drop_index_invalid_syntax() {
        let sql = "DROP INDEX IF my_index;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("my_index".into()))
    }

    #[test]
    fn test_drop_index_extra_tokens() {
        let sql = "DROP INDEX my_index extra;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("extra".into()));
    }

    #[test]
    fn test_drop_index_invalid_name() {
        let sql = "DROP INDEX 123invalid;";
        run_rainy_day_test(sql, ParsingError::TokenizerError("BadNumber".into()));
    }

    #[test]
    fn test_drop_index_if_exists_missing_name() {
        let sql = "DROP INDEX IF EXISTS;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_drop_index_missing_semicolon() {
        let sql = "DROP INDEX my_index";
        run_sunny_day_test(
            sql,
            Statement::DropIndex(DropIndexStatement {
                if_exists: false,
                schema_name: None,
                index_name: "my_index".to_string(),
            }),
        );
    }

    #[test]
    fn test_multiple_drop_index_commands() {
        let sql = "DROP INDEX my_index; DROP INDEX IF EXISTS schema.my_second_index;";

        let mut parser = Parser::from(sql);
        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        let first_expected_statement = Statement::DropIndex(DropIndexStatement {
            if_exists: false,
            schema_name: None,
            index_name: "my_index".to_string(),
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

        let second_expected_statement = Statement::DropIndex(DropIndexStatement {
            if_exists: true,
            schema_name: Some("schema".to_string()),
            index_name: "my_second_index".to_string(),
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
mod drop_view_tests {
    use crate::ast::DropViewStatement;
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
    fn test_drop_view_valid() {
        let sql = "DROP VIEW my_view;";
        run_sunny_day_test(
            sql,
            Statement::DropView(DropViewStatement {
                if_exists: false,
                schema_name: None,
                view_name: "my_view".to_string(),
            }),
        )
    }

    #[test]
    fn test_drop_view_if_exists() {
        let sql = "DROP VIEW IF EXISTS my_view;";
        run_sunny_day_test(
            sql,
            Statement::DropView(DropViewStatement {
                if_exists: true,
                schema_name: None,
                view_name: "my_view".to_string(),
            }),
        )
    }

    #[test]
    fn test_drop_view_with_schema() {
        let sql = "DROP VIEW main.my_view;";
        run_sunny_day_test(
            sql,
            Statement::DropView(DropViewStatement {
                if_exists: false,
                schema_name: Some("main".to_string()),
                view_name: "my_view".to_string(),
            }),
        )
    }

    #[test]
    fn test_drop_view_missing_view_name() {
        let sql = "DROP VIEW ;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));

        let sql = "DROP VIEW"; // TODO: Improve the error reporting
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_drop_view_missing_view_keyword() {
        let sql = "DROP my_view;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("my_view".into()));
    }

    #[test]
    fn test_drop_view_invalid_syntax() {
        let sql = "DROP VIEW IF my_view;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("my_view".into()))
    }

    #[test]
    fn test_drop_view_extra_tokens() {
        let sql = "DROP VIEW my_view extra;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("extra".into()));
    }

    #[test]
    fn test_drop_view_invalid_name() {
        let sql = "DROP VIEW 123invalid;";
        run_rainy_day_test(sql, ParsingError::TokenizerError("BadNumber".into()));
    }

    #[test]
    fn test_drop_view_if_exists_missing_name() {
        let sql = "DROP VIEW IF EXISTS;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_drop_view_missing_semicolon() {
        let sql = "DROP VIEW my_view";
        run_sunny_day_test(
            sql,
            Statement::DropView(DropViewStatement {
                if_exists: false,
                schema_name: None,
                view_name: "my_view".to_string(),
            }),
        );
    }

    #[test]
    fn test_multiple_drop_view_commands() {
        let sql = "DROP VIEW my_view; DROP VIEW IF EXISTS schema.my_second_view;";

        let mut parser = Parser::from(sql);
        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        let first_expected_statement = Statement::DropView(DropViewStatement {
            if_exists: false,
            schema_name: None,
            view_name: "my_view".to_string(),
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

        let second_expected_statement = Statement::DropView(DropViewStatement {
            if_exists: true,
            schema_name: Some("schema".to_string()),
            view_name: "my_second_view".to_string(),
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
mod drop_trigger_tests {
    use crate::ast::DropTriggerStatement;
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
    fn test_drop_trigger_valid() {
        let sql = "DROP TRIGGER my_trigger;";
        run_sunny_day_test(
            sql,
            Statement::DropTrigger(DropTriggerStatement {
                if_exists: false,
                schema_name: None,
                trigger_name: "my_trigger".to_string(),
            }),
        )
    }

    #[test]
    fn test_drop_trigger_if_exists() {
        let sql = "DROP TRIGGER IF EXISTS my_trigger;";
        run_sunny_day_test(
            sql,
            Statement::DropTrigger(DropTriggerStatement {
                if_exists: true,
                schema_name: None,
                trigger_name: "my_trigger".to_string(),
            }),
        )
    }

    #[test]
    fn test_drop_trigger_with_schema() {
        let sql = "DROP TRIGGER main.my_trigger;";
        run_sunny_day_test(
            sql,
            Statement::DropTrigger(DropTriggerStatement {
                if_exists: false,
                schema_name: Some("main".to_string()),
                trigger_name: "my_trigger".to_string(),
            }),
        )
    }

    #[test]
    fn test_drop_trigger_missing_trigger_name() {
        let sql = "DROP TRIGGER ;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_drop_trigger_missing_trigger_keyword() {
        let sql = "DROP my_trigger;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("my_trigger".into()));
    }

    #[test]
    fn test_drop_trigger_invalid_syntax() {
        let sql = "DROP TRIGGER IF my_trigger;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("my_trigger".into()))
    }

    #[test]
    fn test_drop_trigger_extra_tokens() {
        let sql = "DROP TRIGGER my_trigger extra;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("extra".into()));
    }

    #[test]
    fn test_drop_trigger_invalid_name() {
        let sql = "DROP TRIGGER 123invalid;";
        run_rainy_day_test(sql, ParsingError::TokenizerError("BadNumber".into()));
    }

    #[test]
    fn test_drop_trigger_if_exists_missing_name() {
        let sql = "DROP TRIGGER IF EXISTS;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_drop_trigger_missing_semicolon() {
        let sql = "DROP TRIGGER my_trigger";
        run_sunny_day_test(
            sql,
            Statement::DropTrigger(DropTriggerStatement {
                if_exists: false,
                schema_name: None,
                trigger_name: "my_trigger".to_string(),
            }),
        );
    }

    #[test]
    fn test_multiple_drop_trigger_commands() {
        let sql = "DROP TRIGGER my_trigger; DROP TRIGGER IF EXISTS schema.my_second_trigger;";

        let mut parser = Parser::from(sql);
        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        let first_expected_statement = Statement::DropTrigger(DropTriggerStatement {
            if_exists: false,
            schema_name: None,
            trigger_name: "my_trigger".to_string(),
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

        let second_expected_statement = Statement::DropTrigger(DropTriggerStatement {
            if_exists: true,
            schema_name: Some("schema".to_string()),
            trigger_name: "my_second_trigger".to_string(),
        });

        // Verify that the statements match
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }
}
