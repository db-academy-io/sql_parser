use crate::ast::{DropIndexStatement, DropTableStatement, DropTriggerStatement, DropViewStatement};
use crate::{Identifier, Keyword, Parser, ParsingError, Statement};

use super::expression::ExpressionParser;

pub trait DropStatementParser {
    fn parse_drop_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_drop_table_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_drop_view_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_drop_index_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_drop_trigger_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_drop_statement_generic(&mut self) -> Result<(bool, Identifier), ParsingError>;
}

impl<'a> DropStatementParser for Parser<'a> {
    /// Parse DROP statement
    /// The DROP statement consists of the
    ///   - DROP TABLE statements
    ///   - DROP VIEW statements
    ///   - DROP INDEX statements
    ///   - DROP TRIGGER statements
    fn parse_drop_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_keyword(Keyword::Drop)?;
        let keyword = self.peek_as_keyword()?;

        let drop_statement = match keyword {
            Keyword::Table => self.parse_drop_table_statement()?,
            Keyword::View => self.parse_drop_view_statement()?,
            Keyword::Index => self.parse_drop_index_statement()?,
            Keyword::Trigger => self.parse_drop_trigger_statement()?,
            _ => return Err(ParsingError::UnexpectedKeyword(keyword)),
        };

        self.finalize_statement_parsing()?;

        Ok(drop_statement)
    }

    fn parse_drop_statement_generic(&mut self) -> Result<(bool, Identifier), ParsingError> {
        // Parse optional [IF EXISTS] part of the statement
        let if_exists = if self.consume_keyword(Keyword::If).is_ok() {
            self.consume_keyword(Keyword::Exists)?;
            true
        } else {
            false
        };
        let identifier = self.parse_identifier()?;
        Ok((if_exists, identifier))
    }

    fn parse_drop_table_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_keyword(Keyword::Table)?;

        let (if_exists, identifier) = self.parse_drop_statement_generic()?;

        Ok(Statement::DropTable(DropTableStatement {
            if_exists,
            identifier,
        }))
    }

    fn parse_drop_view_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_keyword(Keyword::View)?;

        let (if_exists, identifier) = self.parse_drop_statement_generic()?;

        Ok(Statement::DropView(DropViewStatement {
            if_exists,
            identifier,
        }))
    }

    fn parse_drop_index_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_keyword(Keyword::Index)?;

        let (if_exists, identifier) = self.parse_drop_statement_generic()?;

        Ok(Statement::DropIndex(DropIndexStatement {
            if_exists,
            identifier,
        }))
    }

    fn parse_drop_trigger_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_keyword(Keyword::Trigger)?;

        let (if_exists, identifier) = self.parse_drop_statement_generic()?;

        Ok(Statement::DropTrigger(DropTriggerStatement {
            if_exists,
            identifier,
        }))
    }
}

#[cfg(test)]
mod drop_table_tests {
    use crate::ast::DropTableStatement;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Identifier, Parser, ParsingError, Statement};

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
                identifier: Identifier::Single("my_table".to_string()),
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
                identifier: Identifier::Compound(vec!["main".to_string(), "my_table".to_string()]),
            }),
        )
    }

    #[test]
    fn test_drop_table_missing_table_name() {
        let sql = "DROP TABLE ;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn test_drop_table_missing_table_keyword() {
        let sql = "DROP my_table;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("my_table".to_string()));
    }

    #[test]
    fn test_drop_table_invalid_syntax() {
        let sql = "DROP TABLE IF my_table;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected Exists keyword, got: my_table".to_string()),
        )
    }

    #[test]
    fn test_drop_table_extra_tokens() {
        let sql = "DROP TABLE my_table extra;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("extra".to_string()));
    }

    #[test]
    fn test_drop_table_invalid_name() {
        let sql = "DROP TABLE 123invalid;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".to_string()),
        );
    }

    #[test]
    fn test_drop_table_if_exists_missing_name() {
        let sql = "DROP TABLE IF EXISTS;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn test_drop_table_missing_semicolon() {
        let sql = "DROP TABLE my_table";
        run_sunny_day_test(
            sql,
            Statement::DropTable(DropTableStatement::name("my_table".to_string())),
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
            identifier: Identifier::Compound(vec![
                "schema".to_string(),
                "my_second_table".to_string(),
            ]),
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
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Identifier, Parser, ParsingError, Statement};

    #[test]
    fn test_drop_index_valid() {
        let sql = "DROP INDEX my_index;";
        run_sunny_day_test(
            sql,
            Statement::DropIndex(DropIndexStatement::name("my_index".to_string())),
        )
    }

    #[test]
    fn test_drop_index_if_exists() {
        let sql = "DROP INDEX IF EXISTS my_index;";
        run_sunny_day_test(
            sql,
            Statement::DropIndex(DropIndexStatement {
                if_exists: true,
                identifier: Identifier::Single("my_index".to_string()),
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
                identifier: Identifier::Compound(vec!["main".to_string(), "my_index".to_string()]),
            }),
        )
    }

    #[test]
    fn test_drop_index_missing_index_name() {
        let sql = "DROP INDEX ;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );

        let sql = "DROP INDEX";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn test_drop_index_missing_index_keyword() {
        let sql = "DROP my_index;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("my_index".into()));
    }

    #[test]
    fn test_drop_index_invalid_syntax() {
        let sql = "DROP INDEX IF my_index;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected Exists keyword, got: my_index".into()),
        )
    }

    #[test]
    fn test_drop_index_extra_tokens() {
        let sql = "DROP INDEX my_index extra;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("extra".into()));
    }

    #[test]
    fn test_drop_index_invalid_name() {
        let sql = "DROP INDEX 123invalid;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn test_drop_index_if_exists_missing_name() {
        let sql = "DROP INDEX IF EXISTS;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn test_drop_index_missing_semicolon() {
        let sql = "DROP INDEX my_index";
        run_sunny_day_test(
            sql,
            Statement::DropIndex(DropIndexStatement::name("my_index".to_string())),
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
            identifier: Identifier::Single("my_index".to_string()),
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
            identifier: Identifier::Compound(vec![
                "schema".to_string(),
                "my_second_index".to_string(),
            ]),
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
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Identifier, Parser, ParsingError, Statement};

    #[test]
    fn test_drop_view_valid() {
        let sql = "DROP VIEW my_view;";
        run_sunny_day_test(
            sql,
            Statement::DropView(DropViewStatement {
                if_exists: false,
                identifier: Identifier::Single("my_view".to_string()),
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
                identifier: Identifier::Single("my_view".to_string()),
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
                identifier: Identifier::Compound(vec!["main".to_string(), "my_view".to_string()]),
            }),
        )
    }

    #[test]
    fn test_drop_view_missing_view_name() {
        let sql = "DROP VIEW ;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );

        let sql = "DROP VIEW";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn test_drop_view_missing_view_keyword() {
        let sql = "DROP my_view;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("my_view".into()));
    }

    #[test]
    fn test_drop_view_invalid_syntax() {
        let sql = "DROP VIEW IF my_view;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected Exists keyword, got: my_view".into()),
        )
    }

    #[test]
    fn test_drop_view_extra_tokens() {
        let sql = "DROP VIEW my_view extra;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("extra".into()));
    }

    #[test]
    fn test_drop_view_invalid_name() {
        let sql = "DROP VIEW 123invalid;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn test_drop_view_if_exists_missing_name() {
        let sql = "DROP VIEW IF EXISTS;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn test_drop_view_missing_semicolon() {
        let sql = "DROP VIEW my_view";
        run_sunny_day_test(
            sql,
            Statement::DropView(DropViewStatement::name("my_view".to_string())),
        );
    }

    #[test]
    fn test_multiple_drop_view_commands() {
        let sql = "DROP VIEW my_view; DROP VIEW IF EXISTS schema.my_second_view;";

        let mut parser = Parser::from(sql);
        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        let first_expected_statement =
            Statement::DropView(DropViewStatement::name("my_view".to_string()));

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
            identifier: Identifier::Compound(vec![
                "schema".to_string(),
                "my_second_view".to_string(),
            ]),
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
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Identifier, Parser, ParsingError, Statement};

    #[test]
    fn test_drop_trigger_valid() {
        let sql = "DROP TRIGGER my_trigger;";
        run_sunny_day_test(
            sql,
            Statement::DropTrigger(DropTriggerStatement::name("my_trigger".to_string())),
        )
    }

    #[test]
    fn test_drop_trigger_if_exists() {
        let sql = "DROP TRIGGER IF EXISTS my_trigger;";
        run_sunny_day_test(
            sql,
            Statement::DropTrigger(DropTriggerStatement {
                if_exists: true,
                identifier: Identifier::Single("my_trigger".to_string()),
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
                identifier: Identifier::Compound(vec![
                    "main".to_string(),
                    "my_trigger".to_string(),
                ]),
            }),
        )
    }

    #[test]
    fn test_drop_trigger_missing_trigger_name() {
        let sql = "DROP TRIGGER ;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn test_drop_trigger_missing_trigger_keyword() {
        let sql = "DROP my_trigger;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("my_trigger".into()));
    }

    #[test]
    fn test_drop_trigger_invalid_syntax() {
        let sql = "DROP TRIGGER IF my_trigger;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected Exists keyword, got: my_trigger".into()),
        )
    }

    #[test]
    fn test_drop_trigger_extra_tokens() {
        let sql = "DROP TRIGGER my_trigger extra;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("extra".into()));
    }

    #[test]
    fn test_drop_trigger_invalid_name() {
        let sql = "DROP TRIGGER 123invalid;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn test_drop_trigger_if_exists_missing_name() {
        let sql = "DROP TRIGGER IF EXISTS;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn test_drop_trigger_missing_semicolon() {
        let sql = "DROP TRIGGER my_trigger";
        run_sunny_day_test(
            sql,
            Statement::DropTrigger(DropTriggerStatement {
                if_exists: false,
                identifier: Identifier::Single("my_trigger".to_string()),
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
            identifier: Identifier::Single("my_trigger".to_string()),
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
            identifier: Identifier::Compound(vec![
                "schema".to_string(),
                "my_second_trigger".to_string(),
            ]),
        });

        // Verify that the statements match
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }
}
