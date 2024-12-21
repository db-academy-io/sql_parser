use crate::parser::errors::ParsingError;
use crate::parser::IdentifierParser;
use crate::{DropTriggerStatement, Keyword, Parser, Statement};

pub trait DropTriggerStatementParser {
    fn parse_drop_trigger_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> DropTriggerStatementParser for Parser<'a> {
    fn parse_drop_trigger_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Trigger)?;

        let if_exists = self.parse_if_exists_clause()?;
        let identifier = self.parse_identifier()?;

        Ok(Statement::DropTrigger(DropTriggerStatement {
            if_exists,
            identifier,
        }))
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{ast::DropTriggerStatement, Identifier};

    pub fn drop_trigger_statement() -> DropTriggerStatement {
        DropTriggerStatement {
            if_exists: false,
            identifier: Identifier::Single("my_trigger".to_string()),
        }
    }
}

#[cfg(test)]
mod drop_trigger_tests {
    use crate::ast::DropTriggerStatement;
    use crate::parser::errors::ParsingError;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Identifier, Parser, Statement};

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
