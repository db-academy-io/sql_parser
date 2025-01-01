use crate::parser::errors::ParsingError;
use crate::parser::IdentifierParser;
use crate::{DropTriggerStatement, Keyword, Parser, Statement};

pub trait DropTriggerStatementParser {
    fn parse_drop_trigger_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl DropTriggerStatementParser for Parser<'_> {
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
    use crate::parser::test_utils::{
        assert_statements_equal, run_rainy_day_test, run_sunny_day_test,
    };
    use crate::{Identifier, Parser, Statement};

    use super::test_utils::drop_trigger_statement;

    #[test]
    fn drop_trigger_test() {
        run_sunny_day_test(
            "DROP TRIGGER my_trigger;",
            Statement::DropTrigger(drop_trigger_statement()),
        )
    }

    #[test]
    fn drop_trigger_if_exists() {
        let mut expected_statement = drop_trigger_statement();
        expected_statement.if_exists = true;

        run_sunny_day_test(
            "DROP TRIGGER IF EXISTS my_trigger;",
            Statement::DropTrigger(expected_statement),
        )
    }

    #[test]
    fn drop_trigger_with_schema() {
        let mut expected_statement = drop_trigger_statement();
        expected_statement.identifier =
            Identifier::Compound(vec!["main".to_string(), "my_trigger".to_string()]);

        run_sunny_day_test(
            "DROP TRIGGER main.my_trigger;",
            Statement::DropTrigger(expected_statement),
        )
    }

    #[test]
    fn drop_trigger_missing_trigger_name() {
        run_rainy_day_test(
            "DROP TRIGGER ;",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn drop_trigger_missing_trigger_keyword() {
        run_rainy_day_test(
            "DROP my_trigger;",
            ParsingError::UnexpectedToken("my_trigger at position 5".into()),
        );
    }

    #[test]
    fn drop_trigger_invalid_syntax() {
        run_rainy_day_test(
            "DROP TRIGGER IF my_trigger;",
            ParsingError::UnexpectedToken(
                "Expected Exists keyword, got: my_trigger at position 16".into(),
            ),
        )
    }

    #[test]
    fn drop_trigger_extra_tokens() {
        run_rainy_day_test(
            "DROP TRIGGER my_trigger extra;",
            ParsingError::UnexpectedToken("extra at position 24".into()),
        );
    }

    #[test]
    fn drop_trigger_invalid_name() {
        run_rainy_day_test(
            "DROP TRIGGER 123invalid;",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn drop_trigger_if_exists_missing_name() {
        run_rainy_day_test(
            "DROP TRIGGER IF EXISTS;",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn drop_trigger_missing_semicolon() {
        run_sunny_day_test(
            "DROP TRIGGER my_trigger",
            Statement::DropTrigger(DropTriggerStatement {
                if_exists: false,
                identifier: Identifier::Single("my_trigger".to_string()),
            }),
        );
    }

    #[test]
    fn multiple_drop_trigger_commands() {
        let mut parser = Parser::from(
            "DROP TRIGGER my_trigger; DROP TRIGGER IF EXISTS schema.my_second_trigger;",
        );

        let first_expected_statement = Statement::DropTrigger(DropTriggerStatement {
            if_exists: false,
            identifier: Identifier::Single("my_trigger".to_string()),
        });

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(first_expected_statement, first_actual_statement);

        let second_expected_statement = Statement::DropTrigger(DropTriggerStatement {
            if_exists: true,
            identifier: Identifier::Compound(vec![
                "schema".to_string(),
                "my_second_trigger".to_string(),
            ]),
        });

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(second_expected_statement, second_actual_statement);
    }
}
