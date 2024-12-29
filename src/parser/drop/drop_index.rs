use crate::parser::errors::ParsingError;
use crate::parser::IdentifierParser;
use crate::{DropIndexStatement, Keyword, Parser, Statement};

pub trait DropIndexStatementParser {
    fn parse_drop_index_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> DropIndexStatementParser for Parser<'a> {
    fn parse_drop_index_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Index)?;

        let if_exists = self.parse_if_exists_clause()?;
        let identifier = self.parse_identifier()?;

        Ok(Statement::DropIndex(DropIndexStatement {
            if_exists,
            identifier,
        }))
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{ast::DropIndexStatement, Identifier};

    pub fn drop_index_statement() -> DropIndexStatement {
        DropIndexStatement {
            if_exists: false,
            identifier: Identifier::Single("my_index".to_string()),
        }
    }
}

#[cfg(test)]
mod drop_index_tests {
    use crate::ast::DropIndexStatement;
    use crate::parser::errors::ParsingError;
    use crate::parser::test_utils::{
        assert_statements_equal, run_rainy_day_test, run_sunny_day_test,
    };
    use crate::{Identifier, Parser, Statement};

    use super::test_utils::drop_index_statement;

    #[test]
    fn drop_index_test() {
        run_sunny_day_test(
            "DROP INDEX my_index;",
            Statement::DropIndex(drop_index_statement()),
        )
    }

    #[test]
    fn drop_index_if_exists() {
        let mut expected_statement = drop_index_statement();
        expected_statement.if_exists = true;
        run_sunny_day_test(
            "DROP INDEX IF EXISTS my_index;",
            Statement::DropIndex(expected_statement),
        )
    }

    #[test]
    fn drop_index_with_schema() {
        let mut expected_statement = drop_index_statement();
        expected_statement.identifier =
            Identifier::Compound(vec!["main".to_string(), "my_index".to_string()]);

        run_sunny_day_test(
            "DROP INDEX main.my_index;",
            Statement::DropIndex(expected_statement),
        )
    }

    #[test]
    fn drop_index_missing_index_name() {
        run_rainy_day_test(
            "DROP INDEX ;",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );

        run_rainy_day_test(
            "DROP INDEX",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn drop_index_missing_index_keyword() {
        run_rainy_day_test(
            "DROP my_index;",
            ParsingError::UnexpectedToken("my_index at position 5".into()),
        );
    }

    #[test]
    fn drop_index_invalid_syntax() {
        run_rainy_day_test(
            "DROP INDEX IF my_index;",
            ParsingError::UnexpectedToken(
                "Expected Exists keyword, got: my_index at position 14".into(),
            ),
        )
    }

    #[test]
    fn drop_index_extra_tokens() {
        run_rainy_day_test(
            "DROP INDEX my_index extra;",
            ParsingError::UnexpectedToken("extra at position 20".into()),
        );
    }

    #[test]
    fn drop_index_invalid_name() {
        run_rainy_day_test(
            "DROP INDEX 123invalid;",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn drop_index_if_exists_missing_name() {
        run_rainy_day_test(
            "DROP INDEX IF EXISTS;",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn drop_index_missing_semicolon() {
        run_sunny_day_test(
            "DROP INDEX my_index",
            Statement::DropIndex(drop_index_statement()),
        );
    }

    #[test]
    fn multiple_drop_index_commands() {
        let sql = "DROP INDEX my_index; DROP INDEX IF EXISTS schema.my_second_index;";

        let mut parser = Parser::from(sql);
        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        let first_expected_statement = Statement::DropIndex(drop_index_statement());

        assert_statements_equal(first_expected_statement, first_actual_statement);

        let second_expected_statement = Statement::DropIndex(DropIndexStatement {
            if_exists: true,
            identifier: Identifier::Compound(vec![
                "schema".to_string(),
                "my_second_index".to_string(),
            ]),
        });

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(second_expected_statement, second_actual_statement);
    }
}
