use crate::parser::errors::ParsingError;
use crate::parser::IdentifierParser;
use crate::{DropViewStatement, Keyword, Parser, Statement};

pub trait DropViewStatementParser {
    fn parse_drop_view_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl DropViewStatementParser for Parser<'_> {
    fn parse_drop_view_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::View)?;

        let if_exists = self.parse_if_exists_clause()?;
        let identifier = self.parse_identifier()?;

        Ok(Statement::DropView(DropViewStatement {
            if_exists,
            identifier,
        }))
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{ast::DropViewStatement, Identifier};

    pub fn drop_view_statement() -> DropViewStatement {
        DropViewStatement {
            if_exists: false,
            identifier: Identifier::Single("my_view".to_string()),
        }
    }
}

#[cfg(test)]
mod drop_view_tests {
    use crate::ast::DropViewStatement;
    use crate::parser::errors::ParsingError;
    use crate::parser::test_utils::{
        assert_statements_equal, run_rainy_day_test, run_sunny_day_test,
    };
    use crate::{Identifier, Parser, Statement};

    use super::test_utils::drop_view_statement;

    #[test]
    fn drop_view_test() {
        run_sunny_day_test(
            "DROP VIEW my_view;",
            Statement::DropView(drop_view_statement()),
        )
    }

    #[test]
    fn drop_view_if_exists() {
        let mut expected_statement = drop_view_statement();
        expected_statement.if_exists = true;

        run_sunny_day_test(
            "DROP VIEW IF EXISTS my_view;",
            Statement::DropView(expected_statement),
        )
    }

    #[test]
    fn drop_view_with_schema() {
        let mut expected_statement = drop_view_statement();
        expected_statement.identifier =
            Identifier::Compound(vec!["main".to_string(), "my_view".to_string()]);

        run_sunny_day_test(
            "DROP VIEW main.my_view;",
            Statement::DropView(expected_statement),
        )
    }

    #[test]
    fn drop_view_missing_view_name() {
        run_rainy_day_test(
            "DROP VIEW ;",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );

        run_rainy_day_test(
            "DROP VIEW",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn drop_view_missing_view_keyword() {
        run_rainy_day_test(
            "DROP my_view;",
            ParsingError::UnexpectedToken("my_view at position 5".into()),
        );
    }

    #[test]
    fn drop_view_invalid_syntax() {
        run_rainy_day_test(
            "DROP VIEW IF my_view;",
            ParsingError::UnexpectedToken(
                "Expected Exists keyword, got: my_view at position 13".into(),
            ),
        )
    }

    #[test]
    fn drop_view_extra_tokens() {
        run_rainy_day_test(
            "DROP VIEW my_view extra;",
            ParsingError::UnexpectedToken("extra at position 18".into()),
        );
    }

    #[test]
    fn drop_view_invalid_name() {
        run_rainy_day_test(
            "DROP VIEW 123invalid;",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn drop_view_if_exists_missing_name() {
        run_rainy_day_test(
            "DROP VIEW IF EXISTS;",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn drop_view_missing_semicolon() {
        let mut expected_statement = drop_view_statement();
        expected_statement.if_exists = true;

        run_sunny_day_test(
            "DROP VIEW IF EXISTS my_view",
            Statement::DropView(expected_statement),
        );
    }

    #[test]
    fn multiple_drop_view_commands() {
        let sql = "DROP VIEW my_view; DROP VIEW IF EXISTS schema.my_second_view;";

        let mut parser = Parser::from(sql);
        let first_expected_statement = Statement::DropView(drop_view_statement());

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(first_expected_statement, first_actual_statement);

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

        assert_statements_equal(second_expected_statement, second_actual_statement);
    }
}
