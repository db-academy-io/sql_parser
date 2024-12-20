use crate::parser::errors::ParsingError;
use crate::{DropViewStatement, Keyword, Parser, Statement};

use super::DropGenericStatementParser;

pub trait DropViewStatementParser {
    fn parse_drop_view_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> DropViewStatementParser for Parser<'a> {
    fn parse_drop_view_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::View)?;

        let (if_exists, identifier) = self.parse_drop_statement_generic()?;

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
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Identifier, Parser, Statement};

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
