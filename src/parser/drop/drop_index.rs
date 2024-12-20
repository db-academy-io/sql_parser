use crate::{DropIndexStatement, Keyword, Parser, ParsingError, Statement};

use super::DropGenericStatementParser;

pub trait DropIndexStatementParser {
    fn parse_drop_index_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> DropIndexStatementParser for Parser<'a> {
    fn parse_drop_index_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Index)?;

        let (if_exists, identifier) = self.parse_drop_statement_generic()?;

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
