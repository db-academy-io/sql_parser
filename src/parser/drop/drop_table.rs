use crate::parser::errors::ParsingError;
use crate::parser::IdentifierParser;
use crate::{DropTableStatement, Keyword, Parser, Statement};

pub trait DropTableStatementParser {
    fn parse_drop_table_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> DropTableStatementParser for Parser<'a> {
    fn parse_drop_table_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Table)?;

        let if_exists = self.parse_if_exists_clause()?;
        let identifier = self.parse_identifier()?;

        Ok(Statement::DropTable(DropTableStatement {
            if_exists,
            identifier,
        }))
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{ast::DropTableStatement, Identifier};

    pub fn drop_table_statement() -> DropTableStatement {
        DropTableStatement {
            if_exists: false,
            identifier: Identifier::Single("my_table".to_string()),
        }
    }
}

#[cfg(test)]
mod drop_table_tests {
    use crate::ast::DropTableStatement;
    use crate::parser::errors::ParsingError;
    use crate::parser::test_utils::{
        assert_statements_equal, run_rainy_day_test, run_sunny_day_test,
    };
    use crate::{Identifier, Parser, Statement};

    use super::test_utils::drop_table_statement;

    #[test]
    fn drop_table_test() {
        run_sunny_day_test(
            "DROP TABLE my_table;",
            Statement::DropTable(drop_table_statement()),
        )
    }

    #[test]
    fn drop_table_if_exists() {
        let mut expected_statement = drop_table_statement();
        expected_statement.if_exists = true;

        run_sunny_day_test(
            "DROP TABLE IF EXISTS my_table;",
            Statement::DropTable(expected_statement),
        )
    }

    #[test]
    fn drop_table_with_schema() {
        let mut expected_statement = drop_table_statement();
        expected_statement.identifier =
            Identifier::Compound(vec!["main".to_string(), "my_table".to_string()]);

        run_sunny_day_test(
            "DROP TABLE main.my_table;",
            Statement::DropTable(expected_statement),
        )
    }

    #[test]
    fn drop_table_missing_table_name() {
        run_rainy_day_test(
            "DROP TABLE ;",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn drop_table_missing_table_keyword() {
        run_rainy_day_test(
            "DROP my_table",
            ParsingError::UnexpectedToken("my_table".to_string()),
        );
    }

    #[test]
    fn drop_table_invalid_syntax() {
        run_rainy_day_test(
            "DROP TABLE IF my_table;",
            ParsingError::UnexpectedToken("Expected Exists keyword, got: my_table".to_string()),
        )
    }

    #[test]
    fn drop_table_extra_tokens() {
        run_rainy_day_test(
            "DROP TABLE my_table extra;",
            ParsingError::UnexpectedToken("extra".to_string()),
        );
    }

    #[test]
    fn drop_table_invalid_name() {
        run_rainy_day_test(
            "DROP TABLE 123invalid;",
            ParsingError::UnexpectedToken("Expected identifier".to_string()),
        );
    }

    #[test]
    fn drop_table_if_exists_missing_name() {
        run_rainy_day_test(
            "DROP TABLE IF EXISTS;",
            ParsingError::UnexpectedToken("Expected identifier".into()),
        );
    }

    #[test]
    fn drop_table_missing_semicolon() {
        run_sunny_day_test(
            "DROP TABLE my_table",
            Statement::DropTable(drop_table_statement()),
        );
    }

    #[test]
    fn multiple_drop_table_commands() {
        let mut parser =
            Parser::from("DROP TABLE my_table; DROP TABLE IF EXISTS schema.my_second_table;");
        let first_expected_statement = Statement::DropTable(drop_table_statement());
        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(first_expected_statement, first_actual_statement);

        let second_expected_statement = Statement::DropTable(DropTableStatement {
            if_exists: true,
            identifier: Identifier::Compound(vec![
                "schema".to_string(),
                "my_second_table".to_string(),
            ]),
        });
        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        assert_statements_equal(second_expected_statement, second_actual_statement);
    }
}
