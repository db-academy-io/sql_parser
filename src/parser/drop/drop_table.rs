use crate::parser::errors::ParsingError;
use crate::{DropTableStatement, Keyword, Parser, Statement};

use super::DropGenericStatementParser;

pub trait DropTableStatementParser {
    fn parse_drop_table_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> DropTableStatementParser for Parser<'a> {
    fn parse_drop_table_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Table)?;

        let (if_exists, identifier) = self.parse_drop_statement_generic()?;

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
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Identifier, Parser, Statement};

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
