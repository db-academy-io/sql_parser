use crate::{
    expression::ExpressionParser, AttachStatement, DetachStatement, Keyword, Parser, ParsingError,
    Statement,
};

use crate::expression::IdentifierParser;

use super::sqlite3_name::SQLite3NameParser;

pub trait AttachStatementParser {
    /// Parses a DETACH statement
    fn parse_detach_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a ATTACH statement
    fn parse_attach_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> AttachStatementParser for Parser<'a> {
    fn parse_attach_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Attach)?;

        // Consume the optional DATABASE keyword
        let _ = self.consume_as_keyword(Keyword::Database);

        let expression = self.parse_expression()?;

        self.consume_as_keyword(Keyword::As)?;
        let schema_name = self.parse_identifier()?;

        Ok(Statement::Attach(AttachStatement {
            expression,
            schema_name,
        }))
    }

    fn parse_detach_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume_as_keyword(Keyword::Detach)?;

        // Consume the optional DATABASE keyword
        let _ = self.consume_as_keyword(Keyword::Database);

        let schema_name = self.parse_sqlite3_name()?;

        self.finalize_statement_parsing()?;

        Ok(Statement::Detach(DetachStatement {
            schema_name: schema_name.to_string(),
        }))
    }
}

#[cfg(test)]
mod detach_statements_tests {
    use crate::ast::DetachStatement;
    use crate::parser::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Parser, ParsingError, Statement};

    #[test]
    fn test_detach_basic() {
        let sql = "DETACH DATABASE schema_name;";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "schema_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_without_database_keyword() {
        let sql = "DETACH schema_name;";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "schema_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_with_single_quoted_schema_name() {
        let sql = "DETACH DATABASE 'schema_name';";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "'schema_name'".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_with_double_quoted_schema_name() {
        let sql = "DETACH DATABASE \"schema_name\";";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "\"schema_name\"".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_missing_schema_name() {
        let sql = "DETACH DATABASE;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_detach_missing_schema_name_no_database() {
        let sql = "DETACH;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken(";".into()));
    }

    #[test]
    fn test_detach_invalid_syntax_extra_token() {
        let sql = "DETACH DATABASE schema_name extra;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("extra".into()));
    }

    #[test]
    fn test_detach_invalid_schema_name_number() {
        let sql = "DETACH DATABASE 123;";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "123".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_missing_semicolon() {
        let sql = "DETACH DATABASE schema_name";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "schema_name".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_multiple_statements() {
        let sql = "DETACH schema_name1; DETACH DATABASE 'schema_name2';";
        let mut parser = Parser::from(sql);

        let first_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let first_expected_statement = Statement::Detach(DetachStatement {
            schema_name: "schema_name1".to_string(),
        });
        assert_eq!(
            first_actual_statement, first_expected_statement,
            "Expected statement {:?}, got {:?}",
            first_expected_statement, first_actual_statement
        );

        let second_actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");
        let second_expected_statement = Statement::Detach(DetachStatement {
            schema_name: "'schema_name2'".to_string(),
        });
        assert_eq!(
            second_actual_statement, second_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_expected_statement, second_actual_statement
        );
    }

    #[test]
    fn test_detach_schema_name_with_spaces() {
        let sql = "DETACH DATABASE 'schema name with spaces';";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "'schema name with spaces'".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_schema_name_with_special_chars() {
        let sql = "DETACH DATABASE '[email protected]!';";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "'[email protected]!'".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_invalid_schema_name_no_quotes() {
        let sql = "DETACH DATABASE schema name;";
        run_rainy_day_test(sql, ParsingError::UnexpectedToken("name".into()));
    }

    #[test]
    fn test_detach_unterminated_string() {
        let sql = "DETACH DATABASE 'schema_name;";
        run_rainy_day_test(
            sql,
            ParsingError::TokenizerError("UnterminatedLiteral: 'schema_name;".into()),
        );
    }

    #[test]
    fn test_detach_with_numeric_schema_name_in_quotes() {
        let sql = "DETACH DATABASE '123';";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "'123'".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_with_escaped_quotes_in_schema_name() {
        let sql = "DETACH DATABASE 'schema''name';";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "'schema''name'".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_with_double_escaped_quotes_in_schema_name() {
        let sql = "DETACH DATABASE \"schema\"\"name\";";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "\"schema\"\"name\"".to_string(),
            }),
        );
    }

    #[test]
    fn test_detach_with_backticks_schema_name() {
        let sql = "DETACH DATABASE `schema_name`;";
        run_sunny_day_test(
            sql,
            Statement::Detach(DetachStatement {
                schema_name: "`schema_name`".to_string(),
            }),
        )
    }
}

#[cfg(test)]
mod attach_statement_tests {
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::{AttachStatement, Expression, Identifier, LiteralValue, Statement};

    #[test]
    fn test_attach_statement() {
        let sql = "ATTACH DATABASE 'test.db' AS db2;";
        run_sunny_day_test(
            sql,
            Statement::Attach(AttachStatement {
                expression: Expression::LiteralValue(LiteralValue::String("'test.db'".to_string())),
                schema_name: Identifier::Single("db2".to_string()),
            }),
        );
    }

    #[test]
    fn test_attach_statement_without_database_keyword() {
        let sql = "ATTACH 'sub_f' AS sub_name;";
        run_sunny_day_test(
            sql,
            Statement::Attach(AttachStatement {
                expression: Expression::LiteralValue(LiteralValue::String("'sub_f'".to_string())),
                schema_name: Identifier::Single("sub_name".to_string()),
            }),
        );
    }
}
