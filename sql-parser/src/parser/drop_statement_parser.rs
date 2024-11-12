use super::{Keyword, Parser};
use crate::{DropTableStatement, ParsingError, Statement, TokenType};

pub trait DropTableStatementParser {
    fn parse_drop_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_drop_table_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> DropTableStatementParser for Parser<'a> {
    /// Parse DROP statement
    /// The DROP statement consists of the
    ///   - DROP TABLE statements
    ///   - DROP VIEW statements
    ///   - DROP INDEX statements
    ///   - DROP TRIGGER statements
    fn parse_drop_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the DROP keyword
        self.consume_token()?;
        let keyword = self.peek_as_keyword()?;

        match keyword {
            Keyword::Table => self.parse_drop_table_statement(),
            Keyword::View => todo!(),
            Keyword::Index => todo!(),
            Keyword::Trigger => todo!(),
            _ => Err(ParsingError::UnexpectedKeyword(keyword)),
        }
    }

    /// Parses DROP TABLE statementes according SQLite documentation
    fn parse_drop_table_statement(&mut self) -> Result<Statement, ParsingError<'a>> {
        // Consume the TABLE keyword
        self.consume_token()?;

        let mut if_exists = false;

        // Parse optional [IF EXISTS] part of the statement
        if let Ok(Keyword::If) = self.peek_as_keyword() {
            // Consume the IF keyword
            self.consume_token()?;
            match self.peek_as_keyword()? {
                Keyword::Exists => {
                    if_exists = true;
                    // Consume the EXISTS keyword
                    self.consume_token()?;
                }
                _ => return Err(ParsingError::ExpectedKeyword(Keyword::Exists)),
            }
        }

        let mut schema_name: Option<String> = None;
        let mut table_name: String = self.peek_as_id()?.to_string();

        // consume the first id token (which might be the table name)
        self.consume_token()?;

        if let Ok(token) = self.peek_token() {
            if token.token_type == TokenType::Dot {
                // in this case we need to parse schema and name

                // consume the DOT token
                self.consume_token()?;

                // swap: The first ID token was for the schema name
                schema_name = Some(table_name);
                table_name = self.peek_as_id()?.to_string();

                // consume table_name token
                self.consume_token()?;
            }
        }

        if let Ok(token) = self.peek_token() {
            if token.token_type != TokenType::Semi {
                return Err(ParsingError::UnexpectedToken(token.clone()));
            }
        }

        if let Ok(token) = self.peek_token() {
            if token.token_type == TokenType::Semi {
                // consume the ';' token
                self.consume_token()?;
            }
        }

        Ok(Statement::DropTable(DropTableStatement {
            if_exists,
            schema_name,
            table_name,
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::DropTableStatement;
    use crate::{Parser, ParsingError, Statement, Token, TokenType};

    fn run_sunny_day_test<'a>(sql: &'a str, expected_statement: Statement) {
        let mut parser = Parser::from(sql);
        let actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        // Verify that the statements are matches
        assert_eq!(
            expected_statement, actual_statement,
            "Expected statement {:?}, got {:?}",
            expected_statement, actual_statement
        );
    }

    fn run_rainy_day_test<'a>(sql: &'a str, expected_error: ParsingError) {
        let mut parser = Parser::from(sql);
        let actual_error = parser
            .parse_statement()
            .expect_err("Expected Parsing Error, got parsed Statement");

        assert_eq!(
            actual_error, expected_error,
            "Expected error {:?}, got {:?}",
            actual_error, expected_error,
        );
    }

    #[test]
    fn test_drop_table_valid() {
        let sql = "DROP TABLE my_table;";
        run_sunny_day_test(
            sql,
            Statement::DropTable(DropTableStatement::table_name("my_table".to_string())),
        )
    }

    #[test]
    fn test_drop_table_if_exists() {
        let sql = "DROP TABLE IF EXISTS my_table;";
        run_sunny_day_test(
            sql,
            Statement::DropTable(DropTableStatement {
                if_exists: true,
                schema_name: None,
                table_name: "my_table".to_string(),
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
                schema_name: Some("main".to_string()),
                table_name: "my_table".to_string(),
            }),
        )
    }

    #[test]
    fn test_drop_table_missing_table_name() {
        let sql = "DROP TABLE ;";
        run_rainy_day_test(sql, ParsingError::UnexpectedEOF);
    }

    #[test]
    fn test_drop_table_missing_table_keyword() {
        let sql = "DROP my_table;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken(Token {
                token_type: TokenType::Id("my_table".into()),
                position: 5,
            }),
        );
    }

    #[test]
    fn test_drop_table_invalid_syntax() {
        let sql = "DROP TABLE IF my_table;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken(Token {
                token_type: TokenType::Id("my_table".into()),
                position: 14,
            }),
        )
    }

    #[test]
    fn test_drop_table_extra_tokens() {
        let sql = "DROP TABLE my_table extra;";
        run_rainy_day_test(
            sql,
            ParsingError::UnexpectedToken(Token {
                token_type: TokenType::Id("extra"),
                position: 20,
            }),
        );
    }

    #[test]
    fn test_drop_table_invalid_name() {
        let sql = "DROP TABLE 123invalid;";
        run_rainy_day_test(sql, ParsingError::BadNumber);
    }

    #[test]
    fn test_drop_table_if_exists_missing_name() {
        let sql = "DROP TABLE IF EXISTS;";
        run_rainy_day_test(sql, ParsingError::UnexpectedEOF);
    }

    #[test]
    fn test_drop_table_missing_semicolon() {
        let sql = "DROP TABLE my_table";
        run_sunny_day_test(
            sql,
            Statement::DropTable(DropTableStatement {
                if_exists: false,
                schema_name: None,
                table_name: "my_table".to_string(),
            }),
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
            Statement::DropTable(DropTableStatement::table_name("my_table".to_string()));

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
            schema_name: Some("schema".to_string()),
            table_name: "my_second_table".to_string(),
        });

        // Verify that the statements are matches
        assert_eq!(
            second_actual_statement, secont_expected_statement,
            "Expected statement {:?}, got {:?}",
            second_actual_statement, secont_expected_statement
        );
    }
}
