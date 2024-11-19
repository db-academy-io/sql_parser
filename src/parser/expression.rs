use crate::Expression;

use super::{Parser, ParsingError, TokenType};
use crate::ast::LiteralValue;
use crate::Keyword;

/// Trait for parsing expressions
/// The expression documentation can be found here:
/// https://www.sqlite.org/lang_expr.html
pub trait ExpressionParser {
    fn parse_expression(&mut self) -> Result<Expression, ParsingError>;

    fn parse_literal_value(&mut self) -> Result<LiteralValue, ParsingError>;
}

impl<'a> ExpressionParser for Parser<'a> {
    /// Parse an expression
    fn parse_expression(&mut self) -> Result<Expression, ParsingError> {
        let is_literal_value = self.parse_literal_value();
        if let Ok(literal_value) = is_literal_value {
            return Ok(Expression::LiteralValue(literal_value));
            // match self.finalize_statement_parsing() {
            //     Ok(()) => return Ok(Expression::LiteralValue(literal_value)),
            //     Err(_) => {
            //         let token = self.peek_token()?;
            //         return Err(ParsingError::UnexpectedToken(token.to_string()));
            //     }
            // }
        }
        todo!()
    }

    /// Parse a literal value
    /// See details [sqlite-literal]
    ///
    /// [sqlite-literal]: https://www.sqlite.org/lang_expr.html#literal_values
    fn parse_literal_value(&mut self) -> Result<LiteralValue, ParsingError> {
        // Check if it's a keyword literal (NULL, CURRENT_TIME, CURRENT_DATE, CURRENT_TIMESTAMP)
        if let Ok(keyword) = self.peek_as_keyword() {
            if keyword == Keyword::Null
                || keyword == Keyword::CurrentTime
                || keyword == Keyword::CurrentDate
                || keyword == Keyword::CurrentTimestamp
            {
                self.consume_token()?;
            }

            match keyword {
                Keyword::Null => return Ok(LiteralValue::Null),
                Keyword::CurrentTime => return Ok(LiteralValue::CurrentTime),
                Keyword::CurrentDate => return Ok(LiteralValue::CurrentDate),
                Keyword::CurrentTimestamp => return Ok(LiteralValue::CurrentTimestamp),
                _ => {}
            }
        }

        // Check if it's a TRUE literal
        if self.peek_as(TokenType::True).is_ok() {
            // Consume the TRUE token
            self.consume_token()?;
            return Ok(LiteralValue::Boolean(true));
        }
        // Check if it's a FALSE literal
        if self.peek_as(TokenType::False).is_ok() {
            // Consume the FALSE token
            self.consume_token()?;
            return Ok(LiteralValue::Boolean(false));
        }

        let token = self.peek_token()?;

        match token.token_type {
            TokenType::String(value) => {
                self.consume_token()?;
                Ok(LiteralValue::String(value.to_string()))
            }
            TokenType::Blob(value) => {
                self.consume_token()?;
                Ok(LiteralValue::Blob(value.to_string()))
            }
            TokenType::Integer(value) => {
                self.consume_token()?;
                Ok(LiteralValue::Number(value.to_string()))
            }
            TokenType::Float(value) => {
                self.consume_token()?;
                Ok(LiteralValue::Number(value.to_string()))
            }
            _ => Err(ParsingError::UnexpectedToken(token.to_string())),
        }
    }
}

#[cfg(test)]
mod literal_value_expression_tests {
    use crate::ast::Expression;
    use crate::ast::SelectItem;
    use crate::{Parser, Statement};

    // TODO: Make this generic, and move to test_utils module
    fn run_sunny_day_test(sql: &str, expected_expression: &Expression) {
        let mut parser = Parser::from(sql);
        let actual_expression = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        match actual_expression {
            Statement::Select(select_statement) => {
                assert_eq!(
                    1,
                    select_statement.columns.len(),
                    "Expected 1 column, got {:?}",
                    select_statement.columns.len()
                );
                let select_item = &select_statement.columns[0];

                match select_item {
                    SelectItem::Expression(actual_expression) => {
                        assert_eq!(
                            expected_expression, actual_expression,
                            "Expected expression {:?}, got {:?}",
                            expected_expression, actual_expression
                        );
                    }
                    _ => panic!("Expected Expression, got {:?}", select_item),
                }
            }
            _ => panic!("Expected Select statement, got {:?}", actual_expression),
        }
    }

    fn numeric_literal_expression(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::Number(value.to_string()))
    }

    fn string_literal_expression(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::String(value.to_string()))
    }

    fn blob_literal_expression(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::Blob(value.to_string()))
    }

    fn boolean_literal_expression(value: bool) -> Expression {
        Expression::LiteralValue(LiteralValue::Boolean(value))
    }

    fn null_literal_expression() -> Expression {
        Expression::LiteralValue(LiteralValue::Null)
    }

    #[test]
    fn test_expression_literal_value_valid() {
        run_sunny_day_test("SELECT 1;", &numeric_literal_expression("1"));

        run_sunny_day_test("SELECT 1.2;", &numeric_literal_expression("1.2"));

        run_sunny_day_test(
            "SELECT 1.234567890;",
            &numeric_literal_expression("1.234567890"),
        );

        run_sunny_day_test(
            "SELECT 'Hello, world!';",
            &string_literal_expression("'Hello, world!'"),
        );

        run_sunny_day_test(
            "SELECT X'DEADBEEF';",
            &blob_literal_expression("X'DEADBEEF'"),
        );

        run_sunny_day_test("SELECT TRUE;", &boolean_literal_expression(true));

        run_sunny_day_test("SELECT FALSE;", &boolean_literal_expression(false));

        run_sunny_day_test("SELECT NULL;", &null_literal_expression());

        run_sunny_day_test(
            "SELECT CURRENT_TIME;",
            &Expression::LiteralValue(LiteralValue::CurrentTime),
        );

        run_sunny_day_test(
            "SELECT CURRENT_DATE;",
            &Expression::LiteralValue(LiteralValue::CurrentDate),
        );

        run_sunny_day_test(
            "SELECT CURRENT_TIMESTAMP;",
            &Expression::LiteralValue(LiteralValue::CurrentTimestamp),
        );
    }

    use super::*;
}
