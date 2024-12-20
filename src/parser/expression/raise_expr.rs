use crate::parser::errors::ParsingError;
use crate::{Expression, Keyword, Parser, RaiseFunction, TokenType};

pub trait RaiseExpressionParser {
    fn parse_raise_expression(&mut self) -> Result<Expression, ParsingError>;
}

impl<'a> RaiseExpressionParser for Parser<'a> {
    fn parse_raise_expression(&mut self) -> Result<Expression, ParsingError> {
        // Consume the RAISE keyword
        self.consume_as_keyword(Keyword::Raise)?;

        self.consume_as(TokenType::LeftParen)?;

        let raise = match self.peek_as_keyword()? {
            Keyword::Ignore => {
                self.consume_as_keyword(Keyword::Ignore)?;
                RaiseFunction::Ignore
            }
            Keyword::Rollback => {
                self.consume_as_keyword(Keyword::Rollback)?;
                self.consume_as(TokenType::Comma)?;

                let message = self.peek_as_string()?;
                // Consume the message string
                self.consume_token()?;

                RaiseFunction::Rollback(message)
            }
            Keyword::Abort => {
                self.consume_as_keyword(Keyword::Abort)?;
                self.consume_as(TokenType::Comma)?;

                let message = self.peek_as_string()?;
                // Consume the message string
                self.consume_token()?;

                RaiseFunction::Abort(message)
            }
            Keyword::Fail => {
                self.consume_as_keyword(Keyword::Fail)?;
                self.consume_as(TokenType::Comma)?;

                let message = self.peek_as_string()?;
                // Consume the message string
                self.consume_token()?;

                RaiseFunction::Fail(message)
            }
            keyword => return Err(ParsingError::UnexpectedKeyword(keyword)),
        };

        // Consume the enclosing right parenthesis
        self.consume_as(TokenType::RightParen)?;

        Ok(Expression::RaiseFunction(raise))
    }
}

#[cfg(test)]
mod raise_expression_tests {
    use crate::parser::ParsingError;
    use crate::{parser::test_utils::run_rainy_day_test, RaiseFunction};

    use crate::expression::test_utils::*;

    #[test]
    fn test_expression_raise_ignore() {
        run_sunny_day_expression_test(
            "SELECT RAISE(IGNORE);",
            &raise_expression(RaiseFunction::Ignore),
        );
    }

    #[test]
    fn test_expression_raise_rollback() {
        run_sunny_day_expression_test(
            "SELECT RAISE(ROLLBACK, 'Error');",
            &raise_expression(RaiseFunction::Rollback("'Error'".to_string())),
        );
    }

    #[test]
    fn test_expression_raise_abort() {
        run_sunny_day_expression_test(
            "SELECT RAISE(ABORT, 'Error');",
            &raise_expression(RaiseFunction::Abort("'Error'".to_string())),
        );
    }

    #[test]
    fn test_expression_raise_fail() {
        run_sunny_day_expression_test(
            "SELECT RAISE(FAIL, 'Error');",
            &raise_expression(RaiseFunction::Fail("'Error'".to_string())),
        );
    }

    #[test]
    fn test_expression_raise_fail_with_empty_string() {
        run_rainy_day_test(
            "SELECT RAISE(FAIL);",
            ParsingError::UnexpectedToken(")".to_string()),
        );
    }
}
