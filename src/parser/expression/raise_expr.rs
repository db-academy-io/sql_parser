use crate::parser::errors::ParsingError;
use crate::{Expression, Keyword, Parser, RaiseFunction, TokenType};

pub trait RaiseExpressionParser {
    fn parse_raise_expression(&mut self) -> Result<Expression, ParsingError>;
}

impl RaiseExpressionParser for Parser<'_> {
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
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::parser::ParsingError;
    use crate::select::test_utils::select_expr;
    use crate::{parser::test_utils::run_rainy_day_test, RaiseFunction};

    use crate::expression::test_utils::*;

    #[test]
    fn raise_ignore() {
        run_sunny_day_test(
            "SELECT RAISE(IGNORE);",
            select_expr(raise_expr(RaiseFunction::Ignore)).into(),
        );
    }

    #[test]
    fn raise_rollback() {
        run_sunny_day_test(
            "SELECT RAISE(ROLLBACK, 'Error');",
            select_expr(raise_expr(RaiseFunction::Rollback("'Error'".to_string()))).into(),
        );
    }

    #[test]
    fn raise_abort() {
        run_sunny_day_test(
            "SELECT RAISE(ABORT, 'Error');",
            select_expr(raise_expr(RaiseFunction::Abort("'Error'".to_string()))).into(),
        );
    }

    #[test]
    fn raise_fail() {
        run_sunny_day_test(
            "SELECT RAISE(FAIL, 'Error');",
            select_expr(raise_expr(RaiseFunction::Fail("'Error'".to_string()))).into(),
        );
    }

    #[test]
    fn raise_fail_with_empty_string() {
        run_rainy_day_test(
            "SELECT RAISE(FAIL);",
            ParsingError::UnexpectedToken(") at position 17".to_string()),
        );
    }
}
