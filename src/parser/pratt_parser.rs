use once_cell::sync::Lazy;
use std::collections::HashMap;

use super::ParsingError;
use crate::{BinaryOp, Expression, Keyword, LiteralValue, Parser, TokenType, UnaryOp};

/// The precedence of the operators.
/// The precedence of an operator is the maximum precedence of the expressions
/// that can be formed with the operator.
///
/// The source of the precedence values is the SQLite grammar, see:
/// [SQL Language Expressions](https://www.sqlite.org/lang_expr.html)
static PRECEDENCE: Lazy<HashMap<TokenType, u8>> = Lazy::new(|| {
    let pairs = [
        (TokenType::Concat, 60),
        (TokenType::Star, 50),
        (TokenType::Slash, 50),
        (TokenType::Remainder, 50),
        (TokenType::Plus, 40),
        (TokenType::Minus, 40),
        (TokenType::BitAnd, 30),
        (TokenType::BitOr, 30),
        (TokenType::LeftShift, 30),
        (TokenType::RightShift, 30),
        (TokenType::GreaterThan, 20),
        (TokenType::LessThan, 20),
        (TokenType::GreaterEquals, 20),
        (TokenType::LessEquals, 20),
        (TokenType::Equals, 10),
        (TokenType::NotEquals, 10),
    ];

    pairs.iter().cloned().collect()
});

pub trait PrattParser {
    /// Parse an expression using Pratt's parsing algorithm
    /// `rbp` is the right binding power of the current operator
    fn parse_expression_pratt(&mut self, rbp: u8) -> Result<Expression, ParsingError>;

    /// Parse a prefix expression
    /// A prefix expression is an expression that does not have a left operand.
    fn parse_prefix(&mut self) -> Result<Expression, ParsingError>;

    /// Parse an infix expression
    /// An infix expression is an expression that has a left operand.
    fn parse_infix(&mut self, left: Expression, precedence: u8)
        -> Result<Expression, ParsingError>;

    /// Get the precedence of an operator
    fn get_precedence(&mut self, operator: &TokenType<'_>) -> u8;

    /// Get the precedence of the current operator
    fn current_precedence(&mut self) -> u8;
}

impl<'a> PrattParser for Parser<'a> {
    fn parse_expression_pratt(&mut self, precedence: u8) -> Result<Expression, ParsingError> {
        let mut expression = self.parse_prefix()?;

        loop {
            let next_precedence = self.current_precedence();

            if precedence >= next_precedence {
                break;
            }
            expression = self.parse_infix(expression, next_precedence)?;
        }
        Ok(expression)
    }

    /// Parse an infix expression
    fn parse_infix(
        &mut self,
        left: Expression,
        precedence: u8,
    ) -> Result<Expression, ParsingError> {
        let token = self.peek_token()?;
        let operator = BinaryOp::try_from(&token.token_type)?;
        // Consume the operator token
        self.consume_token()?;
        let right = self.parse_expression_pratt(precedence)?;
        Ok(Expression::BinaryOp(
            Box::new(left),
            operator,
            Box::new(right),
        ))
    }

    fn parse_prefix(&mut self) -> Result<Expression, ParsingError> {
        if let Ok(keyword) = self.peek_as_keyword() {
            if keyword == Keyword::Null
                || keyword == Keyword::CurrentTime
                || keyword == Keyword::CurrentDate
                || keyword == Keyword::CurrentTimestamp
            {
                self.consume_token()?;

                match keyword {
                    Keyword::Null => return Ok(Expression::LiteralValue(LiteralValue::Null)),
                    Keyword::CurrentTime => {
                        return Ok(Expression::LiteralValue(LiteralValue::CurrentTime))
                    }
                    Keyword::CurrentDate => {
                        return Ok(Expression::LiteralValue(LiteralValue::CurrentDate))
                    }
                    Keyword::CurrentTimestamp => {
                        return Ok(Expression::LiteralValue(LiteralValue::CurrentTimestamp))
                    }
                    _ => {}
                }
            }
        }

        let token = self.peek_token()?;

        match token.token_type {
            TokenType::Id(value) => {
                self.consume_token()?;
                Ok(Expression::Identifier(value.to_string()))
            }
            TokenType::Integer(value) => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::Number(
                    value.to_string(),
                )))
            }
            TokenType::Float(value) => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::Number(
                    value.to_string(),
                )))
            }
            TokenType::String(value) => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::String(
                    value.to_string(),
                )))
            }
            TokenType::Variable(value) => {
                self.consume_token()?;
                Ok(Expression::BindParameter(value.to_string()))
            }
            TokenType::LeftParen => {
                // Consume the left parenthesis
                self.consume_token()?;
                let expression = self.parse_expression_pratt(0)?;
                let token = self.peek_token()?;
                if token.token_type != TokenType::RightParen {
                    return Err(ParsingError::UnexpectedToken(format!(
                        "Expected right parenthesis, got: {}",
                        token.token_type
                    )));
                }
                // Consume the right parenthesis
                self.consume_token()?;
                Ok(expression)
            }
            TokenType::Minus => {
                // Consume the minus token
                self.consume_token()?;
                let pr = self.get_precedence(&TokenType::Minus);
                let expression = self.parse_expression_pratt(pr)?;
                Ok(Expression::UnaryOp(UnaryOp::Minus, Box::new(expression)))
            }
            TokenType::Plus => {
                // Consume the plus token
                self.consume_token()?;
                let pr = self.get_precedence(&TokenType::Plus);
                let expression = self.parse_expression_pratt(pr)?;
                Ok(Expression::UnaryOp(UnaryOp::Plus, Box::new(expression)))
            }
            TokenType::Blob(value) => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::Blob(
                    value.to_string(),
                )))
            }
            TokenType::True => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::Boolean(true)))
            }
            TokenType::False => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::Boolean(false)))
            }

            // TokenType:: => {
            //     self.consume_token()?;
            //     Ok(Expression::LiteralValue(LiteralValue::Null))
            // }
            _ => Err(ParsingError::UnexpectedToken(format!(
                "Unexpected token: {}",
                token.token_type
            ))),
        }
    }

    fn get_precedence(&mut self, operator: &TokenType<'_>) -> u8 {
        *PRECEDENCE.get(operator).unwrap_or(&0)
    }

    fn current_precedence(&mut self) -> u8 {
        if let Ok(token) = self.peek_token() {
            return self.get_precedence(&token.token_type);
        }
        0
    }
}

#[cfg(test)]
mod binary_op_expression_tests {
    use super::super::expression::test_utils::*;
    use crate::{BinaryOp, UnaryOp};

    #[test]
    fn test_unary_op_expression() {
        run_sunny_day_test(
            "SELECT -1;",
            &unary_op_expression(UnaryOp::Minus, numeric_literal_expression("1")),
        );
        run_sunny_day_test(
            "SELECT -+1;",
            &unary_op_expression(
                UnaryOp::Minus,
                unary_op_expression(UnaryOp::Plus, numeric_literal_expression("1")),
            ),
        );
        run_sunny_day_test(
            "SELECT ++1;",
            &unary_op_expression(
                UnaryOp::Plus,
                unary_op_expression(UnaryOp::Plus, numeric_literal_expression("1")),
            ),
        );
    }

    #[test]
    fn test_expression_binary_op_valid() {
        run_sunny_day_test(
            "SELECT 1 + 2;",
            &binary_op_expression(
                BinaryOp::Add,
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
            ),
        );
        run_sunny_day_test(
            "SELECT 1 - 2;",
            &binary_op_expression(
                BinaryOp::Sub,
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
            ),
        );
        run_sunny_day_test(
            "SELECT 1 * 2;",
            &binary_op_expression(
                BinaryOp::Mul,
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
            ),
        );
        run_sunny_day_test(
            "SELECT 1 / 2;",
            &binary_op_expression(
                BinaryOp::Div,
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
            ),
        );
        run_sunny_day_test(
            "SELECT 1 ++ 2;",
            &binary_op_expression(
                BinaryOp::Add,
                numeric_literal_expression("1"),
                unary_op_expression(UnaryOp::Plus, numeric_literal_expression("2")),
            ),
        );

        run_sunny_day_test(
            "SELECT 1 + 2 * 3;",
            &binary_op_expression(
                BinaryOp::Add,
                numeric_literal_expression("1"),
                binary_op_expression(
                    BinaryOp::Mul,
                    numeric_literal_expression("2"),
                    numeric_literal_expression("3"),
                ),
            ),
        );

        run_sunny_day_test(
            "SELECT 1 * 2 / 3;",
            &binary_op_expression(
                BinaryOp::Div,
                binary_op_expression(
                    BinaryOp::Mul,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                numeric_literal_expression("3"),
            ),
        );

        run_sunny_day_test(
            "SELECT 1 + 2 + 3;",
            &binary_op_expression(
                BinaryOp::Add,
                binary_op_expression(
                    BinaryOp::Add,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                numeric_literal_expression("3"),
            ),
        );

        run_sunny_day_test(
            "SELECT 1 * 2 * 3;",
            &binary_op_expression(
                BinaryOp::Mul,
                binary_op_expression(
                    BinaryOp::Mul,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                numeric_literal_expression("3"),
            ),
        );

        run_sunny_day_test(
            "SELECT 1 + 2 * 3 - 4",
            &binary_op_expression(
                BinaryOp::Sub,
                binary_op_expression(
                    BinaryOp::Add,
                    numeric_literal_expression("1"),
                    binary_op_expression(
                        BinaryOp::Mul,
                        numeric_literal_expression("2"),
                        numeric_literal_expression("3"),
                    ),
                ),
                numeric_literal_expression("4"),
            ),
        );
    }
}
