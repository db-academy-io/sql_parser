use super::{
    case_expr::CaseExpressionParser, cast_expr::CastExpressionParser,
    exists_expr::ExistsExpressionParser, precedence::get_precedence,
    raise_expr::RaiseExpressionParser, ExpressionParser, FunctionParser,
};
use crate::{
    parser::ParsingError, BinaryOp, Expression, IdentifierParser, Keyword, LiteralValue, Parser,
    TokenType, UnaryOp,
};

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

    /// Parse an expression which starts with the NOT keyword
    fn parse_not_expression(&mut self) -> Result<Expression, ParsingError>;
}

impl<'a> PrattParser for Parser<'a> {
    /// Parse an expression using Pratt's parsing algorithm
    fn parse_expression_pratt(&mut self, precedence: u8) -> Result<Expression, ParsingError> {
        let mut expression = self.parse_prefix()?;

        loop {
            let current_token = self.peek_token()?;
            let next_precedence = get_precedence(&current_token.token_type);

            if precedence >= next_precedence {
                break;
            }
            expression = self.parse_infix(expression, next_precedence)?;
        }
        Ok(expression)
    }

    /// Parse an infix expression
    /// An infix expression is an expression that has a left operand.
    fn parse_infix(
        &mut self,
        left: Expression,
        precedence: u8,
    ) -> Result<Expression, ParsingError> {
        let token = self.peek_token()?;
        let operator = BinaryOp::try_from(&token.token_type)?;
        // Consume the operator token
        self.consume_as(token.token_type)?;

        let right = self.parse_expression_pratt(precedence)?;
        Ok(Expression::BinaryOp(
            Box::new(left),
            operator,
            Box::new(right),
        ))
    }

    /// Parse a prefix expression
    /// A prefix expression is an expression that does not have a left operand.
    fn parse_prefix(&mut self) -> Result<Expression, ParsingError> {
        if let Ok(keyword) = self.peek_as_keyword() {
            // Check if it's one of the special expressions
            if keyword == Keyword::Null
                || keyword == Keyword::CurrentTime
                || keyword == Keyword::CurrentDate
                || keyword == Keyword::CurrentTimestamp
            {
                self.consume_as_keyword(keyword)?;

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
            match keyword {
                Keyword::Case => return CaseExpressionParser::parse_case_expression(self),
                Keyword::Cast => return CastExpressionParser::parse_cast_expression(self),
                Keyword::Not => return self.parse_not_expression(),
                Keyword::Exists => {
                    return ExistsExpressionParser::parse_exists_expression(self, false)
                }
                Keyword::Raise => return RaiseExpressionParser::parse_raise_expression(self),
                Keyword::Null
                | Keyword::CurrentTime
                | Keyword::CurrentDate
                | Keyword::CurrentTimestamp => {
                    // These are literals, so we don't need to do anything here
                    // as the pratt parser will handle them
                }
                _ => return Err(ParsingError::UnexpectedKeyword(keyword)),
            }
        } else if self.peek_as(TokenType::LeftParen).is_ok() {
            // Consume the left parenthesis
            self.consume_as(TokenType::LeftParen)?;

            // TODO: Handle EXISTS (SELECT ...)
            let expression = if let Ok(Keyword::Select) = self.peek_as_keyword() {
                ExistsExpressionParser::parse_exists_expression(self, false)?
            } else {
                Expression::ExpressionList(self.parse_comma_separated_expressions()?)
            };
            // The right parenthesis must be in the last token in the expression list
            self.consume_as(TokenType::RightParen)?;
            return Ok(expression);
        }
        let token = self.peek_token()?;

        match token.token_type {
            TokenType::Id(_) => {
                let id = self.parse_identifier()?;

                if self.peek_as(TokenType::LeftParen).is_ok() {
                    self.parse_function(id)
                } else {
                    Ok(Expression::Identifier(id))
                }
            }
            TokenType::Integer(value) => {
                self.consume_as_number()?;
                Ok(Expression::LiteralValue(LiteralValue::Number(
                    value.to_string(),
                )))
            }
            TokenType::Float(value) => {
                self.consume_as_number()?;
                Ok(Expression::LiteralValue(LiteralValue::Number(
                    value.to_string(),
                )))
            }
            TokenType::String(value) => {
                self.consume_as_string()?;
                Ok(Expression::LiteralValue(LiteralValue::String(
                    value.to_string(),
                )))
            }
            TokenType::Variable(value) => {
                self.consume_token()?;
                Ok(Expression::BindParameter(value.to_string()))
            }
            TokenType::LeftParen => {
                self.consume_as(TokenType::LeftParen)?;
                let expression = self.parse_expression_pratt(0)?;
                let token = self.peek_token()?;
                if token.token_type != TokenType::RightParen {
                    return Err(ParsingError::UnexpectedToken(format!(
                        "Expected right parenthesis, got: {}",
                        token.token_type
                    )));
                }
                self.consume_as(TokenType::RightParen)?;
                Ok(expression)
            }
            TokenType::Minus => {
                self.consume_as(TokenType::Minus)?;
                let pr = get_precedence(&TokenType::Minus);
                let expression = self.parse_expression_pratt(pr)?;
                Ok(Expression::UnaryOp(UnaryOp::Minus, Box::new(expression)))
            }
            TokenType::Plus => {
                self.consume_as(TokenType::Plus)?;
                let pr = get_precedence(&TokenType::Plus);
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
                self.consume_as(TokenType::True)?;
                Ok(Expression::LiteralValue(LiteralValue::Boolean(true)))
            }
            TokenType::False => {
                self.consume_as(TokenType::False)?;
                Ok(Expression::LiteralValue(LiteralValue::Boolean(false)))
            }
            _ => Err(ParsingError::UnexpectedToken(format!(
                "Unexpected token: {}",
                token.token_type
            ))),
        }
    }

    fn parse_not_expression(&mut self) -> Result<Expression, ParsingError> {
        self.consume_as_keyword(Keyword::Not)?;

        if let Ok(Keyword::Exists) = self.peek_as_keyword() {
            ExistsExpressionParser::parse_exists_expression(self, true)
        } else {
            Ok(Expression::UnaryOp(
                UnaryOp::Not,
                Box::new(self.parse_expression()?),
            ))
        }
    }
}
