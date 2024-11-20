use crate::parser::expression;
use crate::{Expression, UnaryOp};

use super::{Parser, ParsingError, TokenType};
use crate::ast::LiteralValue;
use crate::Keyword;

/// Trait for parsing expressions
/// The expression documentation can be found here:
/// https://www.sqlite.org/lang_expr.html
pub trait ExpressionParser {
    fn is_end_of_expression(&mut self) -> bool;

    fn parse_expression(&mut self) -> Result<Expression, ParsingError>;

    fn parse_literal_value(&mut self) -> Result<LiteralValue, ParsingError>;

    fn parse_bind_parameter(&mut self) -> Result<String, ParsingError>;

    fn parse_identifier(&mut self) -> Result<String, ParsingError>;

    fn parse_compound_identifier(&mut self) -> Result<Vec<String>, ParsingError>;

    fn parse_unary_op(&mut self) -> Result<Expression, ParsingError>;
}

impl<'a> ExpressionParser for Parser<'a> {
    fn is_end_of_expression(&mut self) -> bool {
        self.peek_as(TokenType::Semi).is_ok()
    }

    /// Parse an expression
    /// Parse an expression
    fn parse_expression(&mut self) -> Result<Expression, ParsingError> {
        // Check if it's a literal value
        if let Ok(literal_value) = self.parse_literal_value() {
            return Ok(Expression::LiteralValue(literal_value));
        }

        // Check if it's a bind parameter
        if let Ok(is_bind_parameter) = self.parse_bind_parameter() {
            return Ok(Expression::BindParameter(is_bind_parameter));
        }

        // Check if it's a compound identifier
        if let Ok(compound_identifier) = self.parse_compound_identifier() {
            if compound_identifier.len() == 1 {
                return Ok(Expression::Identifier(compound_identifier[0].clone()));
            } else {
                return Ok(Expression::CompoundIdentifier(compound_identifier));
            }
        }

        // Check if it's an unary operation
        if let Ok(expression) = self.parse_unary_op() {
            return Ok(expression);
        }

        todo!()
    }

    /// Parse an identifier
    fn parse_identifier(&mut self) -> Result<String, ParsingError> {
        let token = self.peek_token()?;
        match token.token_type {
            TokenType::Id(value) => {
                // Consume the identifier token
                self.consume_token()?;
                Ok(value.to_string())
            }
            _ => Err(ParsingError::UnexpectedToken(token.to_string())),
        }
    }

    /// Parse a compound identifier
    fn parse_compound_identifier(&mut self) -> Result<Vec<String>, ParsingError> {
        let mut identifiers = Vec::new();

        while let Ok(identifier) = self.parse_identifier() {
            identifiers.push(identifier);

            if self.peek_as(TokenType::Dot).is_ok() {
                // Consume the dot token
                self.consume_token()?;
            } else {
                break;
            }
        }

        if identifiers.is_empty() {
            return Err(ParsingError::UnexpectedToken(
                "Expected identifier".to_string(),
            ));
        }

        Ok(identifiers)
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

    /// Parse a bind parameter
    fn parse_bind_parameter(&mut self) -> Result<String, ParsingError> {
        let token = self.peek_token()?;
        match token.token_type {
            TokenType::Variable(value) => {
                self.consume_token()?;
                Ok(value.to_string())
            }
            _ => Err(ParsingError::UnexpectedToken(token.to_string())),
        }
    }

    /// Parse a unary operation
    fn parse_unary_op(&mut self) -> Result<Expression, ParsingError> {
        // Check if it's a plus operation
        if self.peek_as(TokenType::Plus).is_ok() {
            self.consume_token()?;
            return Ok(Expression::UnaryOp(
                UnaryOp::Plus,
                Box::new(self.parse_expression()?),
            ));
        }

        // Check if it's a minus operation
        if self.peek_as(TokenType::Minus).is_ok() {
            self.consume_token()?;
            return Ok(Expression::UnaryOp(
                UnaryOp::Minus,
                Box::new(self.parse_expression()?),
            ));
        }

        let token = self.peek_token()?;
        Err(ParsingError::UnexpectedToken(token.to_string()))
    }
}

#[cfg(test)]
mod test_utils {
    use crate::ast::{Expression, SelectItem};
    use crate::{LiteralValue, Parser, Statement, UnaryOp};

    // TODO: Make this generic, and move to test_utils module
    pub fn run_sunny_day_test(sql: &str, expected_expression: &Expression) {
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

    pub fn numeric_literal_expression(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::Number(value.to_string()))
    }

    pub fn string_literal_expression(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::String(value.to_string()))
    }

    pub fn blob_literal_expression(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::Blob(value.to_string()))
    }

    pub fn boolean_literal_expression(value: bool) -> Expression {
        Expression::LiteralValue(LiteralValue::Boolean(value))
    }

    pub fn null_literal_expression() -> Expression {
        Expression::LiteralValue(LiteralValue::Null)
    }

    pub fn bind_parameter_expression(value: &str) -> Expression {
        Expression::BindParameter(value.to_string())
    }

    pub fn identifier_expression(value: &str) -> Expression {
        Expression::Identifier(value.to_string())
    }

    pub fn compound_identifier_expression(values: &[&str]) -> Expression {
        Expression::CompoundIdentifier(values.iter().map(|s| s.to_string()).collect())
    }

    pub fn unary_op_expression(op: UnaryOp, value: Expression) -> Expression {
        Expression::UnaryOp(op, Box::new(value))
    }
}

#[cfg(test)]
mod literal_value_expression_tests {
    use super::test_utils::*;
    use crate::ast::{Expression, LiteralValue};

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
}

#[cfg(test)]
mod bind_parameter_expression_tests {
    use super::test_utils::*;

    #[test]
    fn test_expression_bind_parameter_valid() {
        run_sunny_day_test("SELECT ?;", &bind_parameter_expression("?"));
        run_sunny_day_test("SELECT ?1;", &bind_parameter_expression("?1"));
        run_sunny_day_test("SELECT :name;", &bind_parameter_expression(":name"));
        run_sunny_day_test("SELECT @var;", &bind_parameter_expression("@var"));
        run_sunny_day_test("SELECT $value;", &bind_parameter_expression("$value"));
        run_sunny_day_test("SELECT #param;", &bind_parameter_expression("#param"));

        // TODO: Add tests for invalid bind parameters
    }
}

#[cfg(test)]
mod identifier_expression_tests {
    use super::test_utils::*;

    #[test]
    fn test_expression_identifier_valid() {
        run_sunny_day_test("SELECT id;", &identifier_expression("id"));
        run_sunny_day_test(
            "SELECT table1.column1;",
            &compound_identifier_expression(&["table1", "column1"]),
        );
        run_sunny_day_test(
            "SELECT schema1.table1.column1;",
            &compound_identifier_expression(&["schema1", "table1", "column1"]),
        );
    }
}

#[cfg(test)]
mod unary_op_expression_tests {
    use crate::UnaryOp;

    use super::test_utils::*;

    #[test]
    fn test_expression_unary_op_valid() {
        run_sunny_day_test(
            "SELECT +1;",
            &unary_op_expression(UnaryOp::Plus, numeric_literal_expression("1")),
        );
        run_sunny_day_test(
            "SELECT -1;",
            &unary_op_expression(UnaryOp::Minus, numeric_literal_expression("1")),
        );
        run_sunny_day_test(
            "SELECT -abc;",
            &unary_op_expression(UnaryOp::Minus, identifier_expression("abc")),
        );
        run_sunny_day_test(
            "SELECT +abc;",
            &unary_op_expression(UnaryOp::Plus, identifier_expression("abc")),
        );
    }
}
