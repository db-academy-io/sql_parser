use crate::parser::errors::ParsingError;
use crate::{Expression, Keyword, Parser, TokenType};

use super::{data_type::DataTypeParser, ExpressionParser};

pub trait CastExpressionParser {
    fn parse_cast_expression(&mut self) -> Result<Expression, ParsingError>;
}

impl<'a> CastExpressionParser for Parser<'a> {
    fn parse_cast_expression(&mut self) -> Result<Expression, ParsingError> {
        self.consume_as_keyword(Keyword::Cast)?;

        self.consume_as(TokenType::LeftParen)?;

        let expression = self.parse_expression()?;
        self.consume_as_keyword(Keyword::As)?;

        let data_type = self.parse_data_type()?;

        self.consume_as(TokenType::RightParen)?;

        Ok(Expression::Cast(Box::new(expression), data_type))
    }
}

#[cfg(test)]
mod cast_expression_tests {
    use crate::{BinaryOp, DataType};

    use crate::expression::test_utils::*;

    #[test]
    fn test_expression_cast_basic() {
        run_sunny_day_expression_test(
            "SELECT CAST(1 AS INTEGER);",
            &cast_expression(
                numeric_literal_expression("1"),
                DataType::PlainDataType("INTEGER".to_string()),
            ),
        );
    }

    #[test]
    fn test_expression_cast_expression() {
        run_sunny_day_expression_test(
            "SELECT CAST(1 + 2 AS INTEGER);",
            &cast_expression(
                binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                DataType::PlainDataType("INTEGER".to_string()),
            ),
        );
    }

    #[test]
    fn test_expression_cast_with_null() {
        run_sunny_day_expression_test(
            "SELECT CAST(NULL AS INTEGER);",
            &cast_expression(
                null_literal_expression(),
                DataType::PlainDataType("INTEGER".to_string()),
            ),
        );
    }

    #[test]
    fn test_expression_cast_with_complex_type() {
        run_sunny_day_expression_test(
            "SELECT CAST(1 AS VARCHAR(10));",
            &cast_expression(
                numeric_literal_expression("1"),
                DataType::SizedDataType("VARCHAR".to_string(), "10".to_string()),
            ),
        );
    }

    #[test]
    fn test_expression_cast_with_complex_type2() {
        run_sunny_day_expression_test(
            "SELECT CAST(1 AS VARCHAR(1, 10));",
            &cast_expression(
                numeric_literal_expression("1"),
                DataType::BoundedDataType("VARCHAR".to_string(), "1".to_string(), "10".to_string()),
            ),
        );
    }
}
