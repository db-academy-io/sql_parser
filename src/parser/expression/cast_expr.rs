use crate::parser::errors::ParsingError;
use crate::{DataTypeParser, Expression, Keyword, Parser, TokenType};

use super::ExpressionParser;

pub trait CastExpressionParser {
    fn parse_cast_expression(&mut self) -> Result<Expression, ParsingError>;
}

impl<'a> CastExpressionParser for Parser<'a> {
    fn parse_cast_expression(&mut self) -> Result<Expression, ParsingError> {
        dbg!(&self.peek_token()?);
        self.consume_as_keyword(Keyword::Cast)?;

        self.consume_as(TokenType::LeftParen)?;

        let expression = self.parse_expression()?;
        self.consume_as_keyword(Keyword::As)?;

        let data_type = self.parse_data_type()?;

        dbg!(&self.peek_token()?);
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
                DataType::PlainDataType("INTEGER".into()),
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
                DataType::PlainDataType("INTEGER".into()),
            ),
        );
    }

    #[test]
    fn test_expression_cast_with_null() {
        run_sunny_day_expression_test(
            "SELECT CAST(NULL AS INTEGER);",
            &cast_expression(
                null_literal_expression(),
                DataType::PlainDataType("INTEGER".into()),
            ),
        );
    }

    #[test]
    fn test_expression_cast_with_complex_type() {
        run_sunny_day_expression_test(
            "SELECT CAST(1 AS VARCHAR(10));",
            &cast_expression(
                numeric_literal_expression("1"),
                DataType::SizedDataType("VARCHAR".into(), "10".into()),
            ),
        );
    }

    #[test]
    fn test_expression_cast_with_complex_type2() {
        run_sunny_day_expression_test(
            "SELECT CAST(1 AS VARCHAR(1, 10));",
            &cast_expression(
                numeric_literal_expression("1"),
                DataType::BoundedDataType("VARCHAR".into(), "1".into(), "10".into()),
            ),
        );
    }

    #[test]
    fn cast_with_multi_name() {
        run_sunny_day_expression_test(
            "SELECT CAST(1 AS DOUBLE TRIPPLE PRECISION) as 1;",
            &cast_expression(
                numeric_literal_expression("1"),
                DataType::PlainDataType("DOUBLE TRIPPLE PRECISION".into()),
            ),
        );
    }
}
