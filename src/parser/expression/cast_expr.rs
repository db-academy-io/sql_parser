use crate::parser::errors::ParsingError;
use crate::{DataTypeParser, Expression, Keyword, Parser, TokenType};

use super::ExpressionParser;

pub trait CastExpressionParser {
    fn parse_cast_expression(&mut self) -> Result<Expression, ParsingError>;
}

impl CastExpressionParser for Parser<'_> {
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
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::{BinaryOp, DataType};

    use crate::expression::test_utils::*;

    #[test]
    fn cast_test() {
        run_sunny_day_test(
            "SELECT CAST(1 AS INTEGER);",
            select_expr(cast_expr(
                numeric_expr("1"),
                DataType::PlainDataType("INTEGER".into()),
            ))
            .into(),
        );
    }

    #[test]
    fn cast_expression() {
        run_sunny_day_test(
            "SELECT CAST(1 + 2 AS INTEGER);",
            select_expr(cast_expr(
                binary_op(BinaryOp::Plus, numeric_expr("1"), numeric_expr("2")),
                DataType::PlainDataType("INTEGER".into()),
            ))
            .into(),
        );
    }

    #[test]
    fn cast_with_null() {
        run_sunny_day_test(
            "SELECT CAST(NULL AS INTEGER);",
            select_expr(cast_expr(
                null_expr(),
                DataType::PlainDataType("INTEGER".into()),
            ))
            .into(),
        );
    }

    #[test]
    fn cast_with_sized_type() {
        run_sunny_day_test(
            "SELECT CAST(1 AS VARCHAR(10));",
            select_expr(cast_expr(
                numeric_expr("1"),
                DataType::SizedDataType("VARCHAR".into(), "10".into()),
            ))
            .into(),
        );
    }

    #[test]
    fn cast_with_bounded_type() {
        run_sunny_day_test(
            "SELECT CAST(1 AS VARCHAR(1, 10));",
            select_expr(cast_expr(
                numeric_expr("1"),
                DataType::BoundedDataType("VARCHAR".into(), "1".into(), "10".into()),
            ))
            .into(),
        );
    }

    #[test]
    fn cast_with_multi_name() {
        run_sunny_day_test(
            "SELECT CAST(1 AS DOUBLE TRIPPLE PRECISION) as 1;",
            select_expr(cast_expr(
                numeric_expr("1"),
                DataType::PlainDataType("DOUBLE TRIPPLE PRECISION".into()),
            ))
            .into(),
        );
    }
}
