use crate::parser::errors::ParsingError;
use crate::{Expression, Keyword, LikeExpressionType, Parser};

use super::ExpressionParser;

pub trait LikeExpressionParser {
    fn parse_like_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError>;
}

impl LikeExpressionParser for Parser<'_> {
    fn parse_like_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError> {
        self.consume_as_keyword(Keyword::Like)?;

        let pattern = self.parse_expression()?;

        let mut escape_expression = None;
        if self.consume_as_keyword(Keyword::Escape).is_ok() {
            escape_expression = Some(Box::new(self.parse_expression()?));
        }

        let like_expression = LikeExpressionType {
            expression: Box::new(expression),
            not: is_not,
            like_expression: Box::new(pattern),
            escape_expression,
        };

        Ok(Expression::LikeExpression(like_expression))
    }
}

#[cfg(test)]
mod like_expression_tests {
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::{Expression, LikeExpressionType};

    use crate::parser::expression::test_utils::*;

    fn like_expr(expr: Expression, like_expr: Expression) -> LikeExpressionType {
        LikeExpressionType {
            expression: Box::new(expr),
            not: false,
            like_expression: Box::new(like_expr),
            escape_expression: None,
        }
    }

    #[test]
    fn like_expr_test() {
        let expr = like_expr(numeric_expr("1"), string_expr("'a%'"));
        run_sunny_day_test("SELECT 1 LIKE 'a%';", select_expr(expr.into()).into());
    }

    #[test]
    fn not_like_expr() {
        let mut expr = like_expr(numeric_expr("1"), string_expr("'a%'"));
        expr.not = true;

        run_sunny_day_test("SELECT 1 NOT LIKE 'a%';", select_expr(expr.into()).into());
    }

    #[test]
    fn like_with_escape_expr_test() {
        let mut like_expr = like_expr(numeric_expr("1"), string_expr("'a%'"));
        like_expr.escape_expression = Some(Box::new(string_expr("'b'")));

        run_sunny_day_test(
            "SELECT 1 LIKE 'a%' ESCAPE 'b';",
            select_expr(like_expr.into()).into(),
        );
    }
}
