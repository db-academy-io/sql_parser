use crate::parser::errors::ParsingError;
use crate::{Expression, IsExpression, Keyword, Parser};

use super::ExpressionParser;

pub trait IsExpressionParser {
    /// Parse an `$expr IS $expr` expression
    fn parse_is_expression(&mut self, expression: Expression) -> Result<Expression, ParsingError>;
}

impl IsExpressionParser for Parser<'_> {
    fn parse_is_expression(&mut self, expression: Expression) -> Result<Expression, ParsingError> {
        self.consume_as_keyword(Keyword::Is)?;

        let is_not = self.consume_as_keyword(Keyword::Not).is_ok();
        let mut distinct = false;

        if self.consume_as_keyword(Keyword::Distinct).is_ok() {
            distinct = true;

            // The FROM keyword is mandatory after the DISTINCT keyword
            self.consume_as_keyword(Keyword::From)?;
        }

        let is_expression = IsExpression {
            expression: Box::new(expression),
            not: is_not,
            distinct,
            matching_expression: Box::new(self.parse_expression()?),
        };

        Ok(Expression::IsExpression(is_expression))
    }
}

#[cfg(test)]
mod is_expression_tests {
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::{Expression, IsExpression};

    use crate::parser::expression::test_utils::*;

    fn is_expression(
        expression: Expression,
        matching_expression: Expression,
        distinct: bool,
        is_not: bool,
    ) -> Expression {
        Expression::IsExpression(IsExpression {
            expression: Box::new(expression),
            distinct,
            not: is_not,
            matching_expression: Box::new(matching_expression),
        })
    }

    #[test]
    fn is_expr_test() {
        run_sunny_day_test(
            "SELECT 1 IS 1;",
            select_expr(is_expression(
                numeric_expr("1"),
                numeric_expr("1"),
                false,
                false,
            ))
            .into(),
        );
    }

    #[test]
    fn is_not_expr_test() {
        run_sunny_day_test(
            "SELECT 1 IS NOT 1;",
            select_expr(is_expression(
                numeric_expr("1"),
                numeric_expr("1"),
                false,
                true,
            ))
            .into(),
        );
    }

    #[test]
    fn is_distinct_expr_test() {
        run_sunny_day_test(
            "SELECT 1 IS DISTINCT FROM 1;",
            select_expr(is_expression(
                numeric_expr("1"),
                numeric_expr("1"),
                true,
                false,
            ))
            .into(),
        );
    }
}
