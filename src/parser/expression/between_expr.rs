use crate::parser::errors::ParsingError;
use crate::{BetweenExpression, BinaryMatchingExpression, Expression, Keyword, Parser};

use super::ExpressionParser;

pub trait BetweenExpressionParser {
    fn parse_between_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError>;
}

impl<'a> BetweenExpressionParser for Parser<'a> {
    fn parse_between_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError> {
        // Consume the BETWEEN keyword
        self.consume_as_keyword(Keyword::Between)?;

        let lower_bound = self.parse_expression()?;

        self.consume_as_keyword(Keyword::And)?;

        let upper_bound = self.parse_expression()?;

        if is_not {
            Ok(Expression::BinaryMatchingExpression(
                Box::new(expression),
                BinaryMatchingExpression::Not(Box::new(BinaryMatchingExpression::Between(
                    BetweenExpression {
                        lower_bound: Box::new(lower_bound),
                        upper_bound: Box::new(upper_bound),
                    },
                ))),
            ))
        } else {
            Ok(Expression::BinaryMatchingExpression(
                Box::new(expression),
                BinaryMatchingExpression::Between(BetweenExpression {
                    lower_bound: Box::new(lower_bound),
                    upper_bound: Box::new(upper_bound),
                }),
            ))
        }
    }
}

#[cfg(test)]
mod between_expression_tests {
    use crate::{BetweenExpression, BinaryMatchingExpression, BinaryOp, Expression};

    use crate::parser::expression::test_utils::*;

    fn between_expression(
        expression: Expression,
        lower_bound: Expression,
        upper_bound: Expression,
        inverted: bool,
    ) -> Expression {
        let between_expression = BinaryMatchingExpression::Between(BetweenExpression {
            lower_bound: Box::new(lower_bound),
            upper_bound: Box::new(upper_bound),
        });

        let binary_matching_expression = if inverted {
            BinaryMatchingExpression::Not(Box::new(between_expression))
        } else {
            between_expression
        };

        Expression::BinaryMatchingExpression(Box::new(expression), binary_matching_expression)
    }

    #[test]
    fn test_expression_between_basic() {
        run_sunny_day_expression_test(
            "SELECT 1 BETWEEN 2 AND 3;",
            &between_expression(
                numeric_expr("1"),
                numeric_expr("2"),
                numeric_expr("3"),
                false,
            ),
        );
    }

    #[test]
    fn test_expression_between_with_expression() {
        run_sunny_day_expression_test(
            "SELECT 1 + 2 BETWEEN 3 AND 4;",
            &between_expression(
                binary_op(BinaryOp::Plus, numeric_expr("1"), numeric_expr("2")),
                numeric_expr("3"),
                numeric_expr("4"),
                false,
            ),
        );
    }

    #[test]
    fn test_expression_not_between_with_expression() {
        run_sunny_day_expression_test(
            "SELECT 1 + 2 NOT BETWEEN 3 AND 4;",
            &between_expression(
                binary_op(BinaryOp::Plus, numeric_expr("1"), numeric_expr("2")),
                numeric_expr("3"),
                numeric_expr("4"),
                true,
            ),
        );
    }
}
