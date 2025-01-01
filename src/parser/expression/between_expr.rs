use crate::parser::errors::ParsingError;
use crate::{BetweenExpression, BinaryMatchingExpression, Expression, Keyword, Parser, TokenType};

use super::precedence::get_precedence;
use super::ExpressionParser;

pub trait BetweenExpressionParser {
    fn parse_between_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError>;
}

impl BetweenExpressionParser for Parser<'_> {
    fn parse_between_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError> {
        // Consume the BETWEEN keyword
        self.consume_as_keyword(Keyword::Between)?;

        let precedence = get_precedence(&TokenType::Keyword(Keyword::Between));
        let lower_bound = self.parse_sub_expression(precedence)?;

        self.consume_as_keyword(Keyword::And)?;

        let upper_bound = self.parse_sub_expression(precedence)?;

        let matching_expression = if is_not {
            BinaryMatchingExpression::Not(Box::new(BinaryMatchingExpression::Between(
                BetweenExpression {
                    lower_bound: Box::new(lower_bound),
                    upper_bound: Box::new(upper_bound),
                },
            )))
        } else {
            BinaryMatchingExpression::Between(BetweenExpression {
                lower_bound: Box::new(lower_bound),
                upper_bound: Box::new(upper_bound),
            })
        };

        Ok(Expression::BinaryMatchingExpression(
            Box::new(expression),
            matching_expression,
        ))
    }
}

#[cfg(test)]
mod between_expression_tests {
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::{BetweenExpression, BinaryMatchingExpression, BinaryOp, Expression};

    use crate::parser::expression::test_utils::*;

    fn between_expr(
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
    fn between_expression() {
        run_sunny_day_test(
            "SELECT 1 BETWEEN 2 AND 3;",
            select_expr(between_expr(
                numeric_expr("1"),
                numeric_expr("2"),
                numeric_expr("3"),
                false,
            ))
            .into(),
        );
    }

    #[test]
    fn between_with_expression() {
        run_sunny_day_test(
            "SELECT 1 + 2 BETWEEN 3 AND 4;",
            select_expr(between_expr(
                binary_op(BinaryOp::Plus, numeric_expr("1"), numeric_expr("2")),
                numeric_expr("3"),
                numeric_expr("4"),
                false,
            ))
            .into(),
        );
    }

    #[test]
    fn not_between_with_expression() {
        run_sunny_day_test(
            "SELECT 1 + 2 NOT BETWEEN 3 AND 4;",
            select_expr(between_expr(
                binary_op(BinaryOp::Plus, numeric_expr("1"), numeric_expr("2")),
                numeric_expr("3"),
                numeric_expr("4"),
                true,
            ))
            .into(),
        );
    }
}
