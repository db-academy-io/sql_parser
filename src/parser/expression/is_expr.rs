use crate::{AnIsExpression, BinaryMatchingExpression, Expression, Keyword, Parser, ParsingError};

use super::ExpressionParser;

pub trait IsExpressionParser {
    /// Parse an `$expr IS $expr` expression
    fn parse_is_expression(&mut self, expression: Expression) -> Result<Expression, ParsingError>;
}

impl<'a> IsExpressionParser for Parser<'a> {
    fn parse_is_expression(&mut self, expression: Expression) -> Result<Expression, ParsingError> {
        self.consume_as_keyword(Keyword::Is)?;

        let is_not = self.consume_as_keyword(Keyword::Not).is_ok();
        let mut distinct = false;

        if let Ok(Keyword::Distinct) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Distinct)?;
            distinct = true;

            // The FROM keyword is mandatory after the DISTINCT keyword
            self.consume_as_keyword(Keyword::From)?;
        }

        let is_expression = BinaryMatchingExpression::Is(AnIsExpression {
            expression: Box::new(self.parse_expression()?),
            distinct,
        });

        let result = if is_not {
            BinaryMatchingExpression::Not(Box::new(is_expression))
        } else {
            is_expression
        };

        Ok(Expression::BinaryMatchingExpression(
            Box::new(expression),
            result,
        ))
    }
}

#[cfg(test)]
mod is_expression_tests {
    use crate::{AnIsExpression, BinaryMatchingExpression, Expression};

    use crate::parser::expression::test_utils::*;

    fn expression_from_expression(
        expression: Expression,
        is_expression: Expression,
        distinct: bool,
        is_not: bool,
    ) -> Expression {
        let binary_matching_expression = if is_not {
            BinaryMatchingExpression::Not(Box::new(BinaryMatchingExpression::Is(AnIsExpression {
                expression: Box::new(is_expression),
                distinct,
            })))
        } else {
            BinaryMatchingExpression::Is(AnIsExpression {
                expression: Box::new(is_expression),
                distinct,
            })
        };

        Expression::BinaryMatchingExpression(Box::new(expression), binary_matching_expression)
    }

    #[test]
    fn test_expression_is_another_expression() {
        run_sunny_day_test(
            "SELECT 1 IS 1;",
            &expression_from_expression(
                numeric_literal_expression("1"),
                numeric_literal_expression("1"),
                false,
                false,
            ),
        );
    }

    #[test]
    fn test_expression_is_not_another_expression() {
        run_sunny_day_test(
            "SELECT 1 IS NOT 1;",
            &expression_from_expression(
                numeric_literal_expression("1"),
                numeric_literal_expression("1"),
                false,
                true,
            ),
        );
    }

    #[test]
    fn test_expression_is_distinct_another_expression() {
        run_sunny_day_test(
            "SELECT 1 IS DISTINCT FROM 1;",
            &expression_from_expression(
                numeric_literal_expression("1"),
                numeric_literal_expression("1"),
                true,
                false,
            ),
        );
    }
}
