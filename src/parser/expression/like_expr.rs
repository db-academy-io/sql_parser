use crate::parser::errors::ParsingError;
use crate::{
    BinaryMatchingExpression, EscapeExpression, Expression, Keyword, LikeExpressionType, Parser,
};

use super::ExpressionParser;

pub trait LikeExpressionParser {
    fn parse_like_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError>;
}

impl<'a> LikeExpressionParser for Parser<'a> {
    fn parse_like_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError> {
        self.consume_as_keyword(Keyword::Like)?;

        let pattern = self.parse_expression()?;

        let mut escape_expression = None;
        if let Ok(Keyword::Escape) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Escape)?;
            escape_expression = Some(Box::new(self.parse_expression()?));
        }

        let matching_expression: BinaryMatchingExpression = {
            match escape_expression {
                Some(escape_expression) => BinaryMatchingExpression::Like(
                    LikeExpressionType::EscapeExpression(EscapeExpression {
                        expression: Box::new(pattern),
                        escape_expression: Some(escape_expression),
                    }),
                ),
                None => BinaryMatchingExpression::Like(LikeExpressionType::Expression(Box::new(
                    pattern,
                ))),
            }
        };

        if is_not {
            Ok(Expression::BinaryMatchingExpression(
                Box::new(expression),
                BinaryMatchingExpression::Not(Box::new(matching_expression)),
            ))
        } else {
            Ok(Expression::BinaryMatchingExpression(
                Box::new(expression),
                matching_expression,
            ))
        }
    }
}

#[cfg(test)]
mod like_expression_tests {
    use crate::{BinaryMatchingExpression, EscapeExpression, Expression, LikeExpressionType};

    use crate::parser::expression::test_utils::*;

    fn like_expression(
        expression: Expression,
        like_expression_type: LikeExpressionType,
        inverted: bool,
    ) -> Expression {
        let binary_matching_expression = if inverted {
            BinaryMatchingExpression::Not(Box::new(BinaryMatchingExpression::Like(
                like_expression_type,
            )))
        } else {
            BinaryMatchingExpression::Like(like_expression_type)
        };

        Expression::BinaryMatchingExpression(Box::new(expression), binary_matching_expression)
    }

    #[test]
    fn test_expression_like_basic() {
        run_sunny_day_expression_test(
            "SELECT 1 LIKE 'a%';",
            &like_expression(
                numeric_expr("1"),
                LikeExpressionType::Expression(Box::new(string_expr("'a%'"))),
                false,
            ),
        );
    }

    #[test]
    fn test_expression_not_like_basic() {
        run_sunny_day_expression_test(
            "SELECT 1 NOT LIKE 'a%';",
            &like_expression(
                numeric_expr("1"),
                LikeExpressionType::Expression(Box::new(string_expr("'a%'"))),
                true,
            ),
        );
    }

    #[test]
    fn test_expression_like_with_escape_basic() {
        run_sunny_day_expression_test(
            "SELECT 1 LIKE 'a%' ESCAPE 'b';",
            &like_expression(
                numeric_expr("1"),
                LikeExpressionType::EscapeExpression(EscapeExpression {
                    expression: Box::new(string_expr("'a%'")),
                    escape_expression: Some(Box::new(string_expr("'b'"))),
                }),
                false,
            ),
        );
    }
}
