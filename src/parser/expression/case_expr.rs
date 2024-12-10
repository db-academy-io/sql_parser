use crate::{CaseExpression, Expression, Keyword, Parser, ParsingError, WhenExpression};

use super::ExpressionParser;

pub trait CaseExpressionParser {
    fn parse_case_expression(&mut self) -> Result<Expression, ParsingError>;
}

impl<'a> CaseExpressionParser for Parser<'a> {
    fn parse_case_expression(&mut self) -> Result<Expression, ParsingError> {
        self.consume_as_keyword(Keyword::Case)?;

        // the main expression is optional, so we have to try to parse it first
        let expression = self
            .parse_expression()
            .map(|expression| Some(Box::new(expression)))
            .unwrap_or(None);

        let mut when_expressions = Vec::new();

        while let Ok(Keyword::When) = self.peek_as_keyword() {
            // the when expression must start with the WHEN keyword
            self.consume_as_keyword(Keyword::When)?;

            let expression = self.parse_expression()?;

            // The THEN keyword is required after the WHEN expression
            self.consume_as_keyword(Keyword::Then)?;

            let then_expression = self.parse_expression()?;

            when_expressions.push(WhenExpression {
                condition: Box::new(expression),
                then_expression: Box::new(then_expression),
            });
        }

        let mut else_expression = None;
        if let Ok(Keyword::Else) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Else)?;
            else_expression = Some(Box::new(self.parse_expression()?));
        }

        self.consume_as_keyword(Keyword::End)?;
        Ok(Expression::CaseExpression(CaseExpression {
            expression,
            when_expressions,
            else_expression,
        }))
    }
}

#[cfg(test)]
mod case_expression_tests {
    use crate::{BinaryOp, CaseExpression, Expression, WhenExpression};

    use crate::expression::test_utils::*;

    fn case_expression(
        expression: Option<Box<Expression>>,
        when_expressions: Vec<WhenExpression>,
        else_expression: Option<Box<Expression>>,
    ) -> Expression {
        Expression::CaseExpression(CaseExpression {
            expression,
            when_expressions,
            else_expression,
        })
    }

    #[test]
    fn test_expression_case_basic() {
        let expression = None;
        let when_expressions = vec![WhenExpression {
            condition: Box::new(numeric_literal_expression("1")),
            then_expression: Box::new(numeric_literal_expression("2")),
        }];
        let else_expression = Some(Box::new(numeric_literal_expression("3")));

        run_sunny_day_expression_test(
            "SELECT CASE WHEN 1 THEN 2 ELSE 3 END;",
            &case_expression(expression, when_expressions, else_expression),
        );
    }

    #[test]
    fn test_expression_case_with_multiple_when_expressions() {
        let expression = None;
        let when_expressions = vec![
            WhenExpression {
                condition: Box::new(numeric_literal_expression("1")),
                then_expression: Box::new(numeric_literal_expression("2")),
            },
            WhenExpression {
                condition: Box::new(numeric_literal_expression("3")),
                then_expression: Box::new(numeric_literal_expression("4")),
            },
        ];
        let else_expression = Some(Box::new(numeric_literal_expression("5")));

        run_sunny_day_expression_test(
            "SELECT CASE WHEN 1 THEN 2 WHEN 3 THEN 4 ELSE 5 END;",
            &case_expression(expression, when_expressions, else_expression),
        );
    }

    #[test]
    fn test_expression_case_with_main_expression() {
        let expression = Some(Box::new(binary_op_expression(
            BinaryOp::EqualsEquals,
            numeric_literal_expression("1"),
            numeric_literal_expression("1"),
        )));
        let when_expressions = vec![WhenExpression {
            condition: Box::new(boolean_literal_expression(true)),
            then_expression: Box::new(numeric_literal_expression("1")),
        }];
        let else_expression = Some(Box::new(numeric_literal_expression("2")));

        run_sunny_day_expression_test(
            "SELECT CASE 1 == 1 WHEN TRUE THEN 1 ELSE 2 END;",
            &case_expression(expression, when_expressions, else_expression),
        );
    }
}
