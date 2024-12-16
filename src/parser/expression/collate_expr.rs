use crate::{Expression, Keyword, Parser, ParsingError};

pub trait CollateExpressionParser {
    /// Parse a COLLATE expression, i.e. an expression followed by the COLLATE keyword and a string literal
    /// Example: $expr COLLATE "utf8"
    fn parse_collate_expression(
        &mut self,
        expression: Expression,
    ) -> Result<Expression, ParsingError>;
}

impl<'a> CollateExpressionParser for Parser<'a> {
    fn parse_collate_expression(
        &mut self,
        expression: Expression,
    ) -> Result<Expression, ParsingError> {
        self.consume_as_keyword(Keyword::Collate)?;
        let name = self.peek_as_string()?;
        self.consume_token()?;

        Ok(Expression::CollateExpression(
            Box::new(expression),
            name.to_string(),
        ))
    }
}

#[cfg(test)]
mod collate_expression_tests {
    use crate::parser::expression::test_utils::*;
    use crate::BinaryOp;

    #[test]
    fn test_expression_collate_basic() {
        run_sunny_day_expression_test(
            "SELECT 1 COLLATE 'utf8';",
            &collate_expression(numeric_literal_expression("1"), "'utf8'".to_string()),
        );
    }

    #[test]
    fn test_expression_collate_with_expression() {
        run_sunny_day_expression_test(
            "SELECT 1 + 2 COLLATE 'utf8';",
            &collate_expression(
                binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                "'utf8'".to_string(),
            ),
        );
    }
}
