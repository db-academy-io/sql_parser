use crate::parser::errors::ParsingError;
use crate::{CollateExpressionStatement, Expression, Keyword, Parser};

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

        Ok(Expression::CollateExpression(CollateExpressionStatement {
            expression: Box::new(expression),
            collation_name: name.to_string(),
        }))
    }
}

#[cfg(test)]
mod collate_expression_tests {
    use crate::parser::expression::test_utils::*;
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::BinaryOp;

    #[test]
    fn collate_test() {
        run_sunny_day_test(
            "SELECT 1 COLLATE 'utf8';",
            select_expr(collate_expr(numeric_expr("1"), "'utf8'".to_string())).into(),
        );
    }

    #[test]
    fn collate_with_expression() {
        run_sunny_day_test(
            "SELECT 1 + 2 COLLATE 'utf8';",
            select_expr(collate_expr(
                binary_op(BinaryOp::Plus, numeric_expr("1"), numeric_expr("2")),
                "'utf8'".to_string(),
            ))
            .into(),
        );
    }
}
