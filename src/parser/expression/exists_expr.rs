use crate::parser::errors::ParsingError;
use crate::parser::select::SelectStatementParser;
use crate::{ExistsStatement, Expression, Keyword, Parser, TokenType};

pub trait ExistsExpressionParser {
    fn parse_exists_expression(&mut self, is_not: bool) -> Result<Expression, ParsingError>;
}

impl<'a> ExistsExpressionParser for Parser<'a> {
    fn parse_exists_expression(&mut self, is_not: bool) -> Result<Expression, ParsingError> {
        // Consume the EXISTS keyword, if it's present. The result is ignored, because
        // the EXISTS keyword is an optional keyword
        let _ = self.consume_as_keyword(Keyword::Exists);

        // Check if it's a left parenthesis, which indicates the start of a subquery
        self.consume_as(TokenType::LeftParen)?;

        let select_statement = self.parse_select_statement()?;

        // Consume the enclosing right parenthesis
        self.consume_as(TokenType::RightParen)?;

        let exists_statement = if is_not {
            ExistsStatement::NotExists(select_statement)
        } else {
            ExistsStatement::Exists(select_statement)
        };

        Ok(Expression::ExistsStatement(exists_statement))
    }
}

#[cfg(test)]
mod exists_expression_tests {
    use crate::parser::select::test_utils::select_columns;
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::SelectItem;

    use crate::parser::expression::test_utils::*;

    #[test]
    fn exists_test() {
        run_sunny_day_test(
            "SELECT EXISTS (SELECT 1);",
            select_expr(exist_expr(
                false,
                select_columns(vec![SelectItem::Expression(numeric_expr("1"))]),
            ))
            .into(),
        );
    }

    #[test]
    fn not_exists_test() {
        run_sunny_day_test(
            "SELECT NOT EXISTS (SELECT 1);",
            select_expr(exist_expr(
                true,
                select_columns(vec![SelectItem::Expression(numeric_expr("1"))]),
            ))
            .into(),
        );
    }
}
