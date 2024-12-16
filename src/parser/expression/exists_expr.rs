use crate::parser::select::SelectStatementParser;
use crate::{ExistsStatement, Expression, Keyword, Parser, ParsingError, TokenType};

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

        if is_not {
            Ok(Expression::ExistsStatement(ExistsStatement::NotExists(
                select_statement,
            )))
        } else {
            Ok(Expression::ExistsStatement(ExistsStatement::Exists(
                select_statement,
            )))
        }
    }
}

#[cfg(test)]
mod exists_expression_tests {
    use crate::{Select, SelectItem, SelectStatement};

    use crate::parser::expression::test_utils::*;

    fn select_statement(columns: Vec<SelectItem>) -> SelectStatement {
        SelectStatement::Select(Select {
            columns,
            ..Default::default()
        })
    }

    #[test]
    fn test_expression_exists() {
        run_sunny_day_expression_test(
            "SELECT EXISTS (SELECT 1);",
            &exist_expression(
                false,
                select_statement(vec![SelectItem::Expression(numeric_literal_expression(
                    "1",
                ))]),
            ),
        );
    }

    #[test]
    fn test_expression_not_exists() {
        run_sunny_day_expression_test(
            "SELECT NOT EXISTS (SELECT 1);",
            &exist_expression(
                true,
                select_statement(vec![SelectItem::Expression(numeric_literal_expression(
                    "1",
                ))]),
            ),
        );
    }

    #[test]
    fn test_expression_not_exists_without_exists_keyword() {
        run_sunny_day_expression_test(
            "SELECT NOT (SELECT 21);",
            &exist_expression(
                true,
                select_statement(vec![SelectItem::Expression(numeric_literal_expression(
                    "21",
                ))]),
            ),
        );
    }
}
