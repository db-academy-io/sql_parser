use crate::{Keyword, TokenType};

use super::expression::ExpressionParser;
use super::{Parser, ParsingError};
use crate::ast::{SelectItem, SelectStatement};

/// Trait for parsing SELECT statements
/// The SELECT statement documentation can be found here:
/// https://www.sqlite.org/lang_select.html
pub trait SelectStatementParser {
    fn parse_select_statement(&mut self) -> Result<SelectStatement, ParsingError>;

    fn parse_select_columns(&mut self) -> Result<Vec<SelectItem>, ParsingError>;

    #[allow(dead_code)]
    fn parser_select_from_part(&mut self) -> Result<(), ParsingError>;
}

impl<'a> SelectStatementParser for Parser<'a> {
    fn parse_select_statement(&mut self) -> Result<SelectStatement, ParsingError> {
        // Consume the SELECT keyword
        self.consume_as_keyword(Keyword::Select)?;

        let select_statement = SelectStatement {
            distinct: self.consume_as_keyword(Keyword::Distinct).is_ok(),
            all: self.consume_as_keyword(Keyword::All).is_ok(),
            columns: self.parse_select_columns()?,
            ..Default::default()
        };

        // TODO:
        // self.parser_select_from_part()?;

        Ok(select_statement)
    }

    fn parse_select_columns(&mut self) -> Result<Vec<SelectItem>, ParsingError> {
        let mut select_items = Vec::new();

        loop {
            let expression = self.parse_expression()?;
            select_items.push(SelectItem::Expression(expression));

            if self.peek_as(TokenType::Comma).is_ok() {
                self.consume_as(TokenType::Comma)?;
            } else {
                break;
            }
        }
        Ok(select_items)
    }

    fn parser_select_from_part(&mut self) -> Result<(), ParsingError> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{Expression, Identifier, LiteralValue, SelectItem, SelectStatement, Statement};

    use super::super::test_utils::*;

    #[test]
    fn test_select_statement_parser_with_single_literal_value() {
        run_sunny_day_test(
            "SELECT 1",
            Statement::Select(SelectStatement {
                columns: vec![SelectItem::Expression(Expression::LiteralValue(
                    LiteralValue::Number("1".to_string()),
                ))],
                ..Default::default()
            }),
        );
    }

    #[test]
    fn test_select_statement_parser_with_single_identifier() {
        run_sunny_day_test(
            "SELECT id",
            Statement::Select(SelectStatement {
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Single("id".to_string()),
                ))],
                ..Default::default()
            }),
        );
    }
}
