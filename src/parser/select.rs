use crate::{Keyword, Statement, TokenType};

use super::expression::ExpressionParser;
use super::{Parser, ParsingError};
use crate::ast::{SelectItem, SelectStatement};

/// Trait for parsing SELECT statements
/// The SELECT statement documentation can be found here:
/// https://www.sqlite.org/lang_select.html
pub trait SelectStatementParser {
    fn parse_select_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_select_columns(&mut self) -> Result<Vec<SelectItem>, ParsingError>;

    fn parse_select_distinct(&mut self) -> Result<bool, ParsingError>;

    fn parse_select_all(&mut self) -> Result<bool, ParsingError>;

    fn parser_select_from_part(&mut self) -> Result<(), ParsingError>;
}

impl<'a> SelectStatementParser for Parser<'a> {
    fn parse_select_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the SELECT keyword
        self.consume_token()?;

        let mut select_statement = SelectStatement::default();

        select_statement.distinct = self.parse_select_distinct()?;
        select_statement.all = self.parse_select_all()?;
        select_statement.columns = self.parse_select_columns()?;

        // TODO:
        // self.parser_select_from_part()?;

        Ok(Statement::Select(select_statement))
    }

    fn parse_select_columns(&mut self) -> Result<Vec<SelectItem>, ParsingError> {
        let mut select_items = Vec::new();

        loop {
            // Parse select columns
            if let Ok(expression) = self.parse_expression() {
                select_items.push(SelectItem::Expression(expression));

                if let Ok(_) = self.peek_as(TokenType::Comma) {
                    self.consume_token()?;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        Ok(select_items)
    }

    fn parse_select_distinct(&mut self) -> Result<bool, ParsingError> {
        if let Ok(Keyword::Distinct) = self.peek_as_keyword() {
            self.consume_token()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn parse_select_all(&mut self) -> Result<bool, ParsingError> {
        if let Ok(Keyword::All) = self.peek_as_keyword() {
            self.consume_token()?;
            Ok(true)
        } else {
            Ok(false)
        }
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
