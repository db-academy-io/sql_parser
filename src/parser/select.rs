use crate::Statement;

use super::expression::ExpressionParser;
use super::{Parser, ParsingError};
use crate::ast::{SelectItem, SelectStatement};

/// Trait for parsing SELECT statements
/// The SELECT statement documentation can be found here:
/// https://www.sqlite.org/lang_select.html
pub trait SelectStatementParser {
    fn parse_select_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> SelectStatementParser for Parser<'a> {
    fn parse_select_statement(&mut self) -> Result<Statement, ParsingError> {
        // Consume the SELECT keyword
        self.consume_token()?;

        let mut select_statement = SelectStatement::default();

        let maybe_expression = self.parse_expression();
        dbg!(
            "parse_select_statement: maybe_expression: {:?}",
            &maybe_expression
        );

        if let Ok(expression) = maybe_expression {
            select_statement
                .columns
                .push(SelectItem::Expression(expression));
        }

        if let Ok(()) = self.finalize_statement_parsing() {
            return Ok(Statement::Select(select_statement));
        }

        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{Expression, LiteralValue, SelectItem, SelectStatement, Statement};

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

    // #[test]
    // fn test_select_statement_parser_with_single_identifier() {
    //     run_sunny_day_test(
    //         "SELECT id",
    //         Statement::Select(SelectStatement {
    //             columns: vec![SelectItem::Identifier("id".to_string())],
    //             ..Default::default()
    //         }),
    //     );
    // }
}
