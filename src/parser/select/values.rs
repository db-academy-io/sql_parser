use crate::parser::errors::ParsingError;
use crate::{
    expression::ExpressionParser, Expression, Keyword, Parser, TokenType, ValuesStatement,
};

pub trait ValuesStatementParser {
    fn parse_values_statement(&mut self) -> Result<ValuesStatement, ParsingError>;

    fn parse_values_statement_group(&mut self) -> Result<Vec<Expression>, ParsingError>;
}

impl<'a> ValuesStatementParser for Parser<'a> {
    fn parse_values_statement(&mut self) -> Result<ValuesStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Values)?;
        let mut values = Vec::new();

        // Check if the first token is a left parenthesis
        // which is mandatory for the VALUES statement
        while self.peek_as(TokenType::LeftParen).is_ok() {
            let group = self.parse_values_statement_group()?;

            values.push(group);

            // check if the current token is a comma, then consume it
            // otherwise break the loop
            if self.consume_as(TokenType::Comma).is_err() {
                break;
            }
        }

        Ok(ValuesStatement { values })
    }

    fn parse_values_statement_group(&mut self) -> Result<Vec<Expression>, ParsingError> {
        self.consume_as(TokenType::LeftParen)?;

        let mut group = Vec::new();

        while let Ok(expression) = self.parse_expression() {
            group.push(expression);

            if self.consume_as(TokenType::Comma).is_err() {
                break;
            }
        }

        self.consume_as(TokenType::RightParen)?;

        Ok(group)
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{Expression, SelectBody, SelectStatement, ValuesStatement};

    pub fn values_statement(values: Vec<Vec<Expression>>) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Values(ValuesStatement { values }),
            order_by: None,
            limit: None,
        }
    }
}

#[cfg(test)]
mod values_statement_tests {
    use super::test_utils::values_statement;
    use crate::parser::expression::test_utils::*;
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::{BinaryOp, Statement};

    #[test]
    pub fn single_value() {
        let expected = values_statement(vec![vec![numeric_literal_expression("1")]]);

        run_sunny_day_test("VALUES (1)", Statement::Select(expected));
    }

    #[test]
    pub fn multiple_values() {
        let expected = values_statement(vec![vec![
            numeric_literal_expression("1"),
            numeric_literal_expression("2"),
            numeric_literal_expression("3"),
        ]]);

        run_sunny_day_test("VALUES (1, 2, 3)", Statement::Select(expected));
    }

    #[test]
    pub fn single_expressions() {
        let expected = values_statement(vec![vec![binary_op_expression(
            BinaryOp::Plus,
            numeric_literal_expression("1"),
            numeric_literal_expression("2"),
        )]]);

        run_sunny_day_test("VALUES (1 + 2)", Statement::Select(expected));
    }

    #[test]
    pub fn multiple_expressions() {
        let expected = values_statement(vec![vec![
            binary_op_expression(
                BinaryOp::Plus,
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
            ),
            binary_op_expression(
                BinaryOp::Mul,
                numeric_literal_expression("3"),
                numeric_literal_expression("4"),
            ),
        ]]);

        run_sunny_day_test("VALUES (1 + 2, 3 * 4)", Statement::Select(expected));
    }

    #[test]
    pub fn multiple_groups() {
        let expected = values_statement(vec![
            vec![numeric_literal_expression("1")],
            vec![numeric_literal_expression("2")],
            vec![numeric_literal_expression("3")],
        ]);

        run_sunny_day_test("VALUES (1), (2), (3)", Statement::Select(expected));
    }

    #[test]
    pub fn multiple_expressions_groups() {
        let expected = values_statement(vec![
            vec![binary_op_expression(
                BinaryOp::Plus,
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
            )],
            vec![binary_op_expression(
                BinaryOp::Mul,
                numeric_literal_expression("3"),
                numeric_literal_expression("4"),
            )],
        ]);

        run_sunny_day_test("VALUES (1 + 2), (3 * 4)", Statement::Select(expected));
    }
}
