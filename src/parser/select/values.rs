use crate::{
    expression::ExpressionParser, Expression, Keyword, Parser, ParsingError, TokenType,
    ValuesStatement,
};

pub trait ValuesStatementParser {
    fn parse_values_statement(&mut self) -> Result<ValuesStatement, ParsingError>;

    fn parse_values_group(&mut self) -> Result<Vec<Expression>, ParsingError>;
}

impl<'a> ValuesStatementParser for Parser<'a> {
    fn parse_values_statement(&mut self) -> Result<ValuesStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Values)?;
        let mut values = Vec::new();

        // Check if the first token is a left parenthesis
        // which is mandatory for the VALUES statement
        self.peek_as(TokenType::LeftParen)?;

        while self.peek_as(TokenType::LeftParen).is_ok() {
            let group = self.parse_values_group()?;

            values.push(group);

            // check if the current token is a comma, then consume it
            // otherwise break the loop
            if self.consume_as(TokenType::Comma).is_err() {
                break;
            }
        }

        Ok(ValuesStatement { values })
    }

    fn parse_values_group(&mut self) -> Result<Vec<Expression>, ParsingError> {
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
pub(crate) mod select_values_tests {
    use crate::{BinaryOp, Expression, SelectStatement, Statement, ValuesStatement};

    use crate::parser::expression::test_utils::*;
    use crate::parser::test_utils::run_sunny_day_test;

    fn values_statement(values: Vec<Vec<Expression>>) -> Statement {
        Statement::Select(SelectStatement::Values(ValuesStatement { values }))
    }

    #[test]
    pub fn test_values_statement_parser_single_value() {
        run_sunny_day_test(
            "VALUES (1)",
            values_statement(vec![vec![numeric_literal_expression("1")]]),
        );
    }

    #[test]
    pub fn test_values_statement_parser_multiple_values() {
        run_sunny_day_test(
            "VALUES (1, 2, 3)",
            values_statement(vec![vec![
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
                numeric_literal_expression("3"),
            ]]),
        );
    }

    #[test]
    pub fn test_values_statement_parser_multiple_expressions() {
        run_sunny_day_test(
            "VALUES (1 + 2, 3 * 4)",
            values_statement(vec![vec![
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
            ]]),
        );
    }

    #[test]
    pub fn test_values_statement_parser_multiple_groups() {
        run_sunny_day_test(
            "VALUES (1), (2), (3)",
            values_statement(vec![
                vec![numeric_literal_expression("1")],
                vec![numeric_literal_expression("2")],
                vec![numeric_literal_expression("3")],
            ]),
        );
    }

    #[test]
    pub fn test_values_statement_parser_multiple_groups_with_expressions() {
        run_sunny_day_test(
            "VALUES (1 + 2), (3 * 4)",
            values_statement(vec![
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
            ]),
        );
    }
}
