use crate::{
    parser::select::SelectStatementParser, BinaryMatchingExpression, Expression, Identifier,
    InExpression, Keyword, Parser, ParsingError, TokenType,
};

use super::ExpressionParser;

pub trait InExpressionParser {
    /// Parse an $expr IN $expr expression
    fn parse_in_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError>;
}

impl<'a> InExpressionParser for Parser<'a> {
    fn parse_in_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError> {
        self.consume_as_keyword(Keyword::In)?;

        let in_expression = if self.peek_as(TokenType::LeftParen).is_ok() {
            // Consume the left parenthesis
            self.consume_as(TokenType::LeftParen)?;

            let result = if self.peek_as(TokenType::RightParen).is_ok() {
                BinaryMatchingExpression::In(InExpression::Empty)
            } else if let Ok(Keyword::Select) = self.peek_as_keyword() {
                // The IN expression is a subquery
                let select_statement = self.parse_select_statement()?;
                BinaryMatchingExpression::In(InExpression::Select(select_statement))
            } else {
                let expressions = self.parse_comma_separated_expressions()?;
                BinaryMatchingExpression::In(InExpression::Expression(expressions))
            };

            // Consume the enclosing right parenthesis
            self.consume_as(TokenType::RightParen)?;
            result
        } else {
            // Parse expressions like $expr IN $schema.table or schema.function(*args)
            let id1 = self.peek_as_id()?;
            self.consume_as_id()?;

            let identifier = if self.peek_as(TokenType::Dot).is_ok() {
                // Consume the dot token
                self.consume_as(TokenType::Dot)?;

                let id2 = self.peek_as_id()?;
                self.consume_as_id()?;
                Identifier::Compound(vec![id1.to_string(), id2.to_string()])
            } else {
                Identifier::Single(id1.to_string())
            };

            if self.peek_as(TokenType::LeftParen).is_ok() {
                self.consume_as(TokenType::LeftParen)?;

                if self.peek_as(TokenType::RightParen).is_ok() {
                    self.consume_as(TokenType::RightParen)?;
                    BinaryMatchingExpression::In(InExpression::TableFunction(identifier, vec![]))
                } else {
                    let args = self.parse_comma_separated_expressions()?;

                    self.consume_as(TokenType::RightParen)?;

                    BinaryMatchingExpression::In(InExpression::TableFunction(identifier, args))
                }
            } else {
                BinaryMatchingExpression::In(InExpression::Identity(identifier))
            }
        };

        if is_not {
            Ok(Expression::BinaryMatchingExpression(
                Box::new(expression),
                BinaryMatchingExpression::Not(Box::new(in_expression)),
            ))
        } else {
            Ok(Expression::BinaryMatchingExpression(
                Box::new(expression),
                in_expression,
            ))
        }
    }
}

#[cfg(test)]
mod expression_with_in_statement_tests {
    use crate::{
        BinaryMatchingExpression, BinaryOp, Expression, Identifier, InExpression, SelectItem,
        SelectStatement,
    };

    use crate::parser::expression::test_utils::*;

    fn expression_with_in_statement(
        expression: Expression,
        in_expression: InExpression,
        is_not: bool,
    ) -> Expression {
        let in_expression = if is_not {
            BinaryMatchingExpression::Not(Box::new(BinaryMatchingExpression::In(in_expression)))
        } else {
            BinaryMatchingExpression::In(in_expression)
        };

        Expression::BinaryMatchingExpression(Box::new(expression), in_expression)
    }

    #[test]
    fn test_expression_with_empty_select_statement() {
        run_sunny_day_test(
            "SELECT 1 IN ();",
            &expression_with_in_statement(
                numeric_literal_expression("1"),
                InExpression::Empty,
                false,
            ),
        );

        run_sunny_day_test(
            "SELECT 1 NOT IN ();",
            &expression_with_in_statement(
                numeric_literal_expression("1"),
                InExpression::Empty,
                true,
            ),
        );
    }

    #[test]
    fn test_expression_with_select_statement() {
        let mut select_statement = SelectStatement::default();
        select_statement.columns = vec![SelectItem::Expression(numeric_literal_expression("2"))];

        run_sunny_day_test(
            "SELECT 1 IN (SELECT 2);",
            &expression_with_in_statement(
                numeric_literal_expression("1"),
                InExpression::Select(select_statement),
                false,
            ),
        );
    }

    #[test]
    fn test_expression_with_multiple_expressions() {
        run_sunny_day_test(
            "SELECT 1 IN (2, 3, 4 + 5);",
            &expression_with_in_statement(
                numeric_literal_expression("1"),
                InExpression::Expression(vec![
                    numeric_literal_expression("2"),
                    numeric_literal_expression("3"),
                    binary_op_expression(
                        BinaryOp::Plus,
                        numeric_literal_expression("4"),
                        numeric_literal_expression("5"),
                    ),
                ]),
                false,
            ),
        );
    }

    #[test]
    fn test_expression_with_not_in_expression() {
        let mut select_statement = SelectStatement::default();
        select_statement.columns = vec![SelectItem::Expression(numeric_literal_expression("2"))];

        run_sunny_day_test(
            "SELECT 1 NOT IN (SELECT 2);",
            &expression_with_in_statement(
                numeric_literal_expression("1"),
                InExpression::Select(select_statement),
                true,
            ),
        );
    }

    #[test]
    fn test_expression_with_table_name() {
        run_sunny_day_test(
            "SELECT 1 NOT IN table_name;",
            &expression_with_in_statement(
                numeric_literal_expression("1"),
                InExpression::Identity(Identifier::Single("table_name".to_string())),
                true,
            ),
        );
    }

    #[test]
    fn test_expression_with_schema_and_table_names() {
        run_sunny_day_test(
            "SELECT 1 NOT IN schema_name.table_name;",
            &expression_with_in_statement(
                numeric_literal_expression("1"),
                InExpression::Identity(Identifier::Compound(vec![
                    "schema_name".to_string(),
                    "table_name".to_string(),
                ])),
                true,
            ),
        );
    }

    #[test]
    fn test_expression_with_schema_and_table_function_without_arguments() {
        run_sunny_day_test(
            "SELECT 1 NOT IN schema_name.table_function();",
            &expression_with_in_statement(
                numeric_literal_expression("1"),
                InExpression::TableFunction(
                    Identifier::Compound(vec![
                        "schema_name".to_string(),
                        "table_function".to_string(),
                    ]),
                    vec![],
                ),
                true,
            ),
        );
    }

    #[test]
    fn test_expression_with_schema_and_table_function_with_arguments() {
        run_sunny_day_test(
            "SELECT 1 NOT IN schema_name.table_function(1, 2, 3);",
            &expression_with_in_statement(
                numeric_literal_expression("1"),
                InExpression::TableFunction(
                    Identifier::Compound(vec![
                        "schema_name".to_string(),
                        "table_function".to_string(),
                    ]),
                    vec![
                        numeric_literal_expression("1"),
                        numeric_literal_expression("2"),
                        numeric_literal_expression("3"),
                    ],
                ),
                true,
            ),
        );
    }

    #[test]
    fn test_expression_with_table_function_with_arguments() {
        run_sunny_day_test(
            "SELECT 1 NOT IN table_function(1+2, 3*4);",
            &expression_with_in_statement(
                numeric_literal_expression("1"),
                InExpression::TableFunction(
                    Identifier::Single("table_function".to_string()),
                    vec![
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
                    ],
                ),
                true,
            ),
        );
    }
}
