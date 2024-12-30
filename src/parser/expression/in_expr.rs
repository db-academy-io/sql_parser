use crate::parser::errors::ParsingError;
use crate::{
    parser::select::SelectStatementParser, BinaryMatchingExpression, Expression, Identifier,
    InExpression, Keyword, Parser, TokenType,
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

            let identifier = if self.consume_as(TokenType::Dot).is_ok() {
                let id2 = self.peek_as_id()?;
                self.consume_as_id()?;
                Identifier::Compound(vec![id1.to_string(), id2.to_string()])
            } else {
                Identifier::Single(id1.to_string())
            };

            if self.consume_as(TokenType::LeftParen).is_ok() {
                if self.consume_as(TokenType::RightParen).is_ok() {
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

        let in_expression = if is_not {
            BinaryMatchingExpression::Not(Box::new(in_expression))
        } else {
            in_expression
        };

        Ok(Expression::BinaryMatchingExpression(
            Box::new(expression),
            in_expression,
        ))
    }
}

#[cfg(test)]
mod expression_with_in_statement_tests {
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::{
        BinaryMatchingExpression, BinaryOp, Expression, Identifier, InExpression, Select,
        SelectBody, SelectItem, SelectStatement,
    };

    use crate::parser::expression::test_utils::*;

    fn in_expr(expression: Expression, in_expression: InExpression, is_not: bool) -> Expression {
        let in_expression = if is_not {
            BinaryMatchingExpression::Not(Box::new(BinaryMatchingExpression::In(in_expression)))
        } else {
            BinaryMatchingExpression::In(in_expression)
        };

        Expression::BinaryMatchingExpression(Box::new(expression), in_expression)
    }

    #[test]
    fn in_empty() {
        run_sunny_day_test(
            "SELECT 1 IN ();",
            select_expr(in_expr(numeric_expr("1"), InExpression::Empty, false)).into(),
        );

        run_sunny_day_test(
            "SELECT 1 NOT IN ();",
            select_expr(in_expr(numeric_expr("1"), InExpression::Empty, true)).into(),
        );
    }

    #[test]
    fn in_select() {
        let mut select_statement = Select::default();
        select_statement.columns = vec![SelectItem::Expression(numeric_expr("2"))];

        run_sunny_day_test(
            "SELECT 1 IN (SELECT 2);",
            select_expr(in_expr(
                numeric_expr("1"),
                InExpression::Select(SelectStatement {
                    with_cte: None,
                    select: SelectBody::Select(select_statement),
                    order_by: None,
                    limit: None,
                }),
                false,
            ))
            .into(),
        );
    }

    #[test]
    fn in_multiple_expressions() {
        run_sunny_day_test(
            "SELECT 1 IN (2, 3, 4 + 5);",
            select_expr(in_expr(
                numeric_expr("1"),
                InExpression::Expression(vec![
                    numeric_expr("2"),
                    numeric_expr("3"),
                    binary_op(BinaryOp::Plus, numeric_expr("4"), numeric_expr("5")),
                ]),
                false,
            ))
            .into(),
        );
    }

    #[test]
    fn not_in_select() {
        let mut select_statement = Select::default();
        select_statement.columns = vec![SelectItem::Expression(numeric_expr("2"))];

        run_sunny_day_test(
            "SELECT 1 NOT IN (SELECT 2);",
            select_expr(in_expr(
                numeric_expr("1"),
                InExpression::Select(SelectStatement {
                    with_cte: None,
                    select: SelectBody::Select(select_statement),
                    order_by: None,
                    limit: None,
                }),
                true,
            ))
            .into(),
        );
    }

    #[test]
    fn not_in_with_table_name() {
        run_sunny_day_test(
            "SELECT 1 NOT IN table_name;",
            select_expr(in_expr(
                numeric_expr("1"),
                InExpression::Identity(Identifier::Single("table_name".to_string())),
                true,
            ))
            .into(),
        );
    }

    #[test]
    fn not_in_with_schema_and_table_names() {
        run_sunny_day_test(
            "SELECT 1 NOT IN schema_name.table_name;",
            select_expr(in_expr(
                numeric_expr("1"),
                InExpression::Identity(Identifier::Compound(vec![
                    "schema_name".to_string(),
                    "table_name".to_string(),
                ])),
                true,
            ))
            .into(),
        );
    }

    #[test]
    fn not_in_with_schema_and_function() {
        run_sunny_day_test(
            "SELECT 1 NOT IN schema_name.table_function();",
            select_expr(in_expr(
                numeric_expr("1"),
                InExpression::TableFunction(
                    Identifier::Compound(vec![
                        "schema_name".to_string(),
                        "table_function".to_string(),
                    ]),
                    vec![],
                ),
                true,
            ))
            .into(),
        );
    }

    #[test]
    fn not_in_with_schema_and_function_with_arguments() {
        run_sunny_day_test(
            "SELECT 1 NOT IN schema_name.table_function(1, 2, 3);",
            select_expr(in_expr(
                numeric_expr("1"),
                InExpression::TableFunction(
                    Identifier::Compound(vec![
                        "schema_name".to_string(),
                        "table_function".to_string(),
                    ]),
                    vec![numeric_expr("1"), numeric_expr("2"), numeric_expr("3")],
                ),
                true,
            ))
            .into(),
        );
    }

    #[test]
    fn not_in_function_with_expr_arguments() {
        run_sunny_day_test(
            "SELECT 1 NOT IN table_function(1+2, 3*4);",
            select_expr(in_expr(
                numeric_expr("1"),
                InExpression::TableFunction(
                    Identifier::Single("table_function".to_string()),
                    vec![
                        binary_op(BinaryOp::Plus, numeric_expr("1"), numeric_expr("2")),
                        binary_op(BinaryOp::Mul, numeric_expr("3"), numeric_expr("4")),
                    ],
                ),
                true,
            ))
            .into(),
        );
    }
}
