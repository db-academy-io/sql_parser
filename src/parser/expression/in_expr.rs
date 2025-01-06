use crate::parser::errors::ParsingError;
use crate::IdentifierParser;
use crate::{
    parser::select::SelectStatementParser, Expression, InExpression, InExpressionType, Keyword,
    Parser, TokenType,
};

use super::ExpressionParser;

pub trait InExpressionParser {
    /// Parse an $expr IN $expr expression
    fn parse_in_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError>;

    fn parse_in_expression_within_parenthesis(&mut self) -> Result<InExpressionType, ParsingError>;

    fn parse_in_expression_without_parenthesis(&mut self)
        -> Result<InExpressionType, ParsingError>;
}

impl InExpressionParser for Parser<'_> {
    fn parse_in_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError> {
        self.consume_as_keyword(Keyword::In)?;

        let in_expression = if self.peek_as(TokenType::LeftParen).is_ok() {
            self.parse_in_expression_within_parenthesis()?
        } else {
            self.parse_in_expression_without_parenthesis()?
        };

        Ok(Expression::InExpression(InExpression {
            expression: Box::new(expression),
            in_expression,
            is_not,
        }))
    }

    fn parse_in_expression_within_parenthesis(&mut self) -> Result<InExpressionType, ParsingError> {
        // Consume the left parenthesis
        self.consume_as(TokenType::LeftParen)?;

        let result = if self.peek_as(TokenType::RightParen).is_ok() {
            InExpressionType::Empty
        } else if let Ok(Keyword::Select) = self.peek_as_keyword() {
            // The IN expression is a subquery
            let select_statement = self.parse_select_statement()?;
            InExpressionType::Select(select_statement)
        } else {
            let expressions = self.parse_comma_separated_expressions()?;
            InExpressionType::Expression(expressions)
        };

        // Consume the enclosing right parenthesis
        self.consume_as(TokenType::RightParen)?;
        Ok(result)
    }

    fn parse_in_expression_without_parenthesis(
        &mut self,
    ) -> Result<InExpressionType, ParsingError> {
        // Parse expressions like $expr IN $schema.table or $schema.function(*args)
        let identifier = self.parse_identifier()?;

        if self.consume_as(TokenType::LeftParen).is_ok() {
            if self.consume_as(TokenType::RightParen).is_ok() {
                Ok(InExpressionType::TableFunction(identifier, vec![]))
            } else {
                let args = self.parse_comma_separated_expressions()?;

                self.consume_as(TokenType::RightParen)?;

                Ok(InExpressionType::TableFunction(identifier, args))
            }
        } else {
            Ok(InExpressionType::Identity(identifier))
        }
    }
}

#[cfg(test)]
mod expression_with_in_statement_tests {
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::{
        BinaryOp, Expression, Identifier, InExpression, InExpressionType, Select, SelectBody,
        SelectItem, SelectStatement,
    };

    use crate::parser::expression::test_utils::*;

    fn in_expr(
        expression: Expression,
        in_expression: InExpressionType,
        is_not: bool,
    ) -> Expression {
        Expression::InExpression(InExpression {
            expression: Box::new(expression),
            in_expression,
            is_not,
        })
    }

    #[test]
    fn in_empty() {
        run_sunny_day_test(
            "SELECT 1 IN ();",
            select_expr(in_expr(numeric_expr("1"), InExpressionType::Empty, false)).into(),
        );

        run_sunny_day_test(
            "SELECT 1 NOT IN ();",
            select_expr(in_expr(numeric_expr("1"), InExpressionType::Empty, true)).into(),
        );
    }

    #[test]
    fn in_select() {
        let mut select = Select::default();
        select.columns = vec![SelectItem::Expression(numeric_expr("2"))];
        let select_statement = SelectStatement {
            with_cte: None,
            select: SelectBody::Select(select),
            order_by: None,
            limit: None,
        };

        run_sunny_day_test(
            "SELECT 1 IN (SELECT 2);",
            select_expr(in_expr(
                numeric_expr("1"),
                InExpressionType::Select(select_statement),
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
                InExpressionType::Expression(vec![
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
        let mut select = Select::default();
        select.columns = vec![SelectItem::Expression(numeric_expr("2"))];

        let select_statement = SelectStatement {
            with_cte: None,
            select: SelectBody::Select(select),
            order_by: None,
            limit: None,
        };

        run_sunny_day_test(
            "SELECT 1 NOT IN (SELECT 2);",
            select_expr(in_expr(
                numeric_expr("1"),
                InExpressionType::Select(select_statement),
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
                InExpressionType::Identity(Identifier::Single("table_name".to_string())),
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
                InExpressionType::Identity(Identifier::Compound(vec![
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
                InExpressionType::TableFunction(
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
                InExpressionType::TableFunction(
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
                InExpressionType::TableFunction(
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
