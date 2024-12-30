mod between_expr;
mod case_expr;
mod cast_expr;
mod collate_expr;
mod exists_expr;
mod function_expr;
mod in_expr;
mod is_expr;
mod like_expr;
mod pratt_parser;
mod precedence;
mod raise_expr;
mod regexp_match_expr;
use crate::parser::errors::ParsingError;
use crate::{Expression, Keyword, Parser, TokenType, UnaryMatchingExpression};
use between_expr::BetweenExpressionParser;
use collate_expr::CollateExpressionParser;
use function_expr::FunctionParser;
use in_expr::InExpressionParser;
use is_expr::IsExpressionParser;
use like_expr::LikeExpressionParser;
use pratt_parser::PrattParser;
use regexp_match_expr::RegexpMatchExpressionParser;

/// Trait for parsing expressions
/// The expression documentation can be found here:
/// https://www.sqlite.org/lang_expr.html
pub trait ExpressionParser {
    /// Parse an expression
    fn parse_expression(&mut self) -> Result<Expression, ParsingError>;

    /// Parse a comma separated list of expressions
    fn parse_comma_separated_expressions(&mut self) -> Result<Vec<Expression>, ParsingError>;
}

impl<'a> ExpressionParser for Parser<'a> {
    /// Parse an SQLite3 [expr](https://www.sqlite.org/lang_expr.html)
    fn parse_expression(&mut self) -> Result<Expression, ParsingError> {
        let expression = self.parse_expression_pratt(0)?;

        if let Ok(Keyword::Collate) = self.peek_as_keyword() {
            CollateExpressionParser::parse_collate_expression(self, expression)
        } else if let Ok(Keyword::Isnull) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Isnull)?;
            Ok(Expression::UnaryMatchingExpression(
                Box::new(expression),
                UnaryMatchingExpression::IsNull,
            ))
        } else if let Ok(Keyword::Notnull) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Notnull)?;
            Ok(Expression::UnaryMatchingExpression(
                Box::new(expression),
                UnaryMatchingExpression::IsNotNull,
            ))
        } else if let Ok(Keyword::Not) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Not)?;

            if let Ok(nested_keyword) = self.peek_as_keyword() {
                match nested_keyword {
                    Keyword::Null => {
                        self.consume_as_keyword(Keyword::Null)?;
                        Ok(Expression::UnaryMatchingExpression(
                            Box::new(expression),
                            UnaryMatchingExpression::IsNotNull,
                        ))
                    }
                    Keyword::Between => {
                        BetweenExpressionParser::parse_between_expression(self, expression, true)
                    }
                    Keyword::Like => {
                        LikeExpressionParser::parse_like_expression(self, expression, true)
                    }
                    Keyword::Glob | Keyword::Regexp | Keyword::Match => {
                        RegexpMatchExpressionParser::parse_regexp_match_expression(
                            self, expression, true,
                        )
                    }
                    Keyword::In => InExpressionParser::parse_in_expression(self, expression, true),
                    _ => {
                        return Err(ParsingError::UnexpectedKeyword(nested_keyword));
                    }
                }
            } else {
                Err(ParsingError::UnexpectedKeyword(Keyword::Not))
            }
        } else if let Ok(Keyword::Between) = self.peek_as_keyword() {
            return BetweenExpressionParser::parse_between_expression(self, expression, false);
        } else if let Ok(Keyword::Like) = self.peek_as_keyword() {
            return LikeExpressionParser::parse_like_expression(self, expression, false);
        } else if let Ok(Keyword::Is) = self.peek_as_keyword() {
            return IsExpressionParser::parse_is_expression(self, expression);
        } else if let Ok(Keyword::In) = self.peek_as_keyword() {
            return InExpressionParser::parse_in_expression(self, expression, false);
        } else if let Ok(Keyword::Glob) = self.peek_as_keyword() {
            return RegexpMatchExpressionParser::parse_regexp_match_expression(
                self, expression, false,
            );
        } else if let Ok(Keyword::Regexp) = self.peek_as_keyword() {
            return RegexpMatchExpressionParser::parse_regexp_match_expression(
                self, expression, false,
            );
        } else if let Ok(Keyword::Match) = self.peek_as_keyword() {
            return RegexpMatchExpressionParser::parse_regexp_match_expression(
                self, expression, false,
            );
        } else {
            Ok(expression)
        }
    }

    /// Parse a comma separated list of expressions
    fn parse_comma_separated_expressions(&mut self) -> Result<Vec<Expression>, ParsingError> {
        let mut expressions = Vec::new();

        loop {
            expressions.push(self.parse_expression()?);

            if self.peek_as(TokenType::Comma).is_ok() {
                self.consume_as(TokenType::Comma)?;
            } else {
                break;
            }
        }
        Ok(expressions)
    }
}

#[cfg(test)]
pub(crate) mod test_utils {
    use crate::ast::{Expression, SelectItem};
    use crate::{
        BinaryOp, CollateExpressionStatement, DataType, DistinctType, ExistsStatement, Function,
        FunctionArg, Identifier, LiteralValue, OverClause, Parser, RaiseFunction, Select,
        SelectBody, SelectStatement, Statement, UnaryOp,
    };

    pub fn select_expr(expression: Expression) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(expression)],
                from: None,
                where_clause: None,
                group_by: None,
                having: None,
                window: None,
            }),
            order_by: None,
            limit: None,
        }
    }

    pub fn run_sunny_day_test_with_multiple_expressions(
        sql: &str,
        expected_expressions: &[&Expression],
    ) {
        let mut parser = Parser::from(sql);
        let actual_statement = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        dbg!("actual_expression: {:?}", &actual_statement);

        match actual_statement {
            Statement::Select(SelectStatement {
                with_cte: None,
                select: SelectBody::Select(select_statement),
                order_by: None,
                limit: None,
            }) => {
                assert_eq!(
                    expected_expressions.len(),
                    select_statement.columns.len(),
                    "Expected {} columns, got {:?}",
                    expected_expressions.len(),
                    select_statement.columns.len()
                );

                for (i, select_item) in select_statement.columns.iter().enumerate() {
                    match select_item {
                        SelectItem::Expression(actual_expression) => {
                            assert_eq!(
                                expected_expressions[i], actual_expression,
                                "Expected expression {:?}, got {:?}",
                                expected_expressions[i], actual_expression
                            );
                        }
                        _ => panic!("Expected Expression, got {:?}", select_item),
                    }
                }
            }
            _ => panic!("Expected Select statement, got {:?}", actual_statement),
        }
    }

    pub fn run_sunny_day_expression_test(sql: &str, expected_expression: &Expression) {
        run_sunny_day_test_with_multiple_expressions(sql, &[expected_expression]);
    }

    pub fn numeric_expr(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::Number(value.to_string()))
    }

    pub fn string_expr(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::String(value.to_string()))
    }

    pub fn blob_expr(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::Blob(value.to_string()))
    }

    pub fn boolean_expr(value: bool) -> Expression {
        Expression::LiteralValue(LiteralValue::Boolean(value))
    }

    pub fn null_expr() -> Expression {
        Expression::LiteralValue(LiteralValue::Null)
    }

    pub fn bind_expr(value: &str) -> Expression {
        Expression::BindParameter(value.to_string())
    }

    pub fn identifier_expr(values: &[&str]) -> Expression {
        if values.len() == 1 {
            Expression::Identifier(Identifier::Single(values[0].to_string()))
        } else {
            Expression::Identifier(Identifier::Compound(
                values.iter().map(|s| s.to_string()).collect(),
            ))
        }
    }

    pub fn unary_op(op: UnaryOp, value: Expression) -> Expression {
        Expression::UnaryOp(op, Box::new(value))
    }

    pub fn binary_op(op: BinaryOp, left: Expression, right: Expression) -> Expression {
        Expression::BinaryOp(Box::new(left), op, Box::new(right))
    }

    pub fn function(
        name: &str,
        arg: FunctionArg,
        filter: Option<Box<Expression>>,
        over: Option<OverClause>,
    ) -> Expression {
        let function = Function {
            name: Identifier::Single(name.to_string()),
            arg,
            filter_clause: filter,
            over_clause: over,
        };

        Expression::Function(function)
    }

    pub fn exist_expr(is_not: bool, statement: SelectStatement) -> Expression {
        Expression::ExistsStatement(if is_not {
            ExistsStatement::NotExists(statement)
        } else {
            ExistsStatement::Exists(statement)
        })
    }

    pub fn raise_expr(function: RaiseFunction) -> Expression {
        Expression::RaiseFunction(function)
    }

    pub fn cast_expr(expression: Expression, data_type: DataType) -> Expression {
        Expression::Cast(Box::new(expression), data_type)
    }

    pub fn collate_expr(expression: Expression, name: String) -> Expression {
        Expression::CollateExpression(CollateExpressionStatement {
            expression: Box::new(expression),
            collation_name: name,
        })
    }

    pub fn expr_list(expressions: Vec<Expression>) -> Expression {
        Expression::ExpressionList(expressions)
    }
}
