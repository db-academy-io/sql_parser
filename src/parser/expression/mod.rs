mod between_expr;
mod case_expr;
mod cast_expr;
mod collate_expr;
mod data_type;
mod exists_expr;
mod function_expr;
mod identifier;
mod in_expr;
mod is_expr;
mod like_expr;
mod raise_expr;
mod regexp_match_expr;

use crate::parser::errors::ParsingError;
use crate::{
    BinaryOp, Expression, Keyword, LiteralValue, Parser, TokenType, UnaryMatchingExpression,
    UnaryOp,
};
use between_expr::BetweenExpressionParser;
use case_expr::CaseExpressionParser;
use cast_expr::CastExpressionParser;
use collate_expr::CollateExpressionParser;
pub use data_type::DataTypeParser;
use exists_expr::ExistsExpressionParser;
pub use function_expr::FunctionParser;
pub use identifier::IdentifierParser;
use in_expr::InExpressionParser;
use is_expr::IsExpressionParser;
use like_expr::LikeExpressionParser;
use raise_expr::RaiseExpressionParser;
use regexp_match_expr::RegexpMatchExpressionParser;

use once_cell::sync::Lazy;
use std::collections::HashMap;

/// The precedence of the operators.
///
/// The precedence of an operator is the maximum precedence of the expressions
/// that can be formed with the operator.
///
/// The source of the precedence values is the SQLite grammar, see:
/// [SQL Language Expressions](https://www.sqlite.org/lang_expr.html)
static PRECEDENCE: Lazy<HashMap<TokenType, u8>> = Lazy::new(|| {
    let pairs = [
        (TokenType::Concat, 60),
        (TokenType::Star, 50),
        (TokenType::Slash, 50),
        (TokenType::Remainder, 50),
        (TokenType::Plus, 40),
        (TokenType::Minus, 40),
        (TokenType::BitAnd, 30),
        (TokenType::BitOr, 30),
        (TokenType::LeftShift, 30),
        (TokenType::RightShift, 30),
        (TokenType::GreaterThan, 20),
        (TokenType::LessThan, 20),
        (TokenType::GreaterEquals, 20),
        (TokenType::LessEquals, 20),
        (TokenType::Equals, 10),
        (TokenType::EqualsEquals, 10),
        (TokenType::NotEquals, 10),
    ];

    pairs.iter().cloned().collect()
});

/// Trait for parsing expressions
/// The expression documentation can be found here:
/// https://www.sqlite.org/lang_expr.html
pub trait ExpressionParser {
    /// Parse an expression
    fn parse_expression(&mut self) -> Result<Expression, ParsingError>;

    /// Parse a comma separated list of expressions
    fn parse_comma_separated_expressions(&mut self) -> Result<Vec<Expression>, ParsingError>;

    /// Parse an expression using Pratt's parsing algorithm
    /// `rbp` is the right binding power of the current operator
    fn parse_expression_pratt(&mut self, rbp: u8) -> Result<Expression, ParsingError>;

    /// Parse a prefix expression
    /// A prefix expression is an expression that does not have a left operand.
    fn parse_prefix(&mut self) -> Result<Expression, ParsingError>;

    /// Parse an infix expression
    /// An infix expression is an expression that has a left operand.
    fn parse_infix(&mut self, left: Expression, precedence: u8)
        -> Result<Expression, ParsingError>;

    /// Get the precedence of the given operator
    fn get_precedence(&mut self, operator: &TokenType) -> u8;
}

impl<'a> ExpressionParser for Parser<'a> {
    /// Parse an SQLite3 [expr](https://www.sqlite.org/lang_expr.html#the_expr_list)
    fn parse_expression(&mut self) -> Result<Expression, ParsingError> {
        // Check if it's one of the special expressions
        if let Ok(keyword) = self.peek_as_keyword() {
            match keyword {
                Keyword::Case => return CaseExpressionParser::parse_case_expression(self),
                Keyword::Cast => return CastExpressionParser::parse_cast_expression(self),
                Keyword::Not => {
                    self.consume_as_keyword(Keyword::Not)?;
                    return ExistsExpressionParser::parse_exists_expression(self, true);
                }
                Keyword::Exists => {
                    return ExistsExpressionParser::parse_exists_expression(self, false)
                }
                Keyword::Raise => return RaiseExpressionParser::parse_raise_expression(self),
                Keyword::Null
                | Keyword::CurrentTime
                | Keyword::CurrentDate
                | Keyword::CurrentTimestamp => {
                    // These are literals, so we don't need to do anything here
                    // as the pratt parser will handle them
                }
                _ => return Err(ParsingError::UnexpectedKeyword(keyword)),
            }
        }

        if self.peek_as(TokenType::LeftParen).is_ok() {
            // Consume the left parenthesis
            self.consume_as(TokenType::LeftParen)?;

            // TODO: Handle EXISTS (SELECT ...)
            let expression = if let Ok(Keyword::Select) = self.peek_as_keyword() {
                ExistsExpressionParser::parse_exists_expression(self, false)?
            } else {
                Expression::ExpressionList(self.parse_comma_separated_expressions()?)
            };
            // The right parenthesis must be in the last token in the expression list
            self.consume_as(TokenType::RightParen)?;
            return Ok(expression);
        }

        let expression = self.parse_expression_pratt(0)?;

        if let Ok(keyword) = self.peek_as_keyword() {
            match keyword {
                Keyword::Collate => {
                    return CollateExpressionParser::parse_collate_expression(self, expression);
                }
                Keyword::Isnull => {
                    self.consume_as_keyword(Keyword::Isnull)?;
                    return Ok(Expression::UnaryMatchingExpression(
                        Box::new(expression),
                        UnaryMatchingExpression::IsNull,
                    ));
                }
                Keyword::Notnull => {
                    self.consume_as_keyword(Keyword::Notnull)?;
                    return Ok(Expression::UnaryMatchingExpression(
                        Box::new(expression),
                        UnaryMatchingExpression::IsNotNull,
                    ));
                }
                Keyword::Not => {
                    self.consume_as_keyword(Keyword::Not)?;

                    if let Ok(nested_keyword) = self.peek_as_keyword() {
                        return match nested_keyword {
                            Keyword::Null => {
                                self.consume_as_keyword(Keyword::Null)?;
                                Ok(Expression::UnaryMatchingExpression(
                                    Box::new(expression),
                                    UnaryMatchingExpression::IsNotNull,
                                ))
                            }
                            Keyword::Between => BetweenExpressionParser::parse_between_expression(
                                self, expression, true,
                            ),
                            Keyword::Like => {
                                LikeExpressionParser::parse_like_expression(self, expression, true)
                            }
                            Keyword::Glob | Keyword::Regexp | Keyword::Match => {
                                RegexpMatchExpressionParser::parse_regexp_match_expression(
                                    self, expression, true,
                                )
                            }
                            Keyword::In => {
                                InExpressionParser::parse_in_expression(self, expression, true)
                            }
                            _ => {
                                return Err(ParsingError::UnexpectedKeyword(nested_keyword));
                            }
                        };
                    } else {
                        return Err(ParsingError::UnexpectedKeyword(keyword));
                    }
                }
                Keyword::Between => {
                    // No need to consume the BETWEEN keyword, as the between parser will handle it
                    return BetweenExpressionParser::parse_between_expression(
                        self, expression, false,
                    );
                }
                Keyword::Like => {
                    // No need to consume the LIKE keyword, as the like parser will handle it
                    return LikeExpressionParser::parse_like_expression(self, expression, false);
                }
                Keyword::Glob | Keyword::Regexp | Keyword::Match => {
                    // No need to consume the keyword, as the regexp match parser will handle it
                    return RegexpMatchExpressionParser::parse_regexp_match_expression(
                        self, expression, false,
                    );
                }
                Keyword::Is => {
                    // No need to consume the IS keyword, as the is parser will handle it
                    return IsExpressionParser::parse_is_expression(self, expression);
                }
                Keyword::In => {
                    // No need to consume the IN keyword, as the in parser will handle it
                    return InExpressionParser::parse_in_expression(self, expression, false);
                }
                _ => {
                    // return Err(ParsingError::UnexpectedKeyword(keyword));
                }
            }
        }

        Ok(expression)
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

    /// Parse an expression using Pratt's parsing algorithm
    fn parse_expression_pratt(&mut self, precedence: u8) -> Result<Expression, ParsingError> {
        let mut expression = self.parse_prefix()?;

        loop {
            let current_token = self.peek_token()?;
            let next_precedence = self.get_precedence(&current_token.token_type);

            if precedence >= next_precedence {
                break;
            }
            expression = self.parse_infix(expression, next_precedence)?;
        }
        Ok(expression)
    }

    /// Parse an infix expression
    /// An infix expression is an expression that has a left operand.
    fn parse_infix(
        &mut self,
        left: Expression,
        precedence: u8,
    ) -> Result<Expression, ParsingError> {
        let token = self.peek_token()?;
        let operator = BinaryOp::try_from(&token.token_type)?;
        // Consume the operator token
        self.consume_as(token.token_type)?;

        let right = self.parse_expression_pratt(precedence)?;
        Ok(Expression::BinaryOp(
            Box::new(left),
            operator,
            Box::new(right),
        ))
    }

    /// Parse a prefix expression
    /// A prefix expression is an expression that does not have a left operand.
    fn parse_prefix(&mut self) -> Result<Expression, ParsingError> {
        if let Ok(keyword) = self.peek_as_keyword() {
            if keyword == Keyword::Null
                || keyword == Keyword::CurrentTime
                || keyword == Keyword::CurrentDate
                || keyword == Keyword::CurrentTimestamp
            {
                self.consume_as_keyword(keyword)?;

                match keyword {
                    Keyword::Null => return Ok(Expression::LiteralValue(LiteralValue::Null)),
                    Keyword::CurrentTime => {
                        return Ok(Expression::LiteralValue(LiteralValue::CurrentTime))
                    }
                    Keyword::CurrentDate => {
                        return Ok(Expression::LiteralValue(LiteralValue::CurrentDate))
                    }
                    Keyword::CurrentTimestamp => {
                        return Ok(Expression::LiteralValue(LiteralValue::CurrentTimestamp))
                    }
                    _ => {}
                }
            }
        }

        let token = self.peek_token()?;

        match token.token_type {
            TokenType::Id(_) => {
                let id = self.parse_identifier()?;

                if self.peek_as(TokenType::LeftParen).is_ok() {
                    self.parse_function(id)
                } else {
                    Ok(Expression::Identifier(id))
                }
            }
            TokenType::Integer(value) => {
                self.consume_as_number()?;
                Ok(Expression::LiteralValue(LiteralValue::Number(
                    value.to_string(),
                )))
            }
            TokenType::Float(value) => {
                self.consume_as_number()?;
                Ok(Expression::LiteralValue(LiteralValue::Number(
                    value.to_string(),
                )))
            }
            TokenType::String(value) => {
                self.consume_as_string()?;
                Ok(Expression::LiteralValue(LiteralValue::String(
                    value.to_string(),
                )))
            }
            TokenType::Variable(value) => {
                self.consume_token()?;
                Ok(Expression::BindParameter(value.to_string()))
            }
            TokenType::LeftParen => {
                self.consume_as(TokenType::LeftParen)?;
                let expression = self.parse_expression_pratt(0)?;
                let token = self.peek_token()?;
                if token.token_type != TokenType::RightParen {
                    return Err(ParsingError::UnexpectedToken(format!(
                        "Expected right parenthesis, got: {}",
                        token.token_type
                    )));
                }
                self.consume_as(TokenType::RightParen)?;
                Ok(expression)
            }
            TokenType::Minus => {
                self.consume_as(TokenType::Minus)?;
                let pr = self.get_precedence(&TokenType::Minus);
                let expression = self.parse_expression_pratt(pr)?;
                Ok(Expression::UnaryOp(UnaryOp::Minus, Box::new(expression)))
            }
            TokenType::Plus => {
                self.consume_as(TokenType::Plus)?;
                let pr = self.get_precedence(&TokenType::Plus);
                let expression = self.parse_expression_pratt(pr)?;
                Ok(Expression::UnaryOp(UnaryOp::Plus, Box::new(expression)))
            }
            TokenType::Blob(value) => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::Blob(
                    value.to_string(),
                )))
            }
            TokenType::True => {
                self.consume_as(TokenType::True)?;
                Ok(Expression::LiteralValue(LiteralValue::Boolean(true)))
            }
            TokenType::False => {
                self.consume_as(TokenType::False)?;
                Ok(Expression::LiteralValue(LiteralValue::Boolean(false)))
            }

            // TokenType:: => {
            //     self.consume_token()?;
            //     Ok(Expression::LiteralValue(LiteralValue::Null))
            // }
            _ => Err(ParsingError::UnexpectedToken(format!(
                "Unexpected token: {}",
                token.token_type
            ))),
        }
    }

    /// Get the precedence of the given operator
    fn get_precedence(&mut self, operator: &TokenType) -> u8 {
        *PRECEDENCE.get(operator).unwrap_or(&0)
    }
}

#[cfg(test)]
pub(crate) mod test_utils {
    use crate::ast::{Expression, SelectItem};
    use crate::{
        BinaryOp, CollateExpressionStatement, DataType, ExistsStatement, Function, FunctionArg,
        Identifier, LiteralValue, OverClause, Parser, RaiseFunction, SelectBody, SelectStatement,
        Statement, UnaryOp,
    };

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

    pub fn numeric_literal_expression(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::Number(value.to_string()))
    }

    pub fn string_literal_expression(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::String(value.to_string()))
    }

    pub fn blob_literal_expression(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::Blob(value.to_string()))
    }

    pub fn boolean_literal_expression(value: bool) -> Expression {
        Expression::LiteralValue(LiteralValue::Boolean(value))
    }

    pub fn null_literal_expression() -> Expression {
        Expression::LiteralValue(LiteralValue::Null)
    }

    pub fn bind_parameter_expression(value: &str) -> Expression {
        Expression::BindParameter(value.to_string())
    }

    pub fn identifier_expression(values: &[&str]) -> Expression {
        if values.len() == 1 {
            Expression::Identifier(Identifier::Single(values[0].to_string()))
        } else {
            Expression::Identifier(Identifier::Compound(
                values.iter().map(|s| s.to_string()).collect(),
            ))
        }
    }

    pub fn unary_op_expression(op: UnaryOp, value: Expression) -> Expression {
        Expression::UnaryOp(op, Box::new(value))
    }

    pub fn binary_op_expression(op: BinaryOp, left: Expression, right: Expression) -> Expression {
        Expression::BinaryOp(Box::new(left), op, Box::new(right))
    }

    pub fn function_expression(
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

    pub fn exist_expression(is_not: bool, statement: SelectStatement) -> Expression {
        Expression::ExistsStatement(if is_not {
            ExistsStatement::NotExists(statement)
        } else {
            ExistsStatement::Exists(statement)
        })
    }

    pub fn raise_expression(function: RaiseFunction) -> Expression {
        Expression::RaiseFunction(function)
    }

    pub fn cast_expression(expression: Expression, data_type: DataType) -> Expression {
        Expression::Cast(Box::new(expression), data_type)
    }

    pub fn collate_expression(expression: Expression, name: String) -> Expression {
        Expression::CollateExpression(CollateExpressionStatement {
            expression: Box::new(expression),
            collation_name: name,
        })
    }

    pub fn expression_list(expressions: Vec<Expression>) -> Expression {
        Expression::ExpressionList(expressions)
    }
}

#[cfg(test)]
mod literal_value_expression_tests {
    use super::test_utils::*;
    use crate::ast::{Expression, LiteralValue};

    #[test]
    fn test_expression_literal_value_valid() {
        run_sunny_day_expression_test("SELECT 1;", &numeric_literal_expression("1"));

        run_sunny_day_expression_test("SELECT 1.2;", &numeric_literal_expression("1.2"));

        run_sunny_day_expression_test(
            "SELECT 1.234567890;",
            &numeric_literal_expression("1.234567890"),
        );

        run_sunny_day_expression_test(
            "SELECT 'Hello, world!';",
            &string_literal_expression("'Hello, world!'"),
        );

        run_sunny_day_expression_test(
            "SELECT X'DEADBEEF';",
            &blob_literal_expression("X'DEADBEEF'"),
        );

        run_sunny_day_expression_test("SELECT TRUE;", &boolean_literal_expression(true));

        run_sunny_day_expression_test("SELECT FALSE;", &boolean_literal_expression(false));

        run_sunny_day_expression_test("SELECT NULL;", &null_literal_expression());

        run_sunny_day_expression_test(
            "SELECT CURRENT_TIME;",
            &Expression::LiteralValue(LiteralValue::CurrentTime),
        );

        run_sunny_day_expression_test(
            "SELECT CURRENT_DATE;",
            &Expression::LiteralValue(LiteralValue::CurrentDate),
        );

        run_sunny_day_expression_test(
            "SELECT CURRENT_TIMESTAMP;",
            &Expression::LiteralValue(LiteralValue::CurrentTimestamp),
        );
    }
}

#[cfg(test)]
mod bind_parameter_expression_tests {
    use super::test_utils::*;

    #[test]
    fn test_expression_bind_parameter_valid() {
        run_sunny_day_expression_test("SELECT ?;", &bind_parameter_expression("?"));
        run_sunny_day_expression_test("SELECT ?1;", &bind_parameter_expression("?1"));
        run_sunny_day_expression_test("SELECT :name;", &bind_parameter_expression(":name"));
        run_sunny_day_expression_test("SELECT @var;", &bind_parameter_expression("@var"));
        run_sunny_day_expression_test("SELECT $value;", &bind_parameter_expression("$value"));
        run_sunny_day_expression_test("SELECT #param;", &bind_parameter_expression("#param"));

        // TODO: Add tests for invalid bind parameters
    }
}

#[cfg(test)]
mod unary_op_expression_tests {
    use crate::UnaryOp;

    use super::test_utils::*;

    #[test]
    fn test_expression_unary_op_valid() {
        run_sunny_day_expression_test(
            "SELECT +1;",
            &unary_op_expression(UnaryOp::Plus, numeric_literal_expression("1")),
        );
        run_sunny_day_expression_test(
            "SELECT -1;",
            &unary_op_expression(UnaryOp::Minus, numeric_literal_expression("1")),
        );
        run_sunny_day_expression_test(
            "SELECT -abc;",
            &unary_op_expression(UnaryOp::Minus, identifier_expression(&["abc"])),
        );
        run_sunny_day_expression_test(
            "SELECT +abc;",
            &unary_op_expression(UnaryOp::Plus, identifier_expression(&["abc"])),
        );

        run_sunny_day_expression_test(
            "SELECT -+1;",
            &unary_op_expression(
                UnaryOp::Minus,
                unary_op_expression(UnaryOp::Plus, numeric_literal_expression("1")),
            ),
        );
        run_sunny_day_expression_test(
            "SELECT ++1;",
            &unary_op_expression(
                UnaryOp::Plus,
                unary_op_expression(UnaryOp::Plus, numeric_literal_expression("1")),
            ),
        );
    }
}

#[cfg(test)]
mod binary_op_expression_tests {
    use super::test_utils::*;
    use crate::{BinaryOp, FunctionArg, FunctionArgType, UnaryOp};

    #[test]
    fn test_expression_binary_ops() {
        use BinaryOp::*;
        let operators = [
            Plus,
            Minus,
            Mul,
            Div,
            Remainder,
            GreaterThan,
            GreaterThanOrEquals,
            LessThan,
            LessThanOrEquals,
            Equals,
            EqualsEquals,
            NotEquals,
            Concat,
            BitAnd,
            BitOr,
            LeftShift,
            RightShift,
        ];

        for op in operators {
            run_sunny_day_expression_test(
                &format!("SELECT 1 {} 2;", op),
                &binary_op_expression(
                    op,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
            );
        }
    }

    #[test]
    fn test_expression_binary_operation() {
        run_sunny_day_expression_test(
            "SELECT col1 + col2;",
            &binary_op_expression(
                BinaryOp::Plus,
                identifier_expression(&["col1"]),
                identifier_expression(&["col2"]),
            ),
        );
    }

    #[test]
    fn test_expression_binary_with_prefix() {
        run_sunny_day_expression_test(
            "SELECT 1 ++ 2;",
            &binary_op_expression(
                BinaryOp::Plus,
                numeric_literal_expression("1"),
                unary_op_expression(UnaryOp::Plus, numeric_literal_expression("2")),
            ),
        );
    }

    #[test]
    fn test_expression_binary_operation_the_same_precedence() {
        run_sunny_day_expression_test(
            "SELECT 1 + 2 + 3;",
            &binary_op_expression(
                BinaryOp::Plus,
                binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                numeric_literal_expression("3"),
            ),
        );
    }

    #[test]
    fn test_expression_binary_operation_the_same_precedence2() {
        run_sunny_day_expression_test(
            "SELECT 1 * 2 / 3;",
            &binary_op_expression(
                BinaryOp::Div,
                binary_op_expression(
                    BinaryOp::Mul,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                numeric_literal_expression("3"),
            ),
        );
    }

    #[test]
    fn test_expression_binary_operation_precedence() {
        run_sunny_day_expression_test(
            "SELECT 1 + 2 * 3;",
            &binary_op_expression(
                BinaryOp::Plus,
                numeric_literal_expression("1"),
                binary_op_expression(
                    BinaryOp::Mul,
                    numeric_literal_expression("2"),
                    numeric_literal_expression("3"),
                ),
            ),
        );
    }

    #[test]
    fn test_expression_binary_operation_precedence2() {
        run_sunny_day_expression_test(
            "SELECT 1 + 2 * 3 - 4",
            &binary_op_expression(
                BinaryOp::Minus,
                binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("1"),
                    binary_op_expression(
                        BinaryOp::Mul,
                        numeric_literal_expression("2"),
                        numeric_literal_expression("3"),
                    ),
                ),
                numeric_literal_expression("4"),
            ),
        );
    }

    #[test]
    fn test_expression_binary_operation_precedence3() {
        run_sunny_day_expression_test(
            "SELECT col1 + col2 * 3;",
            &binary_op_expression(
                BinaryOp::Plus,
                identifier_expression(&["col1"]),
                binary_op_expression(
                    BinaryOp::Mul,
                    identifier_expression(&["col2"]),
                    numeric_literal_expression("3"),
                ),
            ),
        );
    }

    #[test]
    fn test_expression_binary_operation_precedence_with_function() {
        run_sunny_day_expression_test(
            "SELECT col1 + col2 * max(1, 3, 4);",
            &binary_op_expression(
                BinaryOp::Plus,
                identifier_expression(&["col1"]),
                binary_op_expression(
                    BinaryOp::Mul,
                    identifier_expression(&["col2"]),
                    function_expression(
                        "max",
                        FunctionArg {
                            distinct: false,
                            arguments: vec![
                                FunctionArgType::Expression(numeric_literal_expression("1")),
                                FunctionArgType::Expression(numeric_literal_expression("3")),
                                FunctionArgType::Expression(numeric_literal_expression("4")),
                            ],
                        },
                        None,
                        None,
                    ),
                ),
            ),
        );
    }
}

#[cfg(test)]
mod unary_matching_expression_tests {
    use crate::{Expression, UnaryMatchingExpression};

    use super::test_utils::*;

    fn unary_matching_expression(
        expression: Expression,
        unary_matching_expression: UnaryMatchingExpression,
    ) -> Expression {
        Expression::UnaryMatchingExpression(Box::new(expression), unary_matching_expression)
    }

    #[test]
    fn test_expression_matching_isnull() {
        run_sunny_day_expression_test(
            "SELECT 1 ISNULL;",
            &unary_matching_expression(
                numeric_literal_expression("1"),
                UnaryMatchingExpression::IsNull,
            ),
        );
    }

    #[test]
    fn test_expression_matching_notnull() {
        run_sunny_day_expression_test(
            "SELECT 1 NOT NULL;",
            &unary_matching_expression(
                numeric_literal_expression("1"),
                UnaryMatchingExpression::IsNotNull,
            ),
        );
        run_sunny_day_expression_test(
            "SELECT 1 NOTNULL;",
            &unary_matching_expression(
                numeric_literal_expression("1"),
                UnaryMatchingExpression::IsNotNull,
            ),
        );
    }
}

#[cfg(test)]
mod parenthesized_expression_tests {
    use crate::{BinaryOp, DataType, Expression, RaiseFunction, UnaryOp};

    use super::test_utils::*;

    #[test]
    fn test_parenthesized_expression() {
        run_sunny_day_expression_test(
            "SELECT (1 + 2 * 3)",
            &Expression::ExpressionList(vec![binary_op_expression(
                BinaryOp::Plus,
                numeric_literal_expression("1"),
                binary_op_expression(
                    BinaryOp::Mul,
                    numeric_literal_expression("2"),
                    numeric_literal_expression("3"),
                ),
            )]),
        );
    }

    #[test]
    fn test_parenthesized_two_expressions() {
        run_sunny_day_expression_test(
            "SELECT (1 + 2, 3 / 4);",
            &Expression::ExpressionList(vec![
                binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                binary_op_expression(
                    BinaryOp::Div,
                    numeric_literal_expression("3"),
                    numeric_literal_expression("4"),
                ),
            ]),
        );
    }

    #[test]
    fn test_parenthesized_multiple_expressions() {
        run_sunny_day_expression_test(
            "SELECT (1 + 2, 3 / 4, cast(5 as int), -5, raise (ignore));",
            &Expression::ExpressionList(vec![
                binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                binary_op_expression(
                    BinaryOp::Div,
                    numeric_literal_expression("3"),
                    numeric_literal_expression("4"),
                ),
                cast_expression(
                    numeric_literal_expression("5"),
                    DataType::PlainDataType("int".into()),
                ),
                unary_op_expression(UnaryOp::Minus, numeric_literal_expression("5")),
                raise_expression(RaiseFunction::Ignore),
            ]),
        );
    }
}
