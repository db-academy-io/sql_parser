mod between_expr;
mod case_expr;
mod cast_expr;
mod collate_expr;
mod exists_expr;
mod function_expr;
mod in_expr;
mod is_expr;
mod like_expr;
mod precedence;
mod raise_expr;
mod regexp_match_expr;
use crate::parser::errors::ParsingError;
use crate::{BinaryOp, Expression, Keyword, LiteralValue, Parser, TokenType, UnaryOp};
use between_expr::BetweenExpressionParser;
use case_expr::CaseExpressionParser;
use cast_expr::CastExpressionParser;
use collate_expr::CollateExpressionParser;
use exists_expr::ExistsExpressionParser;
use function_expr::FunctionParser;
use in_expr::InExpressionParser;
use is_expr::IsExpressionParser;
use like_expr::LikeExpressionParser;
use precedence::get_precedence;
use raise_expr::RaiseExpressionParser;
use regexp_match_expr::RegexpMatchExpressionParser;

use super::IdentifierParser;

/// Trait for parsing expressions
/// The expression documentation can be found here:
/// https://www.sqlite.org/lang_expr.html
pub trait ExpressionParser {
    /// Parse a comma separated list of expressions
    fn parse_comma_separated_expressions(&mut self) -> Result<Vec<Expression>, ParsingError>;

    /// Parse an expression
    fn parse_expression(&mut self) -> Result<Expression, ParsingError>;

    /// Parse an expression using Pratt's parsing algorithm
    /// `rbp` is the right binding power of the current operator
    fn parse_sub_expression(&mut self, rbp: u8) -> Result<Expression, ParsingError>;

    /// Parse a prefix expression
    /// A prefix expression is an expression that does not have a left operand.
    fn parse_prefix_expression(&mut self) -> Result<Expression, ParsingError>;

    /// Parse an infix expression
    /// An infix expression is an expression that has a left operand.
    fn parse_infix_expression(
        &mut self,
        left: Expression,
        precedence: u8,
    ) -> Result<Expression, ParsingError>;

    /// Parse an expression which starts with the NOT keyword
    fn parse_not_expression(&mut self) -> Result<Expression, ParsingError>;
}

impl ExpressionParser for Parser<'_> {
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

    /// Parse an SQLite3 [expr](https://www.sqlite.org/lang_expr.html)
    fn parse_expression(&mut self) -> Result<Expression, ParsingError> {
        let expression = self.parse_sub_expression(0)?;

        if let Ok(Keyword::Collate) = self.peek_as_keyword() {
            CollateExpressionParser::parse_collate_expression(self, expression)
        } else {
            Ok(expression)
        }
    }

    /// Parse an expression using Pratt's parsing algorithm
    fn parse_sub_expression(&mut self, precedence: u8) -> Result<Expression, ParsingError> {
        let mut expression = self.parse_prefix_expression()?;

        loop {
            let current_token = self.peek_token()?;
            let next_precedence = get_precedence(&current_token.token_type);

            if precedence >= next_precedence {
                break;
            }
            expression = self.parse_infix_expression(expression, next_precedence)?;
        }
        Ok(expression)
    }

    /// Parse an infix expression
    /// An infix expression is an expression that has a left operand.
    fn parse_infix_expression(
        &mut self,
        left: Expression,
        precedence: u8,
    ) -> Result<Expression, ParsingError> {
        let token = self.peek_token()?;

        let binary_operator_result = BinaryOp::try_from(&token.token_type);

        if let Ok(binary_operator) = binary_operator_result {
            // Consume the operator token
            self.consume_as(token.token_type)?;

            let right = self.parse_sub_expression(precedence)?;
            Ok(Expression::BinaryOp(
                Box::new(left),
                binary_operator,
                Box::new(right),
            ))
        } else if let Ok(Keyword::Between) = self.peek_as_keyword() {
            return BetweenExpressionParser::parse_between_expression(self, left, false);
        } else if let Ok(Keyword::Like) = self.peek_as_keyword() {
            return LikeExpressionParser::parse_like_expression(self, left, false);
        } else if let Ok(Keyword::Is) = self.peek_as_keyword() {
            return IsExpressionParser::parse_is_expression(self, left);
        } else if let Ok(Keyword::In) = self.peek_as_keyword() {
            return InExpressionParser::parse_in_expression(self, left, false);
        } else if let Ok(Keyword::Glob) = self.peek_as_keyword() {
            return RegexpMatchExpressionParser::parse_regexp_match_expression(self, left, false);
        } else if let Ok(Keyword::Regexp) = self.peek_as_keyword() {
            return RegexpMatchExpressionParser::parse_regexp_match_expression(self, left, false);
        } else if let Ok(Keyword::Match) = self.peek_as_keyword() {
            return RegexpMatchExpressionParser::parse_regexp_match_expression(self, left, false);
        } else if let Ok(Keyword::Isnull) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Isnull)?;
            Ok(Expression::IsNull(Box::new(left)))
        } else if let Ok(Keyword::Notnull) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Notnull)?;
            Ok(Expression::IsNotNull(Box::new(left)))
        } else if let Ok(Keyword::Not) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Not)?;

            if let Ok(nested_keyword) = self.peek_as_keyword() {
                match nested_keyword {
                    Keyword::Null => {
                        self.consume_as_keyword(Keyword::Null)?;
                        Ok(Expression::IsNotNull(Box::new(left)))
                    }
                    Keyword::Between => {
                        BetweenExpressionParser::parse_between_expression(self, left, true)
                    }
                    Keyword::Like => LikeExpressionParser::parse_like_expression(self, left, true),
                    Keyword::Glob | Keyword::Regexp | Keyword::Match => {
                        RegexpMatchExpressionParser::parse_regexp_match_expression(self, left, true)
                    }
                    Keyword::In => InExpressionParser::parse_in_expression(self, left, true),
                    _ => {
                        return Err(ParsingError::UnexpectedKeyword(nested_keyword));
                    }
                }
            } else {
                Err(ParsingError::UnexpectedKeyword(Keyword::Not))
            }
        } else {
            Ok(left)
        }
    }

    /// Parse a prefix expression
    /// A prefix expression is an expression that does not have a left operand.
    fn parse_prefix_expression(&mut self) -> Result<Expression, ParsingError> {
        if let Ok(keyword) = self.peek_as_keyword() {
            match keyword {
                Keyword::Case => return CaseExpressionParser::parse_case_expression(self),
                Keyword::Cast => return CastExpressionParser::parse_cast_expression(self),
                Keyword::Not => return self.parse_not_expression(),
                Keyword::Exists => {
                    return ExistsExpressionParser::parse_exists_expression(self, false)
                }
                Keyword::Raise => return RaiseExpressionParser::parse_raise_expression(self),
                Keyword::Null => {
                    self.consume_as_keyword(Keyword::Null)?;
                    return Ok(Expression::LiteralValue(LiteralValue::Null));
                }
                Keyword::CurrentTime => {
                    self.consume_as_keyword(Keyword::CurrentTime)?;
                    return Ok(Expression::LiteralValue(LiteralValue::CurrentTime));
                }
                Keyword::CurrentDate => {
                    self.consume_as_keyword(Keyword::CurrentDate)?;
                    return Ok(Expression::LiteralValue(LiteralValue::CurrentDate));
                }
                Keyword::CurrentTimestamp => {
                    self.consume_as_keyword(Keyword::CurrentTimestamp)?;
                    return Ok(Expression::LiteralValue(LiteralValue::CurrentTimestamp));
                }
                _ => return Err(ParsingError::UnexpectedKeyword(keyword)),
            }
        } else if self.peek_as(TokenType::LeftParen).is_ok() {
            self.consume_as(TokenType::LeftParen)?;

            let expression = if let Ok(Keyword::Select) = self.peek_as_keyword() {
                ExistsExpressionParser::parse_exists_expression(self, false)?
            } else {
                Expression::ExpressionList(self.parse_comma_separated_expressions()?)
            };
            // The right parenthesis must be in the last token in the expression list
            self.consume_as(TokenType::RightParen)?;
            return Ok(expression);
        }
        let token = self.peek_token()?;

        match token.token_type {
            TokenType::Id(_) => {
                let id = IdentifierParser::parse_identifier(self)?;

                if self.peek_as(TokenType::LeftParen).is_ok() {
                    FunctionParser::parse_function(self, id)
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
                let expression = self.parse_sub_expression(0)?;
                self.consume_as(TokenType::RightParen)?;
                Ok(expression)
            }
            TokenType::Minus => {
                self.consume_as(TokenType::Minus)?;
                let pr = get_precedence(&TokenType::Minus);
                let expression = self.parse_sub_expression(pr)?;
                Ok(Expression::UnaryOp(UnaryOp::Minus, Box::new(expression)))
            }
            TokenType::Plus => {
                self.consume_as(TokenType::Plus)?;
                let pr = get_precedence(&TokenType::Plus);
                let expression = self.parse_sub_expression(pr)?;
                Ok(Expression::UnaryOp(UnaryOp::Plus, Box::new(expression)))
            }
            TokenType::BitNot => {
                self.consume_as(TokenType::BitNot)?;
                let pr = get_precedence(&TokenType::BitNot);
                let expression = self.parse_sub_expression(pr)?;
                Ok(Expression::UnaryOp(UnaryOp::BitNot, Box::new(expression)))
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
            _ => Err(ParsingError::UnexpectedToken(format!(
                "Unexpected token: {}",
                token.token_type
            ))),
        }
    }

    /// Parse an expression which starts with the NOT keyword
    fn parse_not_expression(&mut self) -> Result<Expression, ParsingError> {
        self.consume_as_keyword(Keyword::Not)?;

        if let Ok(Keyword::Exists) = self.peek_as_keyword() {
            ExistsExpressionParser::parse_exists_expression(self, true)
        } else {
            Ok(Expression::UnaryOp(
                UnaryOp::Not,
                Box::new(self.parse_expression()?),
            ))
        }
    }
}

#[cfg(test)]
pub(crate) mod test_utils {
    use crate::ast::Expression;
    use crate::{
        BinaryOp, CollateExpression, DataType, ExistsStatement, FunctionArg, FunctionExpression,
        Identifier, LiteralValue, OverClause, RaiseFunction, SelectStatement, UnaryOp,
    };

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
        let function = FunctionExpression {
            name: Identifier::Single(name.to_string()),
            arg,
            filter_clause: filter,
            over_clause: over,
        };

        Expression::Function(function)
    }

    pub fn exist_expr(is_not: bool, statement: SelectStatement) -> Expression {
        Expression::ExistsStatement(ExistsStatement {
            select_statement: statement,
            not: is_not,
        })
    }

    pub fn raise_expr(function: RaiseFunction) -> Expression {
        Expression::RaiseFunction(function)
    }

    pub fn cast_expr(expression: Expression, data_type: DataType) -> Expression {
        Expression::Cast(Box::new(expression), data_type)
    }

    pub fn collate_expr(expression: Expression, name: String) -> Expression {
        Expression::CollateExpression(CollateExpression {
            expression: Box::new(expression),
            collation_name: name,
        })
    }

    pub fn expr_list(expressions: Vec<Expression>) -> Expression {
        Expression::ExpressionList(expressions)
    }
}

#[cfg(test)]
mod literal_value_expression_tests {
    use crate::ast::{Expression, LiteralValue};
    use crate::parser::expression::test_utils::*;
    use crate::parser::test_utils::*;
    use crate::select::test_utils::select_expr;

    #[test]
    fn literal_values() {
        run_sunny_day_test("SELECT 1;", select_expr(numeric_expr("1")).into());

        run_sunny_day_test("SELECT 1.2;", select_expr(numeric_expr("1.2")).into());

        run_sunny_day_test(
            "SELECT 1.234567890;",
            select_expr(numeric_expr("1.234567890")).into(),
        );

        run_sunny_day_test(
            "SELECT 'Hello, world!';",
            select_expr(string_expr("'Hello, world!'")).into(),
        );

        run_sunny_day_test(
            "SELECT X'DEADBEEF';",
            select_expr(blob_expr("X'DEADBEEF'")).into(),
        );

        run_sunny_day_test("SELECT TRUE;", select_expr(boolean_expr(true)).into());

        run_sunny_day_test("SELECT FALSE;", select_expr(boolean_expr(false)).into());

        run_sunny_day_test("SELECT NULL;", select_expr(null_expr()).into());

        run_sunny_day_test(
            "SELECT CURRENT_TIME;",
            select_expr(Expression::LiteralValue(LiteralValue::CurrentTime)).into(),
        );

        run_sunny_day_test(
            "SELECT CURRENT_DATE;",
            select_expr(Expression::LiteralValue(LiteralValue::CurrentDate)).into(),
        );

        run_sunny_day_test(
            "SELECT CURRENT_TIMESTAMP;",
            select_expr(Expression::LiteralValue(LiteralValue::CurrentTimestamp)).into(),
        );
    }
}

#[cfg(test)]
mod bind_parameter_expression_tests {
    use crate::parser::expression::test_utils::*;
    use crate::parser::test_utils::*;
    use crate::select::test_utils::select_expr;

    #[test]
    fn bind_parameter_expressions() {
        run_sunny_day_test("SELECT ?;", select_expr(bind_expr("?")).into());
        run_sunny_day_test("SELECT ?1;", select_expr(bind_expr("?1")).into());
        run_sunny_day_test("SELECT :name;", select_expr(bind_expr(":name")).into());
        run_sunny_day_test("SELECT @var;", select_expr(bind_expr("@var")).into());
        run_sunny_day_test("SELECT $value;", select_expr(bind_expr("$value")).into());
        run_sunny_day_test("SELECT #param;", select_expr(bind_expr("#param")).into());
    }
}

#[cfg(test)]
mod unary_op_tests {
    use crate::select::test_utils::select_expr;
    use crate::UnaryOp;

    use crate::parser::expression::test_utils::*;
    use crate::parser::test_utils::*;

    #[test]
    fn unary_ops() {
        run_sunny_day_test(
            "SELECT +1;",
            select_expr(unary_op(UnaryOp::Plus, numeric_expr("1"))).into(),
        );
        run_sunny_day_test(
            "SELECT -1;",
            select_expr(unary_op(UnaryOp::Minus, numeric_expr("1"))).into(),
        );
        run_sunny_day_test(
            "SELECT -abc;",
            select_expr(unary_op(UnaryOp::Minus, identifier_expr(&["abc"]))).into(),
        );
        run_sunny_day_test(
            "SELECT +abc;",
            select_expr(unary_op(UnaryOp::Plus, identifier_expr(&["abc"]))).into(),
        );

        run_sunny_day_test(
            "SELECT -+1;",
            select_expr(unary_op(
                UnaryOp::Minus,
                unary_op(UnaryOp::Plus, numeric_expr("1")),
            ))
            .into(),
        );
        run_sunny_day_test(
            "SELECT ++1;",
            select_expr(unary_op(
                UnaryOp::Plus,
                unary_op(UnaryOp::Plus, numeric_expr("1")),
            ))
            .into(),
        );
    }

    #[test]
    fn not_value_expressions() {
        run_sunny_day_test(
            "SELECT NOT 1;",
            select_expr(unary_op(UnaryOp::Not, numeric_expr("1"))).into(),
        );

        run_sunny_day_test(
            "SELECT NOT TRUE;",
            select_expr(unary_op(UnaryOp::Not, boolean_expr(true))).into(),
        );

        run_sunny_day_test(
            "SELECT NOT col1;",
            select_expr(unary_op(UnaryOp::Not, identifier_expr(&["col1"]))).into(),
        );
    }
}

#[cfg(test)]
mod binary_op_tests {
    use crate::parser::expression::test_utils::*;
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::{BinaryOp, FunctionArg, FunctionArgType, UnaryOp};

    #[test]
    fn binary_ops() {
        use BinaryOp::*;
        let operators = [
            Plus,
            Minus,
            Mul,
            Div,
            And,
            Or,
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
            run_sunny_day_test(
                &format!("SELECT 1 {} 2;", op),
                select_expr(binary_op(op, numeric_expr("1"), numeric_expr("2"))).into(),
            );
        }
    }

    #[test]
    fn binary_with_prefix() {
        run_sunny_day_test(
            "SELECT 1 ++ 2;",
            select_expr(binary_op(
                BinaryOp::Plus,
                numeric_expr("1"),
                unary_op(UnaryOp::Plus, numeric_expr("2")),
            ))
            .into(),
        );
    }

    #[test]
    fn binary_operation_the_same_precedence() {
        run_sunny_day_test(
            "SELECT 1 + 2 + 3;",
            select_expr(binary_op(
                BinaryOp::Plus,
                binary_op(BinaryOp::Plus, numeric_expr("1"), numeric_expr("2")),
                numeric_expr("3"),
            ))
            .into(),
        );

        run_sunny_day_test(
            "SELECT 1 * 2 / 3;",
            select_expr(binary_op(
                BinaryOp::Div,
                binary_op(BinaryOp::Mul, numeric_expr("1"), numeric_expr("2")),
                numeric_expr("3"),
            ))
            .into(),
        );
    }

    #[test]
    fn binary_operation_different_precedence() {
        run_sunny_day_test(
            "SELECT 1 + 2 * 3;",
            select_expr(binary_op(
                BinaryOp::Plus,
                numeric_expr("1"),
                binary_op(BinaryOp::Mul, numeric_expr("2"), numeric_expr("3")),
            ))
            .into(),
        );
    }

    #[test]
    fn binary_operation_different_precedence2() {
        run_sunny_day_test(
            "SELECT 1 + 2 * 3 - 4",
            select_expr(binary_op(
                BinaryOp::Minus,
                binary_op(
                    BinaryOp::Plus,
                    numeric_expr("1"),
                    binary_op(BinaryOp::Mul, numeric_expr("2"), numeric_expr("3")),
                ),
                numeric_expr("4"),
            ))
            .into(),
        );
    }

    #[test]
    fn binary_operation_different_precedence3() {
        run_sunny_day_test(
            "SELECT col1 + col2 * 3;",
            select_expr(binary_op(
                BinaryOp::Plus,
                identifier_expr(&["col1"]),
                binary_op(BinaryOp::Mul, identifier_expr(&["col2"]), numeric_expr("3")),
            ))
            .into(),
        );
    }

    #[test]
    fn binary_operation_precedence_with_function() {
        run_sunny_day_test(
            "SELECT col1 + col2 * max(1, 3, 4);",
            select_expr(binary_op(
                BinaryOp::Plus,
                identifier_expr(&["col1"]),
                binary_op(
                    BinaryOp::Mul,
                    identifier_expr(&["col2"]),
                    function(
                        "max",
                        FunctionArg {
                            distinct: false,
                            arguments: vec![
                                FunctionArgType::Expression(numeric_expr("1")),
                                FunctionArgType::Expression(numeric_expr("3")),
                                FunctionArgType::Expression(numeric_expr("4")),
                            ],
                        },
                        None,
                        None,
                    ),
                ),
            ))
            .into(),
        );
    }
}

#[cfg(test)]
mod null_expression_tests {
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::Expression;

    use crate::parser::expression::test_utils::*;

    #[test]
    fn isnull() {
        run_sunny_day_test(
            "SELECT 1 ISNULL;",
            select_expr(Expression::IsNull(Box::new(numeric_expr("1")))).into(),
        );
    }

    #[test]
    fn notnull() {
        run_sunny_day_test(
            "SELECT 1 NOTNULL;",
            select_expr(Expression::IsNotNull(Box::new(numeric_expr("1")))).into(),
        );

        run_sunny_day_test(
            "SELECT 1 NOT NULL;",
            select_expr(Expression::IsNotNull(Box::new(numeric_expr("1")))).into(),
        );
    }
}

#[cfg(test)]
mod parenthesized_expression_tests {
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::{BinaryOp, DataType, Expression, RaiseFunction, UnaryOp};

    use crate::parser::expression::test_utils::*;

    #[test]
    fn parenthesized_expression() {
        run_sunny_day_test(
            "SELECT (1 + 2 * 3)",
            select_expr(Expression::ExpressionList(vec![binary_op(
                BinaryOp::Plus,
                numeric_expr("1"),
                binary_op(BinaryOp::Mul, numeric_expr("2"), numeric_expr("3")),
            )]))
            .into(),
        );
    }

    #[test]
    fn parenthesized_binary_expression() {
        run_sunny_day_test(
            "SELECT (1 + 2) * 3;",
            select_expr(binary_op(
                BinaryOp::Mul,
                Expression::ExpressionList(vec![binary_op(
                    BinaryOp::Plus,
                    numeric_expr("1"),
                    numeric_expr("2"),
                )]),
                numeric_expr("3"),
            ))
            .into(),
        );
    }

    #[test]
    fn parenthesized_two_expressions() {
        run_sunny_day_test(
            "SELECT (1 + 2, 3 / 4);",
            select_expr(Expression::ExpressionList(vec![
                binary_op(BinaryOp::Plus, numeric_expr("1"), numeric_expr("2")),
                binary_op(BinaryOp::Div, numeric_expr("3"), numeric_expr("4")),
            ]))
            .into(),
        );
    }

    #[test]
    fn parenthesized_multiple_expressions() {
        run_sunny_day_test(
            "SELECT (1 + 2, 3 / 4, cast(5 as int), -5, raise (ignore));",
            select_expr(Expression::ExpressionList(vec![
                binary_op(BinaryOp::Plus, numeric_expr("1"), numeric_expr("2")),
                binary_op(BinaryOp::Div, numeric_expr("3"), numeric_expr("4")),
                cast_expr(numeric_expr("5"), DataType::PlainDataType("int".into())),
                unary_op(UnaryOp::Minus, numeric_expr("5")),
                raise_expr(RaiseFunction::Ignore),
            ]))
            .into(),
        );
    }
}
