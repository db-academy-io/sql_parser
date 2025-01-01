use super::{
    case_expr::CaseExpressionParser, cast_expr::CastExpressionParser,
    exists_expr::ExistsExpressionParser, precedence::get_precedence,
    raise_expr::RaiseExpressionParser, ExpressionParser, FunctionParser,
};
use crate::{
    expression::{
        between_expr::BetweenExpressionParser, in_expr::InExpressionParser,
        is_expr::IsExpressionParser, like_expr::LikeExpressionParser,
        regexp_match_expr::RegexpMatchExpressionParser,
    },
    parser::ParsingError,
    BinaryOp, Expression, IdentifierParser, Keyword, LiteralValue, Parser, TokenType,
    UnaryMatchingExpression, UnaryOp,
};

pub trait PrattParser {
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

    /// Parse an expression which starts with the NOT keyword
    fn parse_not_expression(&mut self) -> Result<Expression, ParsingError>;
}

impl<'a> PrattParser for Parser<'a> {
    /// Parse an expression using Pratt's parsing algorithm
    fn parse_expression_pratt(&mut self, precedence: u8) -> Result<Expression, ParsingError> {
        dbg!("parse_expression_pratt");
        let mut expression = self.parse_prefix()?;

        dbg!("prefix expression: {:?}", &expression);
        loop {
            println!("precedence: {:?}", precedence);
            let current_token = self.peek_token()?;
            let next_precedence = get_precedence(&current_token.token_type);

            dbg!("current_token: {:?}", &current_token);
            dbg!(
                "precedence: {:?}, next_precedence: {:?}",
                &precedence,
                &next_precedence
            );

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
        dbg!("parse_infix");

        let token = self.peek_token()?;

        let binary_operator_result = BinaryOp::try_from(&token.token_type);

        if let Ok(binary_operator) = binary_operator_result {
            // Consume the operator token
            self.consume_as(token.token_type)?;

            let right = self.parse_expression_pratt(precedence)?;
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
            Ok(Expression::UnaryMatchingExpression(
                Box::new(left),
                UnaryMatchingExpression::IsNull,
            ))
        } else if let Ok(Keyword::Notnull) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Notnull)?;
            Ok(Expression::UnaryMatchingExpression(
                Box::new(left),
                UnaryMatchingExpression::IsNotNull,
            ))
        } else if let Ok(Keyword::Not) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Not)?;

            if let Ok(nested_keyword) = self.peek_as_keyword() {
                match nested_keyword {
                    Keyword::Null => {
                        self.consume_as_keyword(Keyword::Null)?;
                        Ok(Expression::UnaryMatchingExpression(
                            Box::new(left),
                            UnaryMatchingExpression::IsNotNull,
                        ))
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
    fn parse_prefix(&mut self) -> Result<Expression, ParsingError> {
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
                let id = self.parse_identifier()?;

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
                let expression = self.parse_expression_pratt(0)?;
                self.consume_as(TokenType::RightParen)?;
                Ok(expression)
            }
            TokenType::Minus => {
                self.consume_as(TokenType::Minus)?;
                let pr = get_precedence(&TokenType::Minus);
                let expression = self.parse_expression_pratt(pr)?;
                Ok(Expression::UnaryOp(UnaryOp::Minus, Box::new(expression)))
            }
            TokenType::Plus => {
                self.consume_as(TokenType::Plus)?;
                let pr = get_precedence(&TokenType::Plus);
                let expression = self.parse_expression_pratt(pr)?;
                Ok(Expression::UnaryOp(UnaryOp::Plus, Box::new(expression)))
            }
            TokenType::BitNot => {
                self.consume_as(TokenType::BitNot)?;
                let pr = get_precedence(&TokenType::BitNot);
                let expression = self.parse_expression_pratt(pr)?;
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
mod unary_matching_expression_tests {
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::{Expression, UnaryMatchingExpression};

    use crate::parser::expression::test_utils::*;

    fn unary_matching_expression(
        expression: Expression,
        unary_matching_expression: UnaryMatchingExpression,
    ) -> Expression {
        Expression::UnaryMatchingExpression(Box::new(expression), unary_matching_expression)
    }

    #[test]
    fn isnull() {
        run_sunny_day_test(
            "SELECT 1 ISNULL;",
            select_expr(unary_matching_expression(
                numeric_expr("1"),
                UnaryMatchingExpression::IsNull,
            ))
            .into(),
        );
    }

    #[test]
    fn notnull() {
        run_sunny_day_test(
            "SELECT 1 NOTNULL;",
            select_expr(unary_matching_expression(
                numeric_expr("1"),
                UnaryMatchingExpression::IsNotNull,
            ))
            .into(),
        );

        run_sunny_day_test(
            "SELECT 1 NOT NULL;",
            select_expr(unary_matching_expression(
                numeric_expr("1"),
                UnaryMatchingExpression::IsNotNull,
            ))
            .into(),
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
