use crate::{
    parser::window_definition::WindowDefinitionParser, Expression, Function, FunctionArg,
    FunctionArgType, Identifier, Keyword, OverClause, Parser, ParsingError, TokenType,
};

use super::ExpressionParser;

pub trait FunctionParser {
    /// Parse a function
    fn parse_function(&mut self, name: Identifier) -> Result<Expression, ParsingError>;

    /// Parse a function argument
    fn parse_function_arg(&mut self) -> Result<FunctionArg, ParsingError>;

    /// Parse a function filter clause
    fn parse_function_filter_clause(&mut self) -> Result<Expression, ParsingError>;
}

impl<'a> FunctionParser for Parser<'a> {
    /// Parse a function
    fn parse_function(&mut self, name: Identifier) -> Result<Expression, ParsingError> {
        let mut function = Function {
            name,
            arg: FunctionArg::default(),
            filter_clause: None,
            over_clause: None,
        };

        // Consume the opening left parenthesis
        self.consume_as(TokenType::LeftParen)?;

        function.arg = self.parse_function_arg()?;

        // Consume the closing right parenthesis
        self.consume_as(TokenType::RightParen)?;

        if let Ok(Keyword::Filter) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Filter)?;
            function.filter_clause = Some(Box::new(self.parse_function_filter_clause()?));
        }

        if let Ok(Keyword::Over) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Over)?;

            function.over_clause = if let Ok(identifier) = self.consume_as_id() {
                Some(OverClause::WindowName(identifier.to_string()))
            } else {
                Some(OverClause::WindowDefinition(
                    self.parse_window_definition()?,
                ))
            };
        }

        Ok(Expression::Function(function))
    }

    /// Parse a function arguments
    fn parse_function_arg(&mut self) -> Result<FunctionArg, ParsingError> {
        match self.peek_token()?.token_type {
            // A wildcard argument, e.g. `abc(*)`
            TokenType::Star => {
                self.consume_as(TokenType::Star)?;
                Ok(FunctionArg {
                    distinct: false,
                    arguments: vec![FunctionArgType::Wildcard],
                })
            }
            // No argument, e.g. `abc()`
            TokenType::RightParen => {
                // Do not consume the right parenthesis, as it will be consumed in the upper level
                Ok(FunctionArg {
                    distinct: false,
                    arguments: vec![],
                })
            }
            _ => {
                let mut function_argument = FunctionArg {
                    distinct: self.consume_as_keyword(Keyword::Distinct).is_ok(),
                    arguments: vec![],
                };

                while let Ok(expression) = self.parse_expression() {
                    function_argument
                        .arguments
                        .push(FunctionArgType::Expression(expression));

                    if self.consume_as(TokenType::Comma).is_err() {
                        break;
                    }
                }

                if let Ok(Keyword::Order) = self.peek_as_keyword() {
                    self.consume_as_keyword(Keyword::Order)?;
                    // Consume the mandatory BY keyword
                    self.consume_as_keyword(Keyword::By)?;

                    if function_argument.arguments.is_empty() {
                        return Err(ParsingError::UnexpectedParsingState(
                            "Expected at least one argument".to_string(),
                        ));
                    }

                    let last_expression = function_argument
                        .arguments
                        .pop()
                        .expect("Expected at least one argument");

                    match last_expression {
                        FunctionArgType::Expression(expression) => {
                            let ordering_terms = self.parse_ordering_terms()?;
                            let arg = FunctionArgType::OrderedBy(expression, ordering_terms);
                            function_argument.arguments.push(arg);
                        }
                        FunctionArgType::OrderedBy(_, _) => {
                            return Err(ParsingError::UnexpectedParsingState(
                                "Expected expression, got OrderedBy expression".to_string(),
                            ));
                        }
                        FunctionArgType::Wildcard => {
                            return Err(ParsingError::UnexpectedParsingState(
                                "Expected expression, got Wildcard expression".to_string(),
                            ));
                        }
                    }
                }

                Ok(function_argument)
            }
        }
    }

    /// Parse a function filter clause
    fn parse_function_filter_clause(&mut self) -> Result<Expression, ParsingError> {
        // Consume the opening left parenthesis
        self.consume_as(TokenType::LeftParen)?;

        // Consume the mandatory WHERE keyword
        self.consume_as_keyword(Keyword::Where)?;

        let expression = self.parse_expression()?;

        self.consume_as(TokenType::RightParen)?;
        Ok(expression)
    }
}

#[cfg(test)]
mod function_expression_tests {
    use crate::{
        expression::test_utils::*, BetweenFrameSpec, BetweenFrameSpecType, BinaryOp, FrameSpec,
        FrameSpecExclude, FrameSpecType, FrameType, FunctionArg, FunctionArgType, NullsOrdering,
        Ordering, OrderingTerm, OverClause, WindowDefinition,
    };

    #[test]
    fn test_expression_function_basic_empty_args() {
        run_sunny_day_expression_test(
            "SELECT abc();",
            &function_expression("abc", FunctionArg::default(), None, None),
        );
    }

    #[test]
    fn test_expression_function_basic_wildcard_arg() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![FunctionArgType::Wildcard],
        };
        run_sunny_day_expression_test(
            "SELECT abc(*);",
            &function_expression("abc", expected_arg, None, None),
        );
    }

    #[test]
    fn test_expression_function_basic_literal_arg() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![FunctionArgType::Expression(numeric_literal_expression("1"))],
        };
        run_sunny_day_expression_test(
            "SELECT abc(1);",
            &function_expression("abc", expected_arg, None, None),
        );
    }

    #[test]
    fn test_expression_function_basic_distinct_literal_arg() {
        let expected_arg = FunctionArg {
            distinct: true,
            arguments: vec![FunctionArgType::Expression(numeric_literal_expression("1"))],
        };
        run_sunny_day_expression_test(
            "SELECT abc(distinct 1);",
            &function_expression("abc", expected_arg, None, None),
        );
    }

    #[test]
    fn test_expression_function_multiple_literal_args() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![
                FunctionArgType::Expression(numeric_literal_expression("1")),
                FunctionArgType::Expression(numeric_literal_expression("2")),
            ],
        };
        run_sunny_day_expression_test(
            "SELECT abc(1, 2);",
            &function_expression("abc", expected_arg, None, None),
        );
    }

    #[test]
    fn test_expression_function_multiple_expression_args() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![
                FunctionArgType::Expression(binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                )),
                FunctionArgType::Expression(binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("4"),
                    binary_op_expression(
                        BinaryOp::Div,
                        numeric_literal_expression("5"),
                        numeric_literal_expression("6"),
                    ),
                )),
            ],
        };
        run_sunny_day_expression_test(
            "SELECT abc(1 + 2, 4 + 5 / 6);",
            &function_expression("abc", expected_arg, None, None),
        );
    }

    #[test]
    fn test_expression_function_expression_arg_with_order_by() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![FunctionArgType::OrderedBy(
                numeric_literal_expression("1"),
                vec![OrderingTerm {
                    expression: Box::new(numeric_literal_expression("2")),
                    ordering: None,
                    nulls_ordering: None,
                }],
            )],
        };
        run_sunny_day_expression_test(
            "SELECT abc(1 order by 2);",
            &function_expression("abc", expected_arg, None, None),
        );
    }

    #[test]
    fn test_expression_function_expression_arg_with_order_by_and_nulls_ordering() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![FunctionArgType::OrderedBy(
                numeric_literal_expression("1"),
                vec![
                    OrderingTerm {
                        expression: Box::new(numeric_literal_expression("2")),
                        ordering: Some(Ordering::Asc),
                        nulls_ordering: None,
                    },
                    OrderingTerm {
                        expression: Box::new(numeric_literal_expression("3")),
                        ordering: Some(Ordering::Desc),
                        nulls_ordering: None,
                    },
                    OrderingTerm {
                        expression: Box::new(numeric_literal_expression("4")),
                        ordering: None,
                        nulls_ordering: Some(NullsOrdering::First),
                    },
                    OrderingTerm {
                        expression: Box::new(numeric_literal_expression("5")),
                        ordering: Some(Ordering::Desc),
                        nulls_ordering: Some(NullsOrdering::Last),
                    },
                ],
            )],
        };
        run_sunny_day_expression_test(
            "SELECT abc(1 order by 2 asc, 3 desc, 4 nulls first, 5 desc nulls last);",
            &function_expression("abc", expected_arg, None, None),
        );
    }

    #[test]
    fn test_expression_function_with_filter_clause() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![FunctionArgType::Expression(numeric_literal_expression("1"))],
        };

        let filter_clause = binary_op_expression(
            BinaryOp::GreaterThan,
            numeric_literal_expression("2"),
            numeric_literal_expression("3"),
        );

        run_sunny_day_expression_test(
            "SELECT abc(1) filter (where 2 > 3);",
            &function_expression("abc", expected_arg, Some(Box::new(filter_clause)), None),
        );
    }

    #[test]
    fn test_expression_function_with_over_clause_window_name() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![FunctionArgType::Expression(numeric_literal_expression("1"))],
        };

        run_sunny_day_expression_test(
            "SELECT abc(1) over a;",
            &function_expression(
                "abc",
                expected_arg,
                None,
                Some(OverClause::WindowName("a".to_string())),
            ),
        );
    }

    #[test]
    fn test_expression_function_with_over_clause_partition_by() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![FunctionArgType::Expression(numeric_literal_expression("1"))],
        };

        let over_clause = WindowDefinition {
            base_window_name: None,
            partition_by: Some(vec![numeric_literal_expression("1")]),
            order_by: None,
            frame_spec: None,
        };

        run_sunny_day_expression_test(
            "SELECT abc(1) over (partition by 1);",
            &function_expression(
                "abc",
                expected_arg,
                None,
                Some(OverClause::WindowDefinition(over_clause)),
            ),
        );
    }

    #[test]
    fn test_expression_function_with_over_clause_order_by() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![FunctionArgType::Expression(numeric_literal_expression("1"))],
        };

        let over_clause = WindowDefinition {
            base_window_name: None,
            partition_by: None,
            order_by: Some(vec![OrderingTerm {
                expression: Box::new(numeric_literal_expression("1")),
                ordering: Some(Ordering::Asc),
                nulls_ordering: Some(NullsOrdering::Last),
            }]),
            frame_spec: None,
        };

        run_sunny_day_expression_test(
            "SELECT abc(1) over (order by 1 asc nulls last);",
            &function_expression(
                "abc",
                expected_arg,
                None,
                Some(OverClause::WindowDefinition(over_clause)),
            ),
        );
    }

    #[test]
    fn test_expression_function_with_over_clause_frame_spec() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![FunctionArgType::Expression(numeric_literal_expression("1"))],
        };

        let over_clause = WindowDefinition {
            base_window_name: Some("a".to_string()),
            partition_by: None,
            order_by: None,
            frame_spec: Some(FrameSpec {
                frame_type: FrameType::Groups,
                frame_spec_type: FrameSpecType::Between(BetweenFrameSpec {
                    start: BetweenFrameSpecType::Preceding(Box::new(numeric_literal_expression(
                        "1",
                    ))),
                    end: BetweenFrameSpecType::CurrentRow,
                }),
                exclude: Some(FrameSpecExclude::CurrentRow),
            }),
        };

        run_sunny_day_expression_test(
            "SELECT abc(1) over (a groups between 1 preceding and current row exclude current row);",
            &function_expression("abc", expected_arg, None, Some(OverClause::WindowDefinition(over_clause))),
        );
    }
}

// #[cfg(test)]
// mod math_functions {
//     use crate::{expression::test_utils::*, FunctionArg, FunctionArgType};
//     // acos(X)
//     // acosh(X)
//     // asin(X)
//     // asinh(X)
//     // atan(X)
//     // atan2(Y,X)
//     // atanh(X)
//     // ceil(X)
//     // ceiling(X)
//     // cos(X)
//     // cosh(X)
//     // degrees(X)
//     // exp(X)
//     // floor(X)
//     // ln(X)
//     // log(B,X)
//     // log(X)
//     // log10(X)
//     // log2(X)
//     // mod(X,Y)
//     // pi()
//     // pow(X,Y)
//     // power(X,Y)
//     // radians(X)
//     // sin(X)
//     // sinh(X)
//     // sqrt(X)
//     // tan(X)
//     // tanh(X)
//     // trunc(X)

//     #[test]
//     fn test_expression_function_abs() {
//         run_sunny_day_test(
//             "SELECT abs(-1);",
//             &function_expression(
//                 "abs",
//                 FunctionArg {
//                     distinct: false,
//                     arguments: vec![FunctionArgType::Expression(numeric_literal_expression("-1"))],
//                 },
//                 None,
//                 None,
//             ),
//         );
//     }
// }
