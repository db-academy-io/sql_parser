use crate::{
    Expression, Function, FunctionArg, FunctionArgType, Identifier, Keyword, NullsOrdering,
    Ordering, OrderingTerm, OverClause, Parser, ParsingError, TokenType,
};

use super::ExpressionParser;

pub trait FunctionParser {
    /// Parse a function
    fn parse_function(&mut self, name: Identifier) -> Result<Expression, ParsingError>;

    /// Parse a function argument
    fn parse_function_arg(&mut self) -> Result<FunctionArg, ParsingError>;

    /// Parse a function ordering terms
    fn parse_function_ordering_terms(&mut self) -> Result<Vec<OrderingTerm>, ParsingError>;

    /// Parse a function filter clause
    fn parse_function_filter_clause(&mut self) -> Result<Expression, ParsingError>;

    /// Parse a function over clause
    fn parse_function_over_clause(&mut self) -> Result<OverClause, ParsingError>;
}

impl<'a> FunctionParser for Parser<'a> {
    /// Parse a function
    fn parse_function(&mut self, name: Identifier) -> Result<Expression, ParsingError> {
        dbg!("parse_function: name: {:?}", &name);
        let mut function = Function {
            name,
            arg: FunctionArg::default(),
            filter_clause: None,
            over_clause: None,
        };

        if self.peek_as(TokenType::LeftParen).is_ok() {
            // consume the left parenthesis
            self.consume_token()?;
        } else {
            return Err(ParsingError::UnexpectedToken(format!(
                "Expected left parenthesis, got: {}",
                self.peek_token()?.token_type
            )));
        }

        dbg!("AAAA");
        function.arg = self.parse_function_arg()?;
        dbg!("BBBB");
        dbg!("parse_function: function.args: {:?}", &function.arg);
        if let Ok(token) = self.peek_as_keyword() {
            if token == Keyword::Filter {
                self.consume_token()?;
                function.filter_clause = Some(Box::new(self.parse_function_filter_clause()?));
            }
        }

        if let Ok(token) = self.peek_as_keyword() {
            if token == Keyword::Over {
                self.consume_token()?;
                function.over_clause = Some(self.parse_function_over_clause()?);
            }
        }

        Ok(Expression::Function(function))
    }

    /// Parse a function arguments
    fn parse_function_arg(&mut self) -> Result<FunctionArg, ParsingError> {
        match self.peek_token()?.token_type {
            // A wildcard argument, e.g. `abc(*)`
            TokenType::Star => {
                // Consume the star token
                self.consume_token()?;
                Ok(FunctionArg {
                    distinct: false,
                    arguments: vec![FunctionArgType::Wildcard],
                })
            }
            // No argument, e.g. `abc()`
            TokenType::RightParen => {
                // Consume the right parenthesis
                self.consume_token()?;
                Ok(FunctionArg {
                    distinct: false,
                    arguments: vec![],
                })
            }
            _ => {
                let mut function_argument = FunctionArg::default();
                if let Ok(Keyword::Distinct) = self.peek_as_keyword() {
                    function_argument.distinct = true;
                    self.consume_token()?;
                }
                dbg!(
                    "parse_function_arg: function_argument.distinct: {:?}",
                    &function_argument.distinct
                );

                while let Ok(expression) = self.parse_expression() {
                    dbg!("parse_function_arg: expression: {:?}", &expression);
                    function_argument
                        .arguments
                        .push(FunctionArgType::Expression(expression));

                    if self.peek_as(TokenType::Comma).is_ok() {
                        // Consume the comma
                        self.consume_token()?;
                    } else {
                        break;
                    }
                }
                dbg!(
                    "parse_function_arg: function_argument.arguments: {:?}",
                    &function_argument.arguments
                );

                if let Ok(Keyword::Order) = self.peek_as_keyword() {
                    // Consume the order keyword
                    self.consume_token()?;

                    if let Ok(Keyword::By) = self.peek_as_keyword() {
                        // Consume the by keyword
                        self.consume_token()?;
                    } else {
                        return Err(ParsingError::UnexpectedToken(format!(
                            "Expected BY keyword, got: {}",
                            self.peek_token()?.token_type
                        )));
                    }

                    let last_expression = function_argument
                        .arguments
                        .pop()
                        .expect("Expected at least one argument");

                    match last_expression {
                        FunctionArgType::Expression(expression) => {
                            let ordering_terms = self.parse_function_ordering_terms()?;
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

    /// Parse a function ordering terms
    fn parse_function_ordering_terms(&mut self) -> Result<Vec<OrderingTerm>, ParsingError> {
        let mut ordering_terms = vec![];

        while let Ok(expression) = self.parse_expression() {
            // No need to check for CollateExpression, because it will be parsed as an Expression

            let mut ordering_term = OrderingTerm {
                expression: Box::new(expression),
                ordering: None,
                nulls_ordering: None,
            };

            if let Ok(Keyword::Asc) = self.peek_as_keyword() {
                ordering_term.ordering = Some(Ordering::Asc);
                self.consume_token()?;
            } else if let Ok(Keyword::Desc) = self.peek_as_keyword() {
                ordering_term.ordering = Some(Ordering::Desc);
                self.consume_token()?;
            }

            if let Ok(Keyword::Nulls) = self.peek_as_keyword() {
                self.consume_token()?;

                if let Ok(Keyword::First) = self.peek_as_keyword() {
                    ordering_term.nulls_ordering = Some(NullsOrdering::First);
                    self.consume_token()?;
                } else if let Ok(Keyword::Last) = self.peek_as_keyword() {
                    ordering_term.nulls_ordering = Some(NullsOrdering::Last);
                    self.consume_token()?;
                }
            }

            ordering_terms.push(ordering_term);

            if self.peek_as(TokenType::Comma).is_ok() {
                self.consume_token()?;
            } else {
                break;
            }
        }

        Ok(ordering_terms)
    }

    /// Parse a function filter clause
    fn parse_function_filter_clause(&mut self) -> Result<Expression, ParsingError> {
        todo!()
    }

    /// Parse a function over clause
    fn parse_function_over_clause(&mut self) -> Result<OverClause, ParsingError> {
        todo!()
    }
}

#[cfg(test)]
mod function_expression_tests {
    use crate::{
        expression::test_utils::*, BinaryOp, FunctionArg, FunctionArgType, NullsOrdering, Ordering,
        OrderingTerm,
    };

    #[test]
    fn test_expression_function_basic_empty_args() {
        run_sunny_day_test(
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
        run_sunny_day_test(
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
        run_sunny_day_test(
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
        run_sunny_day_test(
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
        run_sunny_day_test(
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
                    BinaryOp::Add,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                )),
                FunctionArgType::Expression(binary_op_expression(
                    BinaryOp::Add,
                    numeric_literal_expression("4"),
                    binary_op_expression(
                        BinaryOp::Div,
                        numeric_literal_expression("5"),
                        numeric_literal_expression("6"),
                    ),
                )),
            ],
        };
        run_sunny_day_test(
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
        run_sunny_day_test(
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
        run_sunny_day_test(
            "SELECT abc(1 order by 2 asc, 3 desc, 4 nulls first, 5 desc nulls last);",
            &function_expression("abc", expected_arg, None, None),
        );
    }
}
