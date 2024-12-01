use crate::{
    BetweenFrameSpec, BetweenFrameSpecType, Expression, FrameSpec, FrameSpecExclude, FrameSpecType,
    FrameType, Function, FunctionArg, FunctionArgType, Identifier, Keyword, NullsOrdering,
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

    /// Parse a frame spec
    fn parse_function_over_clause_frame_spec(&mut self) -> Result<FrameSpec, ParsingError>;

    /// Parse a frame spec between clause
    fn parse_function_over_clause_frame_spec_between(
        &mut self,
    ) -> Result<FrameSpecType, ParsingError>;
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

        function.arg = self.parse_function_arg()?;

        if self.peek_as(TokenType::RightParen).is_ok() {
            // consume the right parenthesis
            self.consume_token()?;
        }

        if let Ok(Keyword::Filter) = self.peek_as_keyword() {
            self.consume_token()?;
            function.filter_clause = Some(Box::new(self.parse_function_filter_clause()?));
        }

        if let Ok(Keyword::Over) = self.peek_as_keyword() {
            self.consume_token()?;
            function.over_clause = Some(self.parse_function_over_clause()?);
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
                while let Ok(expression) = self.parse_expression() {
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
        self.peek_as(TokenType::LeftParen)?;
        // consume the left parenthesis
        self.consume_token()?;

        if let Ok(Keyword::Where) = self.peek_as_keyword() {
            self.consume_token()?;
        } else {
            return Err(ParsingError::UnexpectedToken(format!(
                "Expected WHERE keyword, got: {}",
                self.peek_token()?.token_type
            )));
        }

        let expression = self.parse_expression()?;

        self.peek_as(TokenType::RightParen)?;
        // consume the right parenthesis
        self.consume_token()?;

        Ok(expression)
    }

    /// Parse a function over clause
    fn parse_function_over_clause(&mut self) -> Result<OverClause, ParsingError> {
        if let Ok(identifier) = self.peek_as_id() {
            let over_clause = OverClause {
                window_name: Some(identifier.to_string()),
                ..Default::default()
            };
            self.consume_token()?;
            return Ok(over_clause);
        }

        self.peek_as(TokenType::LeftParen)?;
        // consume the left parenthesis
        self.consume_token()?;

        let mut over_clause = OverClause::default();
        if let Ok(base_window_name) = self.peek_as_id() {
            over_clause.window_name = Some(base_window_name.to_string());
            self.consume_token()?;
        }

        if let Ok(Keyword::Partition) = self.peek_as_keyword() {
            self.consume_token()?;

            self.consume_keyword(Keyword::By)?;

            while let Ok(expression) = self.parse_expression() {
                match over_clause.partition_by.as_mut() {
                    Some(partition_by) => partition_by.push(expression),
                    None => over_clause.partition_by = Some(vec![expression]),
                }
                if self.peek_as(TokenType::Comma).is_ok() {
                    self.consume_token()?;
                } else {
                    break;
                }
            }
        }

        if let Ok(Keyword::Order) = self.peek_as_keyword() {
            self.consume_token()?;

            self.consume_keyword(Keyword::By)?;
            let ordering_terms = self.parse_function_ordering_terms()?;
            over_clause.order_by = Some(ordering_terms);
        }

        if self.peek_as(TokenType::RightParen).is_ok() {
            // consume the right parenthesis
            self.consume_token()?;
        }

        // frame spec
        if let Ok(Keyword::Range | Keyword::Rows | Keyword::Groups) = self.peek_as_keyword() {
            // do not consume the keyword, as it will be used in the frame spec parsing
            over_clause.frame_spec = Some(self.parse_function_over_clause_frame_spec()?);
        }

        Ok(over_clause)
    }

    fn parse_function_over_clause_frame_spec(&mut self) -> Result<FrameSpec, ParsingError> {
        let frame_type = match self.peek_as_keyword()? {
            Keyword::Range => FrameType::Range,
            Keyword::Rows => FrameType::Rows,
            Keyword::Groups => FrameType::Groups,
            _ => {
                return Err(ParsingError::UnexpectedToken(format!(
                    "Expected frame type, got: {}",
                    self.peek_token()?.token_type
                )))
            }
        };

        // consume the frame type token
        self.consume_token()?;

        #[allow(unused_assignments)]
        let mut frame_spec_type: Option<FrameSpecType> = None;

        if let Ok(Keyword::Between) = self.peek_as_keyword() {
            // consume the BETWEEN keyword
            self.consume_token()?;

            frame_spec_type = Some(self.parse_function_over_clause_frame_spec_between()?);
        } else if let Ok(Keyword::Unbounded) = self.peek_as_keyword() {
            // consume the UNBOUNDED keyword
            self.consume_token()?;

            self.consume_keyword(Keyword::Preceding)?;
            frame_spec_type = Some(FrameSpecType::UnboundedPreceding);
        } else if let Ok(Keyword::Current) = self.peek_as_keyword() {
            // consume the CURRENT keyword
            self.consume_token()?;

            self.consume_keyword(Keyword::Row)?;
            frame_spec_type = Some(FrameSpecType::CurrentRow);
        } else {
            let expression = self.parse_expression()?;
            self.consume_keyword(Keyword::Preceding)?;
            frame_spec_type = Some(FrameSpecType::Preceding(Box::new(expression)));
        }

        if frame_spec_type.is_none() {
            return Err(ParsingError::UnexpectedToken(format!(
                "Expected frame spec type, got: {}",
                self.peek_token()?.token_type
            )));
        }

        let frame_spec_type = frame_spec_type.unwrap();

        let mut exclude = None;

        if let Ok(Keyword::Exclude) = self.peek_as_keyword() {
            self.consume_token()?;

            if let Ok(Keyword::No) = self.peek_as_keyword() {
                self.consume_token()?;
                self.consume_keyword(Keyword::Others)?;
                exclude = Some(FrameSpecExclude::NoOthers);
            } else if let Ok(Keyword::Current) = self.peek_as_keyword() {
                self.consume_token()?;
                self.consume_keyword(Keyword::Row)?;
                exclude = Some(FrameSpecExclude::CurrentRow);
            } else if let Ok(Keyword::Group) = self.peek_as_keyword() {
                self.consume_token()?;
                exclude = Some(FrameSpecExclude::Group);
            } else if let Ok(Keyword::Ties) = self.peek_as_keyword() {
                self.consume_token()?;
                exclude = Some(FrameSpecExclude::Ties);
            } else {
                return Err(ParsingError::UnexpectedToken(format!(
                    "Expected exclude type, got: {}",
                    self.peek_token()?.token_type
                )));
            }
        }

        Ok(FrameSpec {
            frame_type,
            frame_spec_type,
            exclude,
        })
    }

    fn parse_function_over_clause_frame_spec_between(
        &mut self,
    ) -> Result<FrameSpecType, ParsingError> {
        let start = if let Ok(Keyword::Unbounded) = self.peek_as_keyword() {
            self.consume_token()?;
            self.consume_keyword(Keyword::Preceding)?;
            BetweenFrameSpecType::UnboundedPreceding
        } else if let Ok(Keyword::Current) = self.peek_as_keyword() {
            self.consume_token()?;
            self.consume_keyword(Keyword::Row)?;
            BetweenFrameSpecType::CurrentRow
        } else {
            let expression = self.parse_expression()?;
            match self.peek_as_keyword()? {
                Keyword::Preceding => {
                    self.consume_keyword(Keyword::Preceding)?;
                    BetweenFrameSpecType::Preceding(Box::new(expression))
                }
                Keyword::Following => {
                    self.consume_keyword(Keyword::Following)?;
                    BetweenFrameSpecType::Following(Box::new(expression))
                }
                _ => {
                    return Err(ParsingError::UnexpectedToken(format!(
                        "Expected PRECEDING or FOLLOWING keyword, got: {}",
                        self.peek_token()?.token_type
                    )));
                }
            }
        };

        self.consume_keyword(Keyword::And)?;

        let end = if let Ok(Keyword::Unbounded) = self.peek_as_keyword() {
            self.consume_token()?;
            self.consume_keyword(Keyword::Following)?;
            BetweenFrameSpecType::UnboundedFollowing
        } else if let Ok(Keyword::Current) = self.peek_as_keyword() {
            self.consume_token()?;
            self.consume_keyword(Keyword::Row)?;
            BetweenFrameSpecType::CurrentRow
        } else {
            let expression = self.parse_expression()?;
            match self.peek_as_keyword()? {
                Keyword::Preceding => {
                    self.consume_keyword(Keyword::Preceding)?;
                    BetweenFrameSpecType::Preceding(Box::new(expression))
                }
                Keyword::Following => {
                    self.consume_keyword(Keyword::Following)?;
                    BetweenFrameSpecType::Following(Box::new(expression))
                }
                _ => {
                    return Err(ParsingError::UnexpectedToken(format!(
                        "Expected PRECEDING or FOLLOWING keyword, got: {}",
                        self.peek_token()?.token_type
                    )));
                }
            }
        };
        Ok(FrameSpecType::Between(BetweenFrameSpec { start, end }))
    }
}

#[cfg(test)]
mod function_expression_tests {
    use crate::{
        expression::test_utils::*, BetweenFrameSpec, BetweenFrameSpecType, BinaryOp, FrameSpec,
        FrameSpecExclude, FrameSpecType, FrameType, FunctionArg, FunctionArgType, NullsOrdering,
        Ordering, OrderingTerm, OverClause,
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

        run_sunny_day_test(
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

        let over_clause = OverClause {
            window_name: Some("a".to_string()),
            partition_by: None,
            order_by: None,
            frame_spec: None,
        };

        run_sunny_day_test(
            "SELECT abc(1) over a;",
            &function_expression("abc", expected_arg, None, Some(over_clause)),
        );
    }

    #[test]
    fn test_expression_function_with_over_clause_partition_by() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![FunctionArgType::Expression(numeric_literal_expression("1"))],
        };

        let over_clause = OverClause {
            window_name: None,
            partition_by: Some(vec![numeric_literal_expression("1")]),
            order_by: None,
            frame_spec: None,
        };

        run_sunny_day_test(
            "SELECT abc(1) over (partition by 1);",
            &function_expression("abc", expected_arg, None, Some(over_clause)),
        );
    }

    #[test]
    fn test_expression_function_with_over_clause_order_by() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![FunctionArgType::Expression(numeric_literal_expression("1"))],
        };

        let over_clause = OverClause {
            window_name: None,
            partition_by: None,
            order_by: Some(vec![OrderingTerm {
                expression: Box::new(numeric_literal_expression("1")),
                ordering: Some(Ordering::Asc),
                nulls_ordering: Some(NullsOrdering::Last),
            }]),
            frame_spec: None,
        };

        run_sunny_day_test(
            "SELECT abc(1) over (order by 1 asc nulls last);",
            &function_expression("abc", expected_arg, None, Some(over_clause)),
        );
    }

    #[test]
    fn test_expression_function_with_over_clause_frame_spec() {
        let expected_arg = FunctionArg {
            distinct: false,
            arguments: vec![FunctionArgType::Expression(numeric_literal_expression("1"))],
        };

        let over_clause = OverClause {
            window_name: Some("a".to_string()),
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

        run_sunny_day_test(
            "SELECT abc(1) over (a groups between 1 preceding and current row exclude current row);",
            &function_expression("abc", expected_arg, None, Some(over_clause)),
        );
    }
}
