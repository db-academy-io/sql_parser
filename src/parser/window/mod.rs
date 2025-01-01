use crate::{
    BetweenFrameSpec, BetweenFrameSpecType, Expression, FrameSpec, FrameSpecExclude, FrameSpecType,
    FrameType, Keyword, NullsOrdering, Ordering, OrderingTerm, TokenType, WindowDefinition,
};

use super::{expression::ExpressionParser, Parser, ParsingError};

pub trait WindowDefinitionParser {
    fn parse_window_definition(&mut self) -> Result<WindowDefinition, ParsingError>;

    fn parse_ordering_terms(&mut self) -> Result<Vec<OrderingTerm>, ParsingError>;

    fn parser_partition_by_clause(&mut self) -> Result<Option<Vec<Expression>>, ParsingError>;

    fn parse_over_clause_frame_spec(&mut self) -> Result<FrameSpec, ParsingError>;

    fn parse_over_clause_frame_spec_between(&mut self) -> Result<FrameSpecType, ParsingError>;
}

impl WindowDefinitionParser for Parser<'_> {
    /// Parse a function over clause
    fn parse_window_definition(&mut self) -> Result<WindowDefinition, ParsingError> {
        self.consume_as(TokenType::LeftParen)?;

        let mut over_clause = WindowDefinition::default();
        if let Ok(base_window_name) = self.consume_as_id() {
            over_clause.base_window_name = Some(base_window_name.to_string());
        }

        over_clause.partition_by = self.parser_partition_by_clause()?;
        over_clause.order_by = self.parse_order_by_clause()?;

        // frame spec
        if let Ok(Keyword::Range | Keyword::Rows | Keyword::Groups) = self.peek_as_keyword() {
            // do not consume the keyword, as it will be used in the frame spec parsing
            over_clause.frame_spec = Some(self.parse_over_clause_frame_spec()?);
        }

        self.consume_as(TokenType::RightParen)?;

        Ok(over_clause)
    }

    fn parser_partition_by_clause(&mut self) -> Result<Option<Vec<Expression>>, ParsingError> {
        if let Ok(Keyword::Partition) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::Partition)?;

            self.consume_as_keyword(Keyword::By)?;

            let mut partition_by = vec![];
            while let Ok(expression) = self.parse_expression() {
                partition_by.push(expression);
                if self.consume_as(TokenType::Comma).is_err() {
                    break;
                }
            }
            return Ok(Some(partition_by));
        }
        Ok(None)
    }

    /// Parse a function ordering terms
    fn parse_ordering_terms(&mut self) -> Result<Vec<OrderingTerm>, ParsingError> {
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
                self.consume_as_keyword(Keyword::Asc)?;
            } else if let Ok(Keyword::Desc) = self.peek_as_keyword() {
                ordering_term.ordering = Some(Ordering::Desc);
                self.consume_as_keyword(Keyword::Desc)?;
            }

            if let Ok(Keyword::Nulls) = self.peek_as_keyword() {
                self.consume_as_keyword(Keyword::Nulls)?;

                if let Ok(Keyword::First) = self.peek_as_keyword() {
                    ordering_term.nulls_ordering = Some(NullsOrdering::First);
                    self.consume_as_keyword(Keyword::First)?;
                } else if let Ok(Keyword::Last) = self.peek_as_keyword() {
                    ordering_term.nulls_ordering = Some(NullsOrdering::Last);
                    self.consume_as_keyword(Keyword::Last)?;
                } else {
                    return Err(ParsingError::UnexpectedToken(format!(
                        "Expected FIRST or LAST keyword, got: {}",
                        self.peek_token()?.token_type
                    )));
                }
            }

            ordering_terms.push(ordering_term);

            if self.consume_as(TokenType::Comma).is_err() {
                break;
            }
        }

        Ok(ordering_terms)
    }

    fn parse_over_clause_frame_spec(&mut self) -> Result<FrameSpec, ParsingError> {
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

        let frame_spec_type: FrameSpecType = if self.consume_as_keyword(Keyword::Between).is_ok() {
            self.parse_over_clause_frame_spec_between()?
        } else if self.consume_as_keyword(Keyword::Unbounded).is_ok() {
            self.consume_as_keyword(Keyword::Preceding)?;
            FrameSpecType::UnboundedPreceding
        } else if self.consume_as_keyword(Keyword::Current).is_ok() {
            self.consume_as_keyword(Keyword::Row)?;
            FrameSpecType::CurrentRow
        } else {
            let expression = self.parse_expression()?;
            self.consume_as_keyword(Keyword::Preceding)?;
            FrameSpecType::Preceding(Box::new(expression))
        };

        let mut exclude = None;

        if self.consume_as_keyword(Keyword::Exclude).is_ok() {
            if self.consume_as_keyword(Keyword::No).is_ok() {
                self.consume_as_keyword(Keyword::Others)?;
                exclude = Some(FrameSpecExclude::NoOthers);
            } else if self.consume_as_keyword(Keyword::Current).is_ok() {
                self.consume_as_keyword(Keyword::Row)?;
                exclude = Some(FrameSpecExclude::CurrentRow);
            } else if self.consume_as_keyword(Keyword::Group).is_ok() {
                exclude = Some(FrameSpecExclude::Group);
            } else if self.consume_as_keyword(Keyword::Ties).is_ok() {
                exclude = Some(FrameSpecExclude::Ties);
            } else {
                return Err(ParsingError::UnexpectedToken(format!(
                    "Expected Exclude type, got: {}",
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

    fn parse_over_clause_frame_spec_between(&mut self) -> Result<FrameSpecType, ParsingError> {
        let start = if self.consume_as_keyword(Keyword::Unbounded).is_ok() {
            self.consume_as_keyword(Keyword::Preceding)?;
            BetweenFrameSpecType::UnboundedPreceding
        } else if self.consume_as_keyword(Keyword::Current).is_ok() {
            self.consume_as_keyword(Keyword::Row)?;
            BetweenFrameSpecType::CurrentRow
        } else {
            let expression = self.parse_expression()?;
            match self.peek_as_keyword()? {
                Keyword::Preceding => {
                    self.consume_as_keyword(Keyword::Preceding)?;
                    BetweenFrameSpecType::Preceding(Box::new(expression))
                }
                Keyword::Following => {
                    self.consume_as_keyword(Keyword::Following)?;
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

        self.consume_as_keyword(Keyword::And)?;

        let end = if self.consume_as_keyword(Keyword::Unbounded).is_ok() {
            self.consume_as_keyword(Keyword::Following)?;
            BetweenFrameSpecType::UnboundedFollowing
        } else if self.consume_as_keyword(Keyword::Current).is_ok() {
            self.consume_as_keyword(Keyword::Row)?;
            BetweenFrameSpecType::CurrentRow
        } else {
            let expression = self.parse_expression()?;

            if self.consume_as_keyword(Keyword::Preceding).is_ok() {
                BetweenFrameSpecType::Preceding(Box::new(expression))
            } else if self.consume_as_keyword(Keyword::Following).is_ok() {
                BetweenFrameSpecType::Following(Box::new(expression))
            } else {
                return Err(ParsingError::UnexpectedToken(format!(
                    "Expected PRECEDING or FOLLOWING keyword, got: {}",
                    self.peek_token()?.token_type
                )));
            }
        };
        Ok(FrameSpecType::Between(BetweenFrameSpec { start, end }))
    }
}
