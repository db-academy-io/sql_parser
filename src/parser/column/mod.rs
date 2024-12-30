use crate::{
    expression::ExpressionParser, ColumnConstraint, ColumnConstraintType, ColumnDefinition,
    ConflictClause, DataTypeParser, Expression, FKAction, FKConstraintAction, FKDeferrableType,
    ForeignKeyClause, GeneratedColumnConstraint, GeneratedColumnType, Identifier, IdentifierParser,
    Keyword, LiteralValue, PrimaryKeyConstraint, TokenType,
};

use super::{Parser, ParsingError};

pub trait ColumnDefinitionParser {
    fn parse_column_definition(&mut self) -> Result<ColumnDefinition, ParsingError>;

    fn parse_column_constraints(&mut self) -> Result<Vec<ColumnConstraint>, ParsingError>;

    fn parse_column_constraint(
        &mut self,
        name: Option<Identifier>,
    ) -> Result<ColumnConstraint, ParsingError>;

    fn parse_on_conflict_clause(&mut self) -> Result<ConflictClause, ParsingError>;

    fn parse_foreign_key_clause(&mut self) -> Result<ForeignKeyClause, ParsingError>;

    fn parse_fk_constraint_actions(&mut self) -> Result<Vec<FKConstraintAction>, ParsingError>;

    fn parse_fk_constraint_on_or_match_action(
        &mut self,
    ) -> Result<FKConstraintAction, ParsingError>;

    fn parse_fk_constraint_action(&mut self) -> Result<FKAction, ParsingError>;

    fn parse_fk_constraint_columns_list(&mut self) -> Result<Vec<Identifier>, ParsingError>;

    fn parse_fk_deferrable_type(&mut self) -> Result<Option<FKDeferrableType>, ParsingError>;
}

impl<'a> ColumnDefinitionParser for Parser<'a> {
    fn parse_column_definition(&mut self) -> Result<ColumnDefinition, ParsingError> {
        let column_name = self.parse_identifier()?;

        // Parse the optional column type
        let column_type = self.parse_data_type().map(Some).unwrap_or_default();

        let column_constraints = self.parse_column_constraints()?;

        Ok(ColumnDefinition {
            column_name,
            column_type,
            column_constraints,
        })
    }

    fn parse_column_constraints(&mut self) -> Result<Vec<ColumnConstraint>, ParsingError> {
        let mut constraints = Vec::new();

        while self.peek_as_keyword().is_ok() {
            constraints.push(self.parse_column_constraint(None)?);
        }

        Ok(constraints)
    }

    fn parse_column_constraint(
        &mut self,
        name: Option<Identifier>,
    ) -> Result<ColumnConstraint, ParsingError> {
        let keyword = self.peek_as_keyword()?;

        match keyword {
            Keyword::Constraint => {
                self.consume_as_keyword(Keyword::Constraint)?;
                let constraint_name = self.parse_identifier()?;

                // In this case we will ignore the passed name and use the one
                //parsed from the constraint keyword
                self.parse_column_constraint(Some(constraint_name))
            }
            Keyword::Primary => {
                self.consume_as_keyword(Keyword::Primary)?;
                self.consume_as_keyword(Keyword::Key)?;

                let ordering = self.parse_ordering()?;

                // Parse the optional on conflict clause
                let conflict_clause = self.parse_on_conflict_clause()?;

                // Parse the optional auto increment keyword
                let auto_increment = self.consume_as_keyword(Keyword::Autoincrement).is_ok();

                Ok(ColumnConstraint {
                    name,
                    constraint_type: ColumnConstraintType::PrimaryKey(PrimaryKeyConstraint {
                        ordering,
                        conflict_clause,
                        auto_increment,
                    }),
                })
            }
            Keyword::Not => {
                self.consume_as_keyword(Keyword::Not)?;
                self.consume_as_keyword(Keyword::Null)?;

                let conflict_clause = self.parse_on_conflict_clause()?;

                Ok(ColumnConstraint {
                    name,
                    constraint_type: ColumnConstraintType::NotNull(conflict_clause),
                })
            }
            Keyword::Unique => {
                self.consume_as_keyword(Keyword::Unique)?;
                let conflict_clause = self.parse_on_conflict_clause()?;

                Ok(ColumnConstraint {
                    name,
                    constraint_type: ColumnConstraintType::Unique(conflict_clause),
                })
            }
            Keyword::Check => {
                self.consume_as_keyword(Keyword::Check)?;

                self.consume_as(TokenType::LeftParen)?;
                let check_expression = self.parse_expression()?;
                self.consume_as(TokenType::RightParen)?;

                Ok(ColumnConstraint {
                    name,
                    constraint_type: ColumnConstraintType::Check(check_expression),
                })
            }
            Keyword::Default => {
                self.consume_as_keyword(Keyword::Default)?;

                if let Ok(value) = self.parse_signed_number() {
                    Ok(ColumnConstraint {
                        name,
                        constraint_type: ColumnConstraintType::Default(Expression::LiteralValue(
                            LiteralValue::Number(value.to_string()),
                        )),
                    })
                } else {
                    let left_paren_open = self.consume_as(TokenType::LeftParen).is_ok();

                    let default_expression = self.parse_expression()?;
                    if left_paren_open {
                        self.consume_as(TokenType::RightParen)?;
                    }

                    Ok(ColumnConstraint {
                        name,
                        constraint_type: ColumnConstraintType::Default(default_expression),
                    })
                }
            }
            Keyword::Collate => {
                self.consume_as_keyword(Keyword::Collate)?;
                let collation_name = self.parse_identifier()?;
                Ok(ColumnConstraint {
                    name,
                    constraint_type: ColumnConstraintType::Collate(collation_name),
                })
            }
            Keyword::References => {
                let foreign_key_clause = self.parse_foreign_key_clause()?;
                Ok(ColumnConstraint {
                    name,
                    constraint_type: ColumnConstraintType::ForeignKey(foreign_key_clause),
                })
            }
            Keyword::Generated | Keyword::As => {
                if self.consume_as_keyword(Keyword::Generated).is_ok() {
                    self.consume_as_keyword(Keyword::Always)?;
                }

                self.consume_as_keyword(Keyword::As)?;

                let expression = self.parse_expression()?;

                let generated_type = if self.consume_as_keyword(Keyword::Virtual).is_ok() {
                    Some(GeneratedColumnType::Virtual)
                } else if self.consume_as_keyword(Keyword::Stored).is_ok() {
                    Some(GeneratedColumnType::Stored)
                } else {
                    None
                };

                Ok(ColumnConstraint {
                    name,
                    constraint_type: ColumnConstraintType::GeneratedAs(GeneratedColumnConstraint {
                        expression,
                        generated_type,
                    }),
                })
            }

            _ => Err(ParsingError::UnexpectedToken(format!(
                "Expected column constraint keyword, got {}",
                keyword
            ))),
        }
    }

    fn parse_on_conflict_clause(&mut self) -> Result<ConflictClause, ParsingError> {
        if self.consume_as_keyword(Keyword::On).is_ok() {
            self.consume_as_keyword(Keyword::Conflict)?;

            if self.consume_as_keyword(Keyword::Rollback).is_ok() {
                Ok(ConflictClause::Rollback)
            } else if self.consume_as_keyword(Keyword::Abort).is_ok() {
                Ok(ConflictClause::Abort)
            } else if self.consume_as_keyword(Keyword::Fail).is_ok() {
                Ok(ConflictClause::Fail)
            } else if self.consume_as_keyword(Keyword::Ignore).is_ok() {
                Ok(ConflictClause::Ignore)
            } else if self.consume_as_keyword(Keyword::Replace).is_ok() {
                Ok(ConflictClause::Replace)
            } else {
                return Err(ParsingError::UnexpectedToken(
                    "Expected conflict clause".to_string(),
                ));
            }
        } else {
            Ok(ConflictClause::None)
        }
    }

    fn parse_foreign_key_clause(&mut self) -> Result<ForeignKeyClause, ParsingError> {
        self.consume_as_keyword(Keyword::References)?;

        let foreign_table = self.parse_identifier()?;

        let columns = self.parse_fk_constraint_columns_list()?;

        let constraint_actions = self.parse_fk_constraint_actions()?;

        let deferrable = self.parse_fk_deferrable_type()?;

        Ok(ForeignKeyClause {
            table_name: foreign_table,
            columns,
            constraint_actions,
            deferrable,
        })
    }

    fn parse_fk_constraint_columns_list(&mut self) -> Result<Vec<Identifier>, ParsingError> {
        let mut columns = Vec::new();

        if self.consume_as(TokenType::LeftParen).is_ok() {
            while let Ok(id) = self.parse_identifier() {
                columns.push(id);
                if self.consume_as(TokenType::Comma).is_err() {
                    break;
                }
            }
        }

        Ok(columns)
    }

    fn parse_fk_constraint_actions(&mut self) -> Result<Vec<FKConstraintAction>, ParsingError> {
        let mut actions = Vec::new();

        while let Ok(action) = self.parse_fk_constraint_on_or_match_action() {
            actions.push(action);
        }

        Ok(actions)
    }

    fn parse_fk_constraint_on_or_match_action(
        &mut self,
    ) -> Result<FKConstraintAction, ParsingError> {
        let action = if self.consume_as_keyword(Keyword::On).is_ok() {
            if self.consume_as_keyword(Keyword::Delete).is_ok() {
                let action = self.parse_fk_constraint_action()?;
                FKConstraintAction::OnDelete(action)
            } else if self.consume_as_keyword(Keyword::Update).is_ok() {
                let action = self.parse_fk_constraint_action()?;
                FKConstraintAction::OnUpdate(action)
            } else {
                return Err(ParsingError::UnexpectedToken(format!(
                    "Expected DELETE or UPDATE keyword, got {:?}",
                    self.peek_as_keyword()
                )));
            }
        } else if self.consume_as_keyword(Keyword::Match).is_ok() {
            let match_column = self.parse_identifier()?;
            FKConstraintAction::Match(match_column)
        } else {
            return Err(ParsingError::UnexpectedToken(
                "Expected FK constraint action keyword".to_string(),
            ));
        };

        Ok(action)
    }

    fn parse_fk_constraint_action(&mut self) -> Result<FKAction, ParsingError> {
        if self.consume_as_keyword(Keyword::Set).is_ok() {
            if self.consume_as_keyword(Keyword::Null).is_ok() {
                Ok(FKAction::SetNull)
            } else if self.consume_as_keyword(Keyword::Default).is_ok() {
                Ok(FKAction::SetDefault)
            } else {
                return Err(ParsingError::UnexpectedToken(format!(
                    "Expected SET NULL or SET DEFAULT keyword, got {:?}",
                    self.peek_as_keyword()
                )));
            }
        } else if self.consume_as_keyword(Keyword::No).is_ok() {
            self.consume_as_keyword(Keyword::Action)?;
            Ok(FKAction::NoAction)
        } else if self.consume_as_keyword(Keyword::Cascade).is_ok() {
            Ok(FKAction::Cascade)
        } else if self.consume_as_keyword(Keyword::Restrict).is_ok() {
            Ok(FKAction::Restrict)
        } else {
            return Err(ParsingError::UnexpectedToken(
                "Expected FK constraint action keyword".to_string(),
            ));
        }
    }

    fn parse_fk_deferrable_type(&mut self) -> Result<Option<FKDeferrableType>, ParsingError> {
        let is_not = self.consume_as_keyword(Keyword::Not).is_ok();

        if self.consume_as_keyword(Keyword::Deferrable).is_ok() {
            let _ = self.consume_as_keyword(Keyword::Initially);

            let deferr = if self.consume_as_keyword(Keyword::Deferred).is_ok() {
                FKDeferrableType::InitiallyDeferred
            } else if self.consume_as_keyword(Keyword::Immediate).is_ok() {
                FKDeferrableType::InitiallyImmediate
            } else {
                FKDeferrableType::Deferrable
            };

            if is_not {
                Ok(Some(FKDeferrableType::Not(Box::new(deferr))))
            } else {
                Ok(Some(deferr))
            }
        } else if is_not {
            return Err(ParsingError::UnexpectedToken(
                "Expected DEFERRABLE keyword".to_string(),
            ));
        } else {
            Ok(None)
        }
    }
}
