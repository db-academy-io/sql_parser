use crate::column::ColumnDefinitionParser;
use crate::parser::errors::ParsingError;
use crate::IdentifierParser;
use crate::{AlterTableStatement, AlterTableStatementType, Identifier, Keyword, Parser};

pub trait AlterTableStatementParser {
    fn parse_alter_table_statement(&mut self) -> Result<AlterTableStatement, ParsingError>;

    fn parse_rename_statement(
        &mut self,
        table_name: Identifier,
    ) -> Result<AlterTableStatement, ParsingError>;

    fn parse_add_column_statement(
        &mut self,
        table_name: Identifier,
    ) -> Result<AlterTableStatement, ParsingError>;

    fn parse_drop_column_statement(
        &mut self,
        table_name: Identifier,
    ) -> Result<AlterTableStatement, ParsingError>;
}

impl<'a> AlterTableStatementParser for Parser<'a> {
    fn parse_alter_table_statement(&mut self) -> Result<AlterTableStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Alter)?;
        self.consume_as_keyword(Keyword::Table)?;

        let table_name = self.parse_identifier()?;

        let keyword = self.peek_as_keyword()?;

        match keyword {
            Keyword::Add => self.parse_add_column_statement(table_name),
            Keyword::Drop => self.parse_drop_column_statement(table_name),
            Keyword::Rename => self.parse_rename_statement(table_name),
            _ => Err(ParsingError::UnexpectedKeyword(keyword)),
        }
    }

    fn parse_rename_statement(
        &mut self,
        table_name: Identifier,
    ) -> Result<AlterTableStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Rename)?;

        if self.consume_as_keyword(Keyword::To).is_ok() {
            let new_table_name = self.parse_identifier()?;

            Ok(AlterTableStatement {
                table_name,
                statement_type: AlterTableStatementType::RenameTable(new_table_name),
            })
        } else {
            // Consume the optional "COLUMN" keyword if present
            let _ = self.consume_as_keyword(Keyword::Column);

            let column_name = self.parse_identifier()?;

            // Consume the mandatory "TO" keyword
            self.consume_as_keyword(Keyword::To)?;

            let new_column_name = self.parse_identifier()?;

            Ok(AlterTableStatement {
                table_name,
                statement_type: AlterTableStatementType::RenameColumn(column_name, new_column_name),
            })
        }
    }

    fn parse_drop_column_statement(
        &mut self,
        table_name: Identifier,
    ) -> Result<AlterTableStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Drop)?;

        // Consume the optional "COLUMN" keyword if present
        let _ = self.consume_as_keyword(Keyword::Column);

        let column_name = self.parse_identifier()?;
        let statement_type = AlterTableStatementType::DropColumn(column_name);

        Ok(AlterTableStatement {
            table_name,
            statement_type,
        })
    }

    fn parse_add_column_statement(
        &mut self,
        table_name: Identifier,
    ) -> Result<AlterTableStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Add)?;

        // Consume the optional "COLUMN" keyword if present
        let _ = self.consume_as_keyword(Keyword::Column);

        let column_definition = self.parse_column_definition()?;
        let statement_type = AlterTableStatementType::AddColumn(column_definition);

        Ok(AlterTableStatement {
            table_name,
            statement_type,
        })
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{
        AlterTableStatement, AlterTableStatementType, ColumnConstraint, ColumnDefinition, DataType,
        Identifier,
    };

    pub fn alter_table_statement() -> AlterTableStatement {
        alter_table(AlterTableStatementType::AddColumn(ColumnDefinition {
            column_name: Identifier::Single("column_name".to_string()),
            column_type: Some(DataType::PlainDataType("integer".into())),
            column_constraints: vec![],
        }))
    }

    fn alter_table(statement_type: AlterTableStatementType) -> AlterTableStatement {
        AlterTableStatement {
            table_name: Identifier::Single("table_name".to_string()),
            statement_type,
        }
    }

    pub fn rename_table_statement() -> AlterTableStatement {
        alter_table(AlterTableStatementType::RenameTable(Identifier::Single(
            "new_table_name".to_string(),
        )))
    }

    pub fn add_column_statement() -> AlterTableStatement {
        alter_table(AlterTableStatementType::AddColumn(ColumnDefinition {
            column_name: Identifier::Single("column_name".to_string()),
            column_type: Some(DataType::PlainDataType("integer".into())),
            column_constraints: vec![],
        }))
    }

    pub fn column_constraint_statement(constraint: ColumnConstraint) -> AlterTableStatement {
        alter_table(AlterTableStatementType::AddColumn(ColumnDefinition {
            column_name: Identifier::Single("column_name".to_string()),
            column_type: Some(DataType::PlainDataType("integer".into())),
            column_constraints: vec![constraint],
        }))
    }
}

#[cfg(test)]
mod rename_table_tests {
    use super::test_utils::rename_table_statement;
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::{AlterTableStatementType, Identifier, Statement};

    #[test]
    fn rename_table_test() {
        run_sunny_day_test(
            "ALTER TABLE table_name RENAME TO new_table_name",
            Statement::AlterTable(rename_table_statement()),
        );
    }

    #[test]
    fn rename_table_with_schema() {
        let mut expected_statement = rename_table_statement();
        expected_statement.table_name =
            Identifier::Compound(vec!["schema".to_string(), "table_name".to_string()]);

        run_sunny_day_test(
            "ALTER TABLE schema.table_name RENAME TO new_table_name",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn rename_table_with_name_in_square_brackets() {
        let mut expected_statement = rename_table_statement();
        expected_statement.table_name = Identifier::Single("[table_name]".to_string());
        expected_statement.statement_type = AlterTableStatementType::RenameTable(
            Identifier::Single("[new_table_name]".to_string()),
        );

        run_sunny_day_test(
            "ALTER TABLE [table_name] RENAME TO [new_table_name]",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn rename_column_test() {
        let mut expected_statement = rename_table_statement();
        expected_statement.table_name = Identifier::Single("table_name".to_string());
        expected_statement.statement_type = AlterTableStatementType::RenameColumn(
            Identifier::Single("column_name".to_string()),
            Identifier::Single("new_column_name".to_string()),
        );

        run_sunny_day_test(
            "ALTER TABLE table_name RENAME column_name TO new_column_name",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn rename_column_with_column_keyword() {
        let mut expected_statement = rename_table_statement();
        expected_statement.table_name =
            Identifier::Compound(vec!["schema_name".to_string(), "table_name".to_string()]);
        expected_statement.statement_type = AlterTableStatementType::RenameColumn(
            Identifier::Single("column_name".to_string()),
            Identifier::Single("new_column_name".to_string()),
        );

        run_sunny_day_test(
            "ALTER TABLE schema_name.table_name RENAME COLUMN column_name TO new_column_name",
            Statement::AlterTable(expected_statement),
        );
    }
}

#[cfg(test)]
mod add_column_tests {
    use super::test_utils::add_column_statement;
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::{AlterTableStatementType, ColumnDefinition, DataType, Identifier, Statement};

    #[test]
    fn add_column_test() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD column_name integer",
            Statement::AlterTable(add_column_statement()),
        );

        // An optional "COLUMN" keyword is allowed
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer",
            Statement::AlterTable(add_column_statement()),
        );
    }

    #[test]
    fn add_column_without_type() {
        let mut expected_statement = add_column_statement();
        expected_statement.statement_type = AlterTableStatementType::AddColumn(ColumnDefinition {
            column_name: Identifier::Single("column_name".to_string()),
            column_type: None,
            column_constraints: vec![],
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_sized_column_type() {
        let mut expected_statement = add_column_statement();
        expected_statement.statement_type = AlterTableStatementType::AddColumn(ColumnDefinition {
            column_name: Identifier::Single("column_name".to_string()),
            column_type: Some(DataType::SizedDataType("varchar".into(), "10".to_string())),
            column_constraints: vec![],
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(10)",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_bounded_column_type() {
        let mut expected_statement = add_column_statement();
        expected_statement.statement_type = AlterTableStatementType::AddColumn(ColumnDefinition {
            column_name: Identifier::Single("column_name".to_string()),
            column_type: Some(DataType::BoundedDataType(
                "varchar".into(),
                "-10".to_string(),
                "20".to_string(),
            )),
            column_constraints: vec![],
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20)",
            Statement::AlterTable(expected_statement),
        );
    }
}

#[cfg(test)]
mod add_column_with_constraints_tests {
    use std::fmt::Display;

    use super::test_utils::column_constraint_statement;
    use crate::expression::test_utils::*;
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::{
        BinaryOp, ColumnConstraint, ColumnConstraintType, ConflictClause, FKAction,
        FKConstraintAction, FKDeferrableType, ForeignKeyClause, Identifier, Ordering,
        PrimaryKeyConstraint, Statement,
    };

    #[test]
    fn add_column_with_constraint() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: Some(Identifier::Single("constraint_name".to_string())),
            constraint_type: ColumnConstraintType::NotNull(ConflictClause::None),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT constraint_name NOT NULL",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_constraint_without_constraint_name() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: None,
            constraint_type: ColumnConstraintType::Unique(ConflictClause::None),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer UNIQUE",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_primary_key_constraint() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: Some(Identifier::Single("pk".to_string())),
            constraint_type: ColumnConstraintType::PrimaryKey(PrimaryKeyConstraint {
                ordering: Some(Ordering::Asc),
                conflict_clause: ConflictClause::Abort,
                auto_increment: true,
            }),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT pk PRIMARY KEY ASC ON CONFLICT ABORT AUTOINCREMENT",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_not_null_constraint() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: Some(Identifier::Single("nn_constraint".to_string())),
            constraint_type: ColumnConstraintType::NotNull(ConflictClause::Fail),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT nn_constraint NOT NULL ON CONFLICT FAIL",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_unique_constraint() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: Some(Identifier::Single("un_constraint".to_string())),
            constraint_type: ColumnConstraintType::Unique(ConflictClause::Rollback),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT un_constraint UNIQUE ON CONFLICT ROLLBACK",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_check_constraint() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: Some(Identifier::Single("chk_constraint".to_string())),
            constraint_type: ColumnConstraintType::Check(binary_op(
                BinaryOp::GreaterThan,
                identifier_expr(&["column_name"]),
                numeric_expr("0"),
            )),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT chk_constraint CHECK (column_name > 0)",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_default_constraint() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: Some(Identifier::Single("default_constraint".to_string())),
            constraint_type: ColumnConstraintType::Default(string_expr("'default_value'")),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT default_constraint DEFAULT 'default_value'",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_default_expression_constraint() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: Some(Identifier::Single("default_constraint".to_string())),
            constraint_type: ColumnConstraintType::Default(binary_op(
                BinaryOp::Plus,
                identifier_expr(&["column_name"]),
                numeric_expr("1"),
            )),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT default_constraint DEFAULT (column_name + 1)",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_collate_constraint() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: Some(Identifier::Single("collate_constraint".to_string())),
            constraint_type: ColumnConstraintType::Collate(Identifier::Single(
                "utf8_bin".to_string(),
            )),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT collate_constraint COLLATE utf8_bin",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_fk_constraint() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: Some(Identifier::Single("fk_constraint".to_string())),
            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause {
                table_name: Identifier::Single("other_table".to_string()),
                columns: vec![Identifier::Single("column_name".to_string())],
                constraint_actions: vec![],
                deferrable: None,
            }),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT fk_constraint REFERENCES other_table(column_name)",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_fk_constraint_without_column_names() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: Some(Identifier::Single("fk_constraint".to_string())),
            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause {
                table_name: Identifier::Single("other_table".to_string()),
                columns: vec![],
                constraint_actions: vec![],
                deferrable: None,
            }),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT fk_constraint REFERENCES other_table",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_fk_constraint_with_on_clause() {
        impl Display for FKAction {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    FKAction::SetDefault => write!(f, "SET DEFAULT"),
                    FKAction::SetNull => write!(f, "SET NULL"),
                    FKAction::Cascade => write!(f, "CASCADE"),
                    FKAction::Restrict => write!(f, "RESTRICT"),
                    FKAction::NoAction => write!(f, "NO ACTION"),
                }
            }
        }

        let fk_actions = vec![
            FKAction::SetDefault,
            FKAction::SetNull,
            FKAction::Cascade,
            FKAction::Restrict,
            FKAction::NoAction,
        ];

        // ON DELETE actions tests
        for fk_action in fk_actions.iter() {
            let expected_statement = column_constraint_statement(ColumnConstraint {
                name: Some(Identifier::Single("fk_constraint".to_string())),
                constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause {
                    table_name: Identifier::Single("other_table".to_string()),
                    columns: vec![],
                    constraint_actions: vec![FKConstraintAction::OnDelete(fk_action.clone())],
                    deferrable: None,
                }),
            });
            run_sunny_day_test(
                &format!("ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT fk_constraint REFERENCES other_table ON DELETE {}", fk_action),
                Statement::AlterTable(expected_statement),
            );
        }

        // ON UPDATE actions tests
        for fk_action in fk_actions.iter() {
            let expected_statement = column_constraint_statement(ColumnConstraint {
                name: Some(Identifier::Single("fk_constraint".to_string())),
                constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause {
                    table_name: Identifier::Single("other_table".to_string()),
                    columns: vec![],
                    constraint_actions: vec![FKConstraintAction::OnUpdate(fk_action.clone())],
                    deferrable: None,
                }),
            });
            run_sunny_day_test(
                &format!("ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT fk_constraint REFERENCES other_table ON UPDATE {}", fk_action),
                Statement::AlterTable(expected_statement),
            );
        }
    }

    #[test]
    fn add_column_fk_constraint_with_match_clause() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: Some(Identifier::Single("fk_constraint".to_string())),
            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause {
                table_name: Identifier::Single("other_table".to_string()),
                columns: vec![],
                constraint_actions: vec![FKConstraintAction::Match(Identifier::Single(
                    "name".to_string(),
                ))],
                deferrable: None,
            }),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT fk_constraint REFERENCES other_table MATCH name",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_fk_constraint_with_multiple_on_clauses() {
        let expected_statement = column_constraint_statement(ColumnConstraint {
            name: Some(Identifier::Single("fk_constraint".to_string())),
            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause {
                table_name: Identifier::Single("other_table".to_string()),
                columns: vec![],
                constraint_actions: vec![
                    FKConstraintAction::OnUpdate(FKAction::SetNull),
                    FKConstraintAction::OnDelete(FKAction::NoAction),
                ],
                deferrable: None,
            }),
        });

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT fk_constraint REFERENCES other_table ON UPDATE SET NULL ON DELETE NO ACTION",
            Statement::AlterTable(expected_statement),
        );
    }

    #[test]
    fn add_column_with_deferrable_clause() {
        let deferrable_types = vec![
            FKDeferrableType::Deferrable,
            FKDeferrableType::InitiallyDeferred,
            FKDeferrableType::InitiallyImmediate,
            FKDeferrableType::Not(Box::new(FKDeferrableType::Deferrable)),
            FKDeferrableType::Not(Box::new(FKDeferrableType::InitiallyDeferred)),
            FKDeferrableType::Not(Box::new(FKDeferrableType::InitiallyImmediate)),
        ];

        impl Display for FKDeferrableType {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    FKDeferrableType::Deferrable => write!(f, "DEFERRABLE"),
                    FKDeferrableType::Not(deferrable_type) => write!(f, "NOT {}", deferrable_type),
                    FKDeferrableType::InitiallyDeferred => {
                        write!(f, "DEFERRABLE INITIALLY DEFERRED")
                    }
                    FKDeferrableType::InitiallyImmediate => {
                        write!(f, "DEFERRABLE INITIALLY IMMEDIATE")
                    }
                }
            }
        }

        for deferrable_type in deferrable_types.iter() {
            let expected_statement = column_constraint_statement(ColumnConstraint {
                name: Some(Identifier::Single("fk_constraint".to_string())),
                constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause {
                    table_name: Identifier::Single("other_table".to_string()),
                    columns: vec![],
                    constraint_actions: vec![],
                    deferrable: Some(deferrable_type.clone()),
                }),
            });
            run_sunny_day_test(
                &format!("ALTER TABLE table_name ADD COLUMN column_name integer CONSTRAINT fk_constraint REFERENCES other_table {}", deferrable_type),
                Statement::AlterTable(expected_statement),
            );
        }
    }
}
