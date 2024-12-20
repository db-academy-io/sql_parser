use crate::column::ColumnDefinitionParser;
use crate::expression::IdentifierParser;
use crate::parser::errors::ParsingError;
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

        Ok(AlterTableStatement {
            table_name,
            statement_type: AlterTableStatementType::DropColumn(column_name),
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

        Ok(AlterTableStatement {
            table_name,
            statement_type: AlterTableStatementType::AddColumn(column_definition),
        })
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{
        AlterTableStatement, AlterTableStatementType, ColumnDefinition, DataType, Identifier,
    };

    pub fn alter_table_statement2() -> AlterTableStatement {
        AlterTableStatement {
            table_name: Identifier::Single("table_name".to_string()),
            statement_type: AlterTableStatementType::AddColumn(ColumnDefinition {
                column_name: Identifier::Single("column_name".to_string()),
                column_type: Some(DataType::PlainDataType("integer".to_string())),
                column_constraints: vec![],
            }),
        }
    }
}

#[cfg(test)]
mod alter_table_statement_tests {
    use crate::expression::test_utils::*;
    use crate::{
        AlterTableStatement, AlterTableStatementType, BinaryOp, ColumnConstraint,
        ColumnConstraintType, ColumnDefinition, ConflictClause, DataType, FKAction,
        FKConstraintAction, FKDeferrableType, ForeignKeyClause, Identifier, Ordering,
        PrimaryKeyConstraint, Statement,
    };

    use crate::parser::test_utils::run_sunny_day_test;

    fn alter_table_statement(table_name: Identifier, action: AlterTableStatementType) -> Statement {
        Statement::AlterTable(AlterTableStatement {
            table_name,
            statement_type: action,
        })
    }

    #[test]
    fn alter_table_statement_rename_table() {
        run_sunny_day_test(
            "ALTER TABLE table_name RENAME TO new_table_name",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::RenameTable(Identifier::Single(
                    "new_table_name".to_string(),
                )),
            ),
        );
    }

    #[test]
    fn alter_table_statement_rename_table_with_schema() {
        run_sunny_day_test(
            "ALTER TABLE schema.table_name RENAME TO new_table_name",
            alter_table_statement(
                Identifier::Compound(vec!["schema".to_string(), "table_name".to_string()]),
                AlterTableStatementType::RenameTable(Identifier::Single(
                    "new_table_name".to_string(),
                )),
            ),
        );
    }

    #[test]
    fn alter_table_statement_rename_table_with_name_in_square_brackets() {
        run_sunny_day_test(
            "ALTER TABLE [table_name] RENAME TO [new_table_name]",
            alter_table_statement(
                Identifier::Single("[table_name]".to_string()),
                AlterTableStatementType::RenameTable(Identifier::Single(
                    "[new_table_name]".to_string(),
                )),
            ),
        );
    }

    #[test]
    fn alter_table_statement_rename_column() {
        run_sunny_day_test(
            "ALTER TABLE table_name RENAME column_name TO new_column_name",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::RenameColumn(
                    Identifier::Single("column_name".to_string()),
                    Identifier::Single("new_column_name".to_string()),
                ),
            ),
        );
    }

    #[test]
    fn alter_table_statement_rename_column_with_column_keyword() {
        run_sunny_day_test(
            "ALTER TABLE schema_name.table_name RENAME COLUMN column_name TO new_column_name",
            alter_table_statement(
                Identifier::Compound(vec!["schema_name".to_string(), "table_name".to_string()]),
                AlterTableStatementType::RenameColumn(
                    Identifier::Single("column_name".to_string()),
                    Identifier::Single("new_column_name".to_string()),
                ),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD column_name integer",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition {
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::PlainDataType("integer".to_string())),
                    column_constraints: vec![],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_column_keyword() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name text",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition {
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::PlainDataType("text".to_string())),
                    column_constraints: vec![],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_without_type() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition {
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: None,
                    column_constraints: vec![],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_complex_column_type() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(10)",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition {
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::SizedDataType(
                        "varchar".to_string(),
                        "10".to_string(),
                    )),
                    column_constraints: vec![],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_complex_column_type2() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20)",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition {
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType(
                        "varchar".to_string(),
                        "-10".to_string(),
                        "20".to_string(),
                    )),
                    column_constraints: vec![],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_constraint() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT constraint_name NOT NULL",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![ColumnConstraint {
                        name: Some(Identifier::Single("constraint_name".to_string())),
                        constraint_type: ColumnConstraintType::NotNull(ConflictClause::None),
                    }],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_constraint_without_constraint_name() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) UNIQUE",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition {
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType(
                        "varchar".to_string(),
                        "-10".to_string(),
                        "20".to_string(),
                    )),
                    column_constraints: vec![ColumnConstraint {
                        name: None,
                        constraint_type: ColumnConstraintType::Unique(ConflictClause::None),
                    }],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_primary_key_constraint() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT pk PRIMARY KEY ASC ON CONFLICT ABORT AUTOINCREMENT",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![ColumnConstraint {
                        name: Some(Identifier::Single("pk".to_string())),
                        constraint_type: ColumnConstraintType::PrimaryKey(PrimaryKeyConstraint {
                            ordering: Some(Ordering::Asc),
                            conflict_clause: ConflictClause::Abort,
                            auto_increment: true,
                        }),
                    }],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_not_null_constraint() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT nn_constraint NOT NULL ON CONFLICT FAIL",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![ColumnConstraint {
                        name: Some(Identifier::Single("nn_constraint".to_string())),
                        constraint_type: ColumnConstraintType::NotNull(ConflictClause::Fail),
                    }],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_unique_constraint() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT un_constraint UNIQUE ON CONFLICT ROLLBACK",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![ColumnConstraint {
                        name: Some(Identifier::Single("un_constraint".to_string())),
                        constraint_type: ColumnConstraintType::Unique(ConflictClause::Rollback),
                    }],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_check_constraint() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT chk_constraint CHECK (column_name > 0)",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![ColumnConstraint {
                        name: Some(Identifier::Single("chk_constraint".to_string())),
                        constraint_type: ColumnConstraintType::Check(
                            binary_op_expression(BinaryOp::GreaterThan,
                                identifier_expression(&["column_name"]), 
                                numeric_literal_expression("0")
                            )
                        ),
                    }],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_default_constraint() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT default_constraint DEFAULT 'default_value'",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![ColumnConstraint {
                        name: Some(Identifier::Single("default_constraint".to_string())),
                        constraint_type: ColumnConstraintType::Default(
                            string_literal_expression("'default_value'")
                        ),
                    }],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_default_expression_constraint() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT default_constraint DEFAULT (column_name + 1)",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![ColumnConstraint {
                        name: Some(Identifier::Single("default_constraint".to_string())),
                        constraint_type: ColumnConstraintType::Default(
                            binary_op_expression(BinaryOp::Plus,
                                identifier_expression(&["column_name"]), 
                                numeric_literal_expression("1")
                            )
                        ),
                    }],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_collate_constraint() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT collate_constraint COLLATE utf8_bin",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![ColumnConstraint {
                        name: Some(Identifier::Single("collate_constraint".to_string())),
                        constraint_type: ColumnConstraintType::Collate(Identifier::Single("utf8_bin".to_string())),
                    }],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_fk_constraint() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT fk_constraint REFERENCES other_table(column_name)",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![ColumnConstraint {
                        name: Some(Identifier::Single("fk_constraint".to_string())),
                        constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                            table_name: Identifier::Single("other_table".to_string()),
                            columns: vec![Identifier::Single("column_name".to_string())],
                            constraint_actions: vec![],
                            deferrable: None,
                        }),
                    }],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_fk_constraint_without_column_names() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT fk_constraint REFERENCES other_table",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![ColumnConstraint {
                        name: Some(Identifier::Single("fk_constraint".to_string())),
                        constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                            table_name: Identifier::Single("other_table".to_string()),
                            columns: vec![],
                            constraint_actions: vec![],
                            deferrable: None,
                        }),
                    }],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_fk_constraint_with_on_clause() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT fk_constraint REFERENCES other_table ON UPDATE SET NULL",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![ColumnConstraint {
                        name: Some(Identifier::Single("fk_constraint".to_string())),
                        constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                            table_name: Identifier::Single("other_table".to_string()),
                            columns: vec![],
                            constraint_actions: vec![FKConstraintAction::OnUpdate(FKAction::SetNull)],
                            deferrable: None,
                        }),
                    }],
                }),
            ),
        );

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT fk_constraint REFERENCES other_table ON DELETE SET DEFAULT",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![
                        ColumnConstraint {
                            name: Some(Identifier::Single("fk_constraint".to_string())),
                            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                                table_name: Identifier::Single("other_table".to_string()),
                                columns: vec![],
                                constraint_actions: vec![FKConstraintAction::OnDelete(FKAction::SetDefault)],
                                deferrable: None,
                            }),
                        }
                    ],
                }),
            ),
        );

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT fk_constraint REFERENCES other_table ON UPDATE NO ACTION",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![
                        ColumnConstraint {
                            name: Some(Identifier::Single("fk_constraint".to_string())),
                            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                                table_name: Identifier::Single("other_table".to_string()),
                                columns: vec![],
                                constraint_actions: vec![FKConstraintAction::OnUpdate(FKAction::NoAction)],
                                deferrable: None,
                            }),
                        }
                    ],
                }),
            ),
        );

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT fk_constraint REFERENCES other_table ON UPDATE CASCADE",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![
                        ColumnConstraint {
                            name: Some(Identifier::Single("fk_constraint".to_string())),
                            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                                table_name: Identifier::Single("other_table".to_string()),
                                columns: vec![],
                                constraint_actions: vec![FKConstraintAction::OnUpdate(FKAction::Cascade)],
                                deferrable: None,
                            }),
                        }
                    ],
                }),
            ),
        );

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT fk_constraint REFERENCES other_table ON UPDATE RESTRICT",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![
                        ColumnConstraint {
                            name: Some(Identifier::Single("fk_constraint".to_string())),
                            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                                table_name: Identifier::Single("other_table".to_string()),
                                columns: vec![],
                                constraint_actions: vec![FKConstraintAction::OnUpdate(FKAction::Restrict)],
                                deferrable: None,
                            }),
                        }
                    ],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_fk_constraint_with_match_clause() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) CONSTRAINT fk_constraint REFERENCES other_table MATCH name",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![
                        ColumnConstraint {
                            name: Some(Identifier::Single("fk_constraint".to_string())),
                            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                                table_name: Identifier::Single("other_table".to_string()),
                                columns: vec![],
                                constraint_actions: vec![
                                    FKConstraintAction::Match(Identifier::Single("name".to_string())),
                                ],
                                deferrable: None,
                            }),
                        }
                    ],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_fk_constraint_with_multiple_on_clauses() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) REFERENCES other_table ON UPDATE SET NULL ON DELETE NO ACTION",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![
                        ColumnConstraint {
                            name: None,
                            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                                table_name: Identifier::Single("other_table".to_string()),
                                columns: vec![],
                                constraint_actions: vec![
                                    FKConstraintAction::OnUpdate(FKAction::SetNull),
                                    FKConstraintAction::OnDelete(FKAction::NoAction),
                                ],
                                deferrable: None,
                            }),
                        }
                    ],
                }),
            ),
        );
    }

    #[test]
    fn alter_table_statement_add_column_with_deferrable_clause() {
        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) REFERENCES other_table DEFERRABLE",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![
                        ColumnConstraint {
                            name: None,
                            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                                table_name: Identifier::Single("other_table".to_string()),
                                columns: vec![],
                                constraint_actions: vec![],
                                deferrable: Some(FKDeferrableType::Deferrable),
                            }),
                        }
                    ],
                }),
            ),
        );

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) REFERENCES other_table NOT DEFERRABLE",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![
                        ColumnConstraint {
                            name: None,
                            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                                table_name: Identifier::Single("other_table".to_string()),
                                columns: vec![],
                                constraint_actions: vec![],
                                deferrable: Some(FKDeferrableType::Not(Box::new(FKDeferrableType::Deferrable))),
                            }),
                        }
                    ],
                }),
            ),
        );

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) REFERENCES other_table NOT DEFERRABLE INITIALLY DEFERRED",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![
                        ColumnConstraint {
                            name: None,
                            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                                table_name: Identifier::Single("other_table".to_string()),
                                columns: vec![],
                                constraint_actions: vec![],
                                deferrable: Some(FKDeferrableType::Not(Box::new(FKDeferrableType::InitiallyDeferred))),
                            }),
                        }
                    ],
                }),
            ),
        );

        run_sunny_day_test(
            "ALTER TABLE table_name ADD COLUMN column_name varchar(-10, 20) REFERENCES other_table DEFERRABLE INITIALLY IMMEDIATE",
            alter_table_statement(
                Identifier::Single("table_name".to_string()),
                AlterTableStatementType::AddColumn(ColumnDefinition{
                    column_name: Identifier::Single("column_name".to_string()),
                    column_type: Some(DataType::BoundedDataType("varchar".to_string(), "-10".to_string(), "20".to_string())),
                    column_constraints: vec![
                        ColumnConstraint {
                            name: None,
                            constraint_type: ColumnConstraintType::ForeignKey(ForeignKeyClause{
                                table_name: Identifier::Single("other_table".to_string()),
                                columns: vec![],
                                constraint_actions: vec![],
                                deferrable: Some(FKDeferrableType::InitiallyImmediate),
                            }),
                        }
                    ],
                }),
            ),
        );
    }
}
