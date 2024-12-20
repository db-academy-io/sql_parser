use crate::parser::{
    column_definition::ColumnDefinitionParser, insert::InsertStatementParser,
    select::SelectStatementParser,
};
use crate::{
    expression::{ExpressionParser, IdentifierParser},
    ColumnDefinition, CreateTableColumnDef, CreateTableOption, CreateTableStatement, Keyword,
    Parser, ParsingError, TableConstraint, TableConstraintType, TableOption, TokenType,
};

use super::CreateStatementParser;

pub trait CreateTableStatementParser {
    fn parse_create_table_statement(
        &mut self,
        is_temporary: bool,
    ) -> Result<CreateTableStatement, ParsingError>;

    fn parse_create_table_option(&mut self) -> Result<CreateTableOption, ParsingError>;

    fn parse_column_definitions(&mut self) -> Result<Vec<ColumnDefinition>, ParsingError>;

    fn parse_table_constraints(&mut self) -> Result<Vec<TableConstraint>, ParsingError>;

    fn parse_table_constraint(&mut self) -> Result<TableConstraint, ParsingError>;

    fn parse_table_constraint_type(&mut self) -> Result<TableConstraintType, ParsingError>;

    fn parse_table_options(&mut self) -> Result<Vec<TableOption>, ParsingError>;
}

impl<'a> CreateTableStatementParser for Parser<'a> {
    fn parse_create_table_statement(
        &mut self,
        is_temporary: bool,
    ) -> Result<CreateTableStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Table)?;
        let if_not_exists = self.parse_if_not_exists()?;
        let table_name = self.parse_identifier()?;

        dbg!(&self.peek_token()?);
        let create_table_option = self.parse_create_table_option()?;

        Ok(CreateTableStatement {
            temporary: is_temporary,
            if_not_exists,
            table_name,
            create_table_option,
        })
    }

    fn parse_create_table_option(&mut self) -> Result<CreateTableOption, ParsingError> {
        if self.consume_as_keyword(Keyword::As).is_ok() {
            let select_statement = self.parse_select_statement()?;
            return Ok(CreateTableOption::SelectStatement(select_statement));
        }
        dbg!(&self.peek_token()?);
        self.consume_as(TokenType::LeftParen)?;

        let column_definitions = self.parse_column_definitions()?;
        dbg!(&self.peek_token()?);

        let table_constraints = if self.peek_as(TokenType::RightParen).is_ok() {
            vec![]
        } else {
            self.parse_table_constraints()?
        };

        self.consume_as(TokenType::RightParen)?;

        let table_options = self.parse_table_options()?;

        Ok(CreateTableOption::ColumnDefinitions(CreateTableColumnDef {
            columns: column_definitions,
            table_constraints,
            table_options,
        }))
    }

    fn parse_column_definitions(&mut self) -> Result<Vec<ColumnDefinition>, ParsingError> {
        let mut column_definitions = vec![];
        dbg!(&self.peek_token()?);

        // Column definitions starts with an identifier, otherwise it's a table constraint
        while self.peek_as_id_or_star().is_ok() {
            column_definitions.push(self.parse_column_definition()?);
            dbg!(&self.peek_token()?);
            if self.consume_as(TokenType::Comma).is_err() {
                break;
            }
        }
        dbg!(&self.peek_token()?);
        Ok(column_definitions)
    }

    fn parse_table_constraints(&mut self) -> Result<Vec<TableConstraint>, ParsingError> {
        let mut table_constraints = vec![];

        loop {
            let table_constraint = self.parse_table_constraint()?;
            table_constraints.push(table_constraint);

            if self.consume_as(TokenType::Comma).is_err() {
                break;
            }
        }

        Ok(table_constraints)
    }

    fn parse_table_constraint(&mut self) -> Result<TableConstraint, ParsingError> {
        let constraint_name = {
            if self.consume_as_keyword(Keyword::Constraint).is_ok() {
                Some(self.parse_identifier()?)
            } else {
                None
            }
        };
        dbg!(&self.peek_token()?);
        let constraint_type = self.parse_table_constraint_type()?;
        dbg!(&self.peek_token()?);
        Ok(TableConstraint {
            constraint_name,
            constraint_type,
        })
    }

    fn parse_table_constraint_type(&mut self) -> Result<TableConstraintType, ParsingError> {
        let keyword = self.peek_as_keyword()?;
        dbg!(&self.peek_token()?);
        match keyword {
            Keyword::Primary => {
                self.consume_as_keyword(Keyword::Primary)?;
                self.consume_as_keyword(Keyword::Key)?;
                let columns = self.parse_indexed_columns()?;
                let conflict_clause = self.parse_on_conflict_clause()?;
                Ok(TableConstraintType::PrimaryKey(columns, conflict_clause))
            }
            Keyword::Unique => {
                self.consume_as_keyword(Keyword::Unique)?;
                let columns = self.parse_indexed_columns()?;
                let conflict_clause = self.parse_on_conflict_clause()?;
                Ok(TableConstraintType::Unique(columns, conflict_clause))
            }
            Keyword::Check => {
                self.consume_as_keyword(Keyword::Check)?;
                self.consume_as(TokenType::LeftParen)?;
                let expression = self.parse_expression()?;
                self.consume_as(TokenType::RightParen)?;
                Ok(TableConstraintType::Check(expression))
            }
            Keyword::Foreign => {
                self.consume_as_keyword(Keyword::Foreign)?;
                self.consume_as_keyword(Keyword::Key)?;
                let columns = self.parse_columns_names()?;
                let foreign_key_clause = self.parse_foreign_key_clause()?;
                Ok(TableConstraintType::ForeignKey(columns, foreign_key_clause))
            }
            _ => Err(ParsingError::UnexpectedKeyword(keyword)),
        }
    }

    fn parse_table_options(&mut self) -> Result<Vec<TableOption>, ParsingError> {
        let mut table_options = vec![];

        loop {
            if self.consume_as_keyword(Keyword::Without).is_ok() {
                self.consume_as_keyword(Keyword::RowId)?;
                table_options.push(TableOption::WithoutRowId);
            } else if self.consume_as_keyword(Keyword::Strict).is_ok() {
                table_options.push(TableOption::Strict);
            } else if self.consume_as(TokenType::Comma).is_ok() {
                continue;
            } else {
                break;
            }
        }

        Ok(table_options)
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{
        parser::select::test_utils::select_from, CreateTableOption, CreateTableStatement,
        FromClause, Identifier, QualifiedTableName,
    };

    pub fn create_table_statement() -> CreateTableStatement {
        CreateTableStatement {
            temporary: false,
            if_not_exists: false,
            table_name: Identifier::from("table_name"),
            create_table_option: CreateTableOption::SelectStatement(select_from(
                FromClause::Table(QualifiedTableName::from(Identifier::from("table_name"))),
            )),
        }
    }
}

#[cfg(test)]
mod test_create_table_statement_parser {
    use crate::{
        expression::test_utils::identifier_expression, parser::test_utils::run_sunny_day_test,
        ColumnConstraint, ColumnConstraintType, ColumnDefinition, ConflictClause,
        CreateTableColumnDef, CreateTableOption, DataType, Identifier, IndexedColumn, Ordering,
        Statement, TableConstraint, TableConstraintType, TableOption,
    };

    use super::test_utils::create_table_statement;

    #[test]
    fn test_parse_create_table_statement() {
        let expected_stmt = create_table_statement();
        run_sunny_day_test(
            "CREATE TABLE table_name AS SELECT * FROM table_name",
            Statement::CreateTable(expected_stmt),
        );
    }

    #[test]
    fn test_parse_create_temporary_table_statement() {
        let mut expected_stmt = create_table_statement();
        expected_stmt.temporary = true;

        run_sunny_day_test(
            "CREATE TEMP TABLE table_name AS SELECT * FROM table_name",
            Statement::CreateTable(expected_stmt.clone()),
        );

        run_sunny_day_test(
            "CREATE TEMPORARY TABLE table_name AS SELECT * FROM table_name",
            Statement::CreateTable(expected_stmt.clone()),
        );
    }

    #[test]
    fn test_parse_create_table_statement_with_if_not_exists() {
        let mut expected_stmt = create_table_statement();
        expected_stmt.if_not_exists = true;

        run_sunny_day_test(
            "CREATE TABLE IF NOT EXISTS table_name AS SELECT * FROM table_name",
            Statement::CreateTable(expected_stmt.clone()),
        );
    }

    #[test]
    fn test_parse_create_table_statement_with_schema() {
        let mut expected_stmt = create_table_statement();
        expected_stmt.table_name =
            Identifier::Compound(vec!["schema_name".to_string(), "table_name".to_string()]);
        run_sunny_day_test(
            "CREATE TABLE schema_name.table_name AS SELECT * FROM table_name",
            Statement::CreateTable(expected_stmt.clone()),
        );
    }

    #[test]
    fn test_parse_create_table_statement_with_column_definitions() {
        let mut expected_stmt = create_table_statement();
        expected_stmt.create_table_option =
            CreateTableOption::ColumnDefinitions(CreateTableColumnDef {
                columns: vec![
                    ColumnDefinition {
                        column_name: Identifier::from("column_name1"),
                        column_type: Some(DataType::PlainDataType("INTEGER".to_string())),
                        column_constraints: vec![],
                    },
                    ColumnDefinition {
                        column_name: Identifier::from("column_name2"),
                        column_type: Some(DataType::SizedDataType(
                            "VARCHAR".to_string(),
                            "10".to_string(),
                        )),
                        column_constraints: vec![ColumnConstraint {
                            name: Some(Identifier::from("not_null_constraint")),
                            constraint_type: ColumnConstraintType::NotNull(ConflictClause::None),
                        }],
                    },
                ],
                table_constraints: vec![],
                table_options: vec![],
            });

        run_sunny_day_test(
            "CREATE TABLE table_name (column_name1 INTEGER, column_name2 VARCHAR(10) CONSTRAINT not_null_constraint NOT NULL)",
            Statement::CreateTable(expected_stmt.clone()),
        );
    }

    #[test]
    fn test_parse_create_table_statement_with_table_constraints() {
        let mut expected_stmt = create_table_statement();
        expected_stmt.create_table_option =
            CreateTableOption::ColumnDefinitions(CreateTableColumnDef {
                columns: vec![ColumnDefinition {
                    column_name: Identifier::from("column_name1"),
                    column_type: Some(DataType::PlainDataType("INTEGER".to_string())),
                    column_constraints: vec![],
                }],
                table_constraints: vec![TableConstraint {
                    constraint_name: Some(Identifier::from("pk_column_name1")),
                    constraint_type: TableConstraintType::PrimaryKey(
                        vec![IndexedColumn {
                            column: identifier_expression(&["column_name1"]),
                            ordering: Some(Ordering::Asc),
                        }],
                        ConflictClause::None,
                    ),
                }],
                table_options: vec![],
            });

        run_sunny_day_test(
            "CREATE TABLE table_name (column_name1 INTEGER, CONSTRAINT pk_column_name1 PRIMARY KEY (column_name1 ASC))",
            Statement::CreateTable(expected_stmt.clone()),
        );
    }

    #[test]
    fn test_parse_create_table_statement_with_table_options() {
        let table_options = vec![TableOption::WithoutRowId, TableOption::Strict];

        for option in table_options {
            let mut expected_stmt = create_table_statement();
            expected_stmt.create_table_option =
                CreateTableOption::ColumnDefinitions(CreateTableColumnDef {
                    columns: vec![ColumnDefinition {
                        column_name: Identifier::from("column_name1"),
                        column_type: Some(DataType::PlainDataType("INTEGER".to_string())),
                        column_constraints: vec![],
                    }],
                    table_constraints: vec![],
                    table_options: vec![option.clone()],
                });

            run_sunny_day_test(
                &format!(
                    "CREATE TABLE table_name (column_name1 INTEGER) {}",
                    option.to_string()
                ),
                Statement::CreateTable(expected_stmt.clone()),
            );
        }
    }
}
