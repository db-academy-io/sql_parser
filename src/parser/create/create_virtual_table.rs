use super::create_table::CreateTableStatementParser;
use super::CreateStatementParser;
use crate::parser::errors::ParsingError;
use crate::{
    ColumnDefinition, CreateVirtualTableStatement, IdentifierParser, Keyword, Parser, TokenType,
};

pub trait CreateVirtualTableStatementParser {
    fn parse_create_virtual_table_statement(
        &mut self,
    ) -> Result<CreateVirtualTableStatement, ParsingError>;

    fn parse_module_arguments(&mut self) -> Result<Vec<ColumnDefinition>, ParsingError>;
}

impl<'a> CreateVirtualTableStatementParser for Parser<'a> {
    fn parse_create_virtual_table_statement(
        &mut self,
    ) -> Result<CreateVirtualTableStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Virtual)?;
        self.consume_as_keyword(Keyword::Table)?;

        let if_not_exists = self.parse_if_not_exists()?;
        let table_name = self.parse_identifier()?;

        self.consume_as_keyword(Keyword::Using)?;

        let module_name = self.parse_identifier()?;

        let module_arguments = self.parse_module_arguments()?;

        Ok(CreateVirtualTableStatement {
            if_not_exists,
            table_name,
            module_name,
            module_arguments,
        })
    }

    fn parse_module_arguments(&mut self) -> Result<Vec<ColumnDefinition>, ParsingError> {
        if self.consume_as(TokenType::LeftParen).is_ok() {
            let args = self.parse_column_definitions()?;

            self.consume_as(TokenType::RightParen)?;

            Ok(args)
        } else {
            Ok(vec![])
        }
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{CreateVirtualTableStatement, Identifier};

    pub fn create_virtual_table_statement() -> CreateVirtualTableStatement {
        CreateVirtualTableStatement {
            if_not_exists: false,
            table_name: Identifier::from("test_table"),
            module_name: Identifier::from("test_module"),
            module_arguments: vec![],
        }
    }
}

#[cfg(test)]
mod create_virtual_table_tests {
    use test_utils::create_virtual_table_statement;

    use crate::{
        parser::test_utils::run_sunny_day_test, ColumnConstraint, ColumnConstraintType,
        ConflictClause, DataType, Identifier, PrimaryKeyConstraint, Statement,
    };

    use super::*;

    #[test]
    fn create_virtual_table_test() {
        run_sunny_day_test(
            "CREATE VIRTUAL TABLE test_table USING test_module",
            Statement::CreateVirtualTable(create_virtual_table_statement()),
        );
    }

    #[test]
    fn create_virtual_table_with_if_not_exists() {
        let mut stmt = create_virtual_table_statement();
        stmt.if_not_exists = true;

        run_sunny_day_test(
            "CREATE VIRTUAL TABLE IF NOT EXISTS test_table USING test_module",
            Statement::CreateVirtualTable(stmt),
        );
    }

    #[test]
    fn create_virtual_table_with_schema() {
        let mut stmt = create_virtual_table_statement();
        stmt.table_name =
            Identifier::Compound(vec!["test_schema".to_string(), "test_table".to_string()]);

        run_sunny_day_test(
            "CREATE VIRTUAL TABLE test_schema.test_table USING test_module",
            Statement::CreateVirtualTable(stmt),
        );
    }

    #[test]
    fn create_virtual_table_with_module_arguments() {
        let mut stmt = create_virtual_table_statement();
        stmt.module_arguments = vec![
            ColumnDefinition {
                column_name: Identifier::from("id"),
                column_type: Some(DataType::PlainDataType("int".into())),
                column_constraints: vec![ColumnConstraint {
                    name: None,
                    constraint_type: ColumnConstraintType::PrimaryKey(PrimaryKeyConstraint {
                        auto_increment: false,
                        conflict_clause: ConflictClause::None,
                        ordering: None,
                    }),
                }],
            },
            ColumnDefinition {
                column_name: Identifier::from("name"),
                column_type: Some(DataType::SizedDataType("varchar".into(), "50".into())),
                column_constraints: vec![],
            },
            ColumnDefinition {
                column_name: Identifier::from("category"),
                column_type: Some(DataType::SizedDataType("varchar".into(), "15".into())),
                column_constraints: vec![],
            },
            ColumnDefinition {
                column_name: Identifier::from("cost"),
                column_type: Some(DataType::PlainDataType("int".into())),
                column_constraints: vec![],
            },
        ];

        run_sunny_day_test(
            "CREATE VIRTUAL TABLE test_table 
            USING test_module(
                id int PRIMARY KEY,
                name varchar(50),
                category varchar(15),
                cost int
            )",
            Statement::CreateVirtualTable(stmt),
        );
    }
}
