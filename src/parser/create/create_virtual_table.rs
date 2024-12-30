use super::CreateStatementParser;
use crate::parser::errors::ParsingError;
use crate::{CreateVirtualTableStatement, IdentifierParser, Keyword, Parser, TokenType};

pub trait CreateVirtualTableStatementParser {
    fn parse_create_virtual_table_statement(
        &mut self,
    ) -> Result<CreateVirtualTableStatement, ParsingError>;

    fn parse_module_arguments(&mut self) -> Result<Vec<String>, ParsingError>;
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

    fn parse_module_arguments(&mut self) -> Result<Vec<String>, ParsingError> {
        if self.consume_as(TokenType::LeftParen).is_ok() {
            let mut module_arguments = vec![];

            let mut arg = String::new();

            loop {
                let token = self.peek_token()?;

                if token.token_type == TokenType::RightParen {
                    break;
                }

                if token.token_type == TokenType::Comma {
                    if arg.is_empty() {
                        return Err(ParsingError::UnexpectedToken(token.to_string()));
                    }
                    module_arguments.push(arg);
                    arg = String::new();
                    self.consume_as(TokenType::Comma)?;
                } else {
                    arg = format!("{} {}", arg, token.token_type);
                    self.consume_token()?;
                }
            }

            self.consume_as(TokenType::RightParen)?;

            Ok(module_arguments)
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

    use crate::{parser::test_utils::run_sunny_day_test, Identifier, Statement};

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
            "id int PRIMARY KEY".to_string(),
            "name varchar(50)".to_string(),
            "category varchar(15)".to_string(),
            "cost int".to_string(),
        ];

        run_sunny_day_test(
            "CREATE VIRTUAL TABLE test_table USING test_module(
                id int PRIMARY KEY,
                name varchar(50),
                category varchar(15),
                cost int
            )",
            Statement::CreateVirtualTable(stmt),
        );
    }
}
