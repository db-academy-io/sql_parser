use crate::{
    expression::IdentifierParser,
    parser::{insert::InsertStatementParser, select::SelectStatementParser},
    CreateViewStatement, Keyword, Parser, ParsingError,
};

use super::CreateStatementParser;

pub trait CreateViewStatementParser {
    fn parse_create_view_statement(
        &mut self,
        is_temporary: bool,
    ) -> Result<CreateViewStatement, ParsingError>;
}

impl<'a> CreateViewStatementParser for Parser<'a> {
    fn parse_create_view_statement(
        &mut self,
        is_temporary: bool,
    ) -> Result<CreateViewStatement, ParsingError> {
        self.consume_as_keyword(Keyword::View)?;

        let if_not_exists = self.parse_if_not_exists()?;

        let view_name = self.parse_identifier()?;
        let columns = self.parse_columns_names()?;

        self.consume_as_keyword(Keyword::As)?;
        let select_statement = SelectStatementParser::parse_select_statement(self)?;

        Ok(CreateViewStatement {
            temporary: is_temporary,
            if_not_exists,
            view_name,
            columns,
            select_statement,
        })
    }
}

#[cfg(test)]
mod test_utils {
    use crate::{
        parser::select::test_utils::select_from, CreateViewStatement, FromClause, Identifier,
        QualifiedTableName,
    };

    pub fn create_view_statement() -> CreateViewStatement {
        CreateViewStatement {
            temporary: false,
            if_not_exists: false,
            view_name: Identifier::from("view_name"),
            columns: vec![],
            select_statement: select_from(FromClause::Table(QualifiedTableName::from(
                Identifier::from("table_name"),
            ))),
        }
    }
}

#[cfg(test)]
mod test_create_view_statement_parser {
    use crate::{parser::test_utils::run_sunny_day_test, Identifier, Statement};

    use super::test_utils::create_view_statement;

    #[test]
    fn test_parse_create_view_statement() {
        let expected_stmt = create_view_statement();
        run_sunny_day_test(
            "CREATE VIEW view_name AS SELECT * FROM table_name",
            Statement::CreateView(expected_stmt),
        );
    }

    #[test]
    fn test_parse_create_temporary_view_statement() {
        let mut expected_stmt = create_view_statement();
        expected_stmt.temporary = true;
        run_sunny_day_test(
            "CREATE TEMP VIEW view_name AS SELECT * FROM table_name",
            Statement::CreateView(expected_stmt.clone()),
        );
        run_sunny_day_test(
            "CREATE TEMPORARY VIEW view_name AS SELECT * FROM table_name",
            Statement::CreateView(expected_stmt.clone()),
        );
    }

    #[test]
    fn test_parse_create_view_statement_with_if_not_exists() {
        let mut expected_stmt = create_view_statement();
        expected_stmt.if_not_exists = true;
        run_sunny_day_test(
            "CREATE VIEW IF NOT EXISTS view_name AS SELECT * FROM table_name",
            Statement::CreateView(expected_stmt.clone()),
        );
    }

    #[test]
    fn test_parse_create_view_statement_with_schema_name() {
        let mut expected_stmt = create_view_statement();
        expected_stmt.view_name =
            Identifier::Compound(vec!["schema_name".to_string(), "view_name".to_string()]);
        run_sunny_day_test(
            "CREATE VIEW schema_name.view_name AS SELECT * FROM table_name",
            Statement::CreateView(expected_stmt.clone()),
        );
    }

    #[test]
    fn test_parse_create_view_statement_with_columns() {
        let mut expected_stmt = create_view_statement();
        expected_stmt.columns = vec![Identifier::from("column1"), Identifier::from("column2")];
        run_sunny_day_test(
            "CREATE VIEW view_name (column1, column2) AS SELECT * FROM table_name",
            Statement::CreateView(expected_stmt.clone()),
        );
    }
}
