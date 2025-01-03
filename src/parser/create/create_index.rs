use crate::parser::errors::ParsingError;
use crate::{
    parser::{insert::InsertStatementParser, select::SelectStatementParser},
    CreateIndexStatement, IdentifierParser, Keyword, Parser,
};

use super::CreateStatementParser;

pub trait CreateIndexStatementParser {
    fn parse_create_index_statement(
        &mut self,
        unique: bool,
    ) -> Result<CreateIndexStatement, ParsingError>;
}

impl CreateIndexStatementParser for Parser<'_> {
    fn parse_create_index_statement(
        &mut self,
        unique: bool,
    ) -> Result<CreateIndexStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Index)?;

        let if_not_exists = self.parse_if_not_exists()?;
        let index_name = self.parse_identifier()?;
        self.consume_as_keyword(Keyword::On)?;
        let table_name = self.parse_identifier()?;

        let columns = self.parse_indexed_columns()?;

        let where_clause = self.parse_where_clause()?;

        Ok(CreateIndexStatement {
            unique,
            if_not_exists,
            index_name,
            table_name,
            columns,
            where_clause,
        })
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{
        expression::test_utils::identifier_expr, CreateIndexStatement, Identifier, IndexedColumn,
    };

    pub fn create_index_statement() -> CreateIndexStatement {
        CreateIndexStatement {
            unique: false,
            if_not_exists: false,
            index_name: Identifier::from("index_name"),
            table_name: Identifier::from("table_name"),
            columns: vec![IndexedColumn {
                column: identifier_expr(&["column1"]),
                ordering: None,
            }],
            where_clause: None,
        }
    }
}

#[cfg(test)]
mod create_index_tests {
    use super::test_utils::create_index_statement;
    use crate::expression::test_utils::{binary_op, identifier_expr, numeric_expr};
    use crate::{
        parser::test_utils::run_sunny_day_test, BinaryOp, Identifier, IndexedColumn, Statement,
    };

    #[test]
    fn create_index_test() {
        let sql = "CREATE INDEX index_name ON table_name (column1)";
        let expected = create_index_statement();
        run_sunny_day_test(sql, Statement::CreateIndex(expected));
    }

    #[test]
    fn create_unique_index() {
        let sql = "CREATE UNIQUE INDEX index_name ON table_name (column1)";
        let mut expected = create_index_statement();
        expected.unique = true;
        run_sunny_day_test(sql, Statement::CreateIndex(expected));
    }

    #[test]
    fn create_index_with_schema() {
        let sql = "CREATE INDEX schema_name.index_name ON table_name (column1)";
        let mut expected = create_index_statement();
        expected.index_name =
            Identifier::Compound(vec!["schema_name".to_string(), "index_name".to_string()]);
        run_sunny_day_test(sql, Statement::CreateIndex(expected));
    }

    #[test]
    fn create_index_with_multiple_columns() {
        let sql = "CREATE INDEX index_name ON table_name (column1, column2, column3, column4)";
        let mut expected = create_index_statement();
        expected.columns = vec![
            IndexedColumn {
                column: identifier_expr(&["column1"]),
                ordering: None,
            },
            IndexedColumn {
                column: identifier_expr(&["column2"]),
                ordering: None,
            },
            IndexedColumn {
                column: identifier_expr(&["column3"]),
                ordering: None,
            },
            IndexedColumn {
                column: identifier_expr(&["column4"]),
                ordering: None,
            },
        ];
        run_sunny_day_test(sql, Statement::CreateIndex(expected));
    }

    #[test]
    fn create_index_with_if_not_exists() {
        let sql = "CREATE INDEX IF NOT EXISTS index_name ON table_name (column1)";
        let mut expected = create_index_statement();
        expected.if_not_exists = true;
        run_sunny_day_test(sql, Statement::CreateIndex(expected));
    }

    #[test]
    fn create_index_with_where_clause() {
        let sql = "CREATE INDEX index_name ON table_name (column1) WHERE column1 = 1";
        let mut expected = create_index_statement();
        expected.where_clause = Some(Box::new(binary_op(
            BinaryOp::Equals,
            identifier_expr(&["column1"]),
            numeric_expr("1"),
        )));
        run_sunny_day_test(sql, Statement::CreateIndex(expected));
    }
}
