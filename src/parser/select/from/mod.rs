mod function;
mod join;
mod subquery;

use crate::parser::ParsingError;
use crate::{FromClause, Keyword, Parser};

pub use function::SelectFromFunctionParser;
pub use subquery::SelectFromSubqueryParser;

pub trait SelectFromParser {
    fn parse_from_clause(&mut self) -> Result<Option<FromClause>, ParsingError>;
}

impl<'a> SelectFromParser for Parser<'a> {
    fn parse_from_clause(&mut self) -> Result<Option<FromClause>, ParsingError> {
        if let Ok(Keyword::From) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::From)?;
            return Ok(Some(self.parse_from_clause_subquery()?));
        }
        Ok(None)
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{
        DistinctType, Expression, FromClause, Identifier, JoinClause, QualifiedTableName, Select,
        SelectBody, SelectFromFunction, SelectFromSubquery, SelectItem, SelectStatement,
    };

    pub fn select_from_table(table: QualifiedTableName) -> SelectStatement {
        select_from(FromClause::Table(table))
    }

    pub fn select_from_subquery(subquery: SelectFromSubquery) -> SelectStatement {
        select_from(FromClause::Subquery(subquery))
    }

    pub fn select_from_join(join: JoinClause) -> SelectStatement {
        select_from(FromClause::Join(join))
    }

    pub fn select_from_function(function: SelectFromFunction) -> SelectStatement {
        select_from(FromClause::Function(function))
    }

    pub fn select_from(from: FromClause) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
                from: Some(from),
                ..Default::default()
            }),
            order_by: None,
            limit: None,
        }
    }
}

#[cfg(test)]
mod select_from_table {
    use super::test_utils::select_from_table;
    use crate::parser::test_utils::*;
    use crate::{Identifier, IndexedType, QualifiedTableName, Statement};

    #[test]
    fn select_from_table_test() {
        let expected_statement = select_from_table(QualifiedTableName::from(Identifier::Single(
            "table_1".to_string(),
        )));

        run_sunny_day_test(
            "SELECT * FROM table_1",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_table_with_schema() {
        let expected_statement =
            select_from_table(QualifiedTableName::from(Identifier::Compound(vec![
                "schema_1".to_string(),
                "table_1".to_string(),
            ])));

        run_sunny_day_test(
            "SELECT * FROM schema_1.table_1",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_table_with_alias() {
        let expected_statement = select_from_table(QualifiedTableName {
            table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            alias: Some("alias".to_string()),
            indexed_type: None,
        });

        run_sunny_day_test(
            "SELECT * FROM schema_1.table_1 AS alias",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_table_with_alias_without_as_keyword() {
        let expected_statement = select_from_table(QualifiedTableName {
            table_id: Identifier::Single("table_1".to_string()),
            alias: Some("alias".to_string()),
            indexed_type: None,
        });

        run_sunny_day_test(
            "SELECT * FROM table_1 alias",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_table_with_alias_indexed() {
        let expected_statement = select_from_table(QualifiedTableName {
            table_id: Identifier::Single("table_1".to_string()),
            alias: Some("alias".to_string()),
            indexed_type: Some(IndexedType::Indexed("index_1".to_string())),
        });

        run_sunny_day_test(
            "SELECT * FROM table_1 alias INDEXED BY index_1",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_table_not_indexed() {
        let expected_statement = select_from_table(QualifiedTableName {
            table_id: Identifier::Single("table_1".to_string()),
            alias: None,
            indexed_type: Some(IndexedType::NotIndexed),
        });

        run_sunny_day_test(
            "SELECT * FROM table_1 NOT INDEXED",
            Statement::Select(expected_statement),
        );
    }
}
