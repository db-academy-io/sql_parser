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

// #[cfg(test)]
// mod test_select_from_comma_separated_table_or_subqueries {
//     use super::test_utils::select_from;
//     use crate::parser::test_utils::*;
//     use crate::{
//         FromClause, Identifier, IndexedType, JoinClause, JoinTable, JoinType, QualifiedTableName,
//         SelectFromSubquery, Statement,
//     };

//     #[test]
//     fn test_select_from_comma_separated_table_or_subqueries() {
//         fn join(lhs: FromClause, rhs: FromClause) -> FromClause {
//             FromClause::Join(JoinClause {
//                 lhs_table: Box::new(lhs),
//                 join_tables: vec![JoinTable {
//                     join_type: JoinType::Cross,
//                     table: Box::new(rhs),
//                     constraints: None,
//                 }],
//             })
//         }

//         let table_1 = FromClause::Table(QualifiedTableName::from(Identifier::Single(
//             "table_1".to_string(),
//         )));
//         let schema2_table2 =
//             FromClause::Table(QualifiedTableName::from(Identifier::Compound(vec![
//                 "schema2".to_string(),
//                 "table2".to_string(),
//             ])));

//         let schema3_table3 = FromClause::Table(QualifiedTableName {
//             table_id: Identifier::Compound(vec!["schema3".to_string(), "table3".to_string()]),
//             alias: Some("table3_alias".to_string()),
//             indexed_type: None,
//         });

//         let indexed_table = FromClause::Table(QualifiedTableName {
//             table_id: Identifier::Single("indexed_table".to_string()),
//             alias: Some("t1".to_string()),
//             indexed_type: Some(IndexedType::Indexed("index_1".to_string())),
//         });

//         let not_indexed_table = FromClause::Table(QualifiedTableName {
//             table_id: Identifier::Single("not_indexed_table".to_string()),
//             alias: Some("t2".to_string()),
//             indexed_type: Some(IndexedType::NotIndexed),
//         });

//         let subquery = FromClause::Subquery(SelectFromSubquery {
//             subquery: Box::new(select_from(FromClause::Table(QualifiedTableName::from(
//                 Identifier::Single("table_2".to_string()),
//             )))),
//             alias: Some("select_alias".to_string()),
//         });

//         let expected_statement = select_from(FromClause::Froms(vec![join(
//             table_1,
//             join(
//                 schema2_table2,
//                 join(
//                     schema3_table3,
//                     join(indexed_table, join(not_indexed_table, subquery)),
//                 ),
//             ),
//         )]));

//         run_sunny_day_test(
//             "SELECT * FROM (
//                     table_1,
//                     schema2.table2,
//                     schema3.table3 as table3_alias,
//                     indexed_table as t1 INDEXED BY index_1,
//                     not_indexed_table as t2 NOT INDEXED,
//                     (SELECT * FROM table_2) as select_alias
//                 )",
//             Statement::Select(expected_statement),
//         );
//     }
// }
