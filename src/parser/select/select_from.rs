use crate::parser::select::SelectStatementParser;
use crate::parser::{ExpressionParser, IdentifierParser, ParsingError};
use crate::{
    FromClause, JoinClause, JoinConstraint, JoinTable, JoinType, Keyword, Parser,
    QualifiedTableName, SelectFromFunction, SelectFromSubquery, TokenType,
};

pub trait SelectFromParser {
    fn parse_from_clause(&mut self) -> Result<Option<FromClause>, ParsingError>;

    fn parse_from_clause_subquery(&mut self) -> Result<FromClause, ParsingError>;

    fn parse_from_join_clause(&mut self, lhs: FromClause) -> Result<FromClause, ParsingError>;

    fn parse_from_clause_join_type(&mut self) -> Result<JoinType, ParsingError>;

    fn parse_from_clause_join_constraints(
        &mut self,
    ) -> Result<Option<JoinConstraint>, ParsingError>;
}

impl<'a> SelectFromParser for Parser<'a> {
    fn parse_from_clause(&mut self) -> Result<Option<FromClause>, ParsingError> {
        if let Ok(Keyword::From) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::From)?;
            return Ok(Some(self.parse_from_clause_subquery()?));
        }
        Ok(None)
    }

    fn parse_from_clause_subquery(&mut self) -> Result<FromClause, ParsingError> {
        if self.peek_as(TokenType::LeftParen).is_ok() {
            self.consume_as(TokenType::LeftParen)?;

            if let Ok(Keyword::Select) = self.peek_as_keyword() {
                let subquery = self.parse_select_statement()?;
                // Here the right parenthesis is mandatory
                self.consume_as(TokenType::RightParen)?;
                let alias = self.parse_alias_after_as_keyword()?;
                return Ok(FromClause::Subquery(SelectFromSubquery {
                    subquery: Box::new(subquery),
                    alias,
                }));
            } else {
                let mut froms = Vec::new();
                loop {
                    let table_or_subquery = self.parse_from_clause_subquery()?;
                    froms.push(table_or_subquery);

                    if self.consume_as(TokenType::Comma).is_err() {
                        break;
                    }
                }
                // Here the right parenthesis is mandatory
                self.consume_as(TokenType::RightParen)?;
                return Ok(FromClause::Froms(froms));
            };
        }

        if let Ok(id) = self.parse_identifier() {
            if self.peek_as(TokenType::LeftParen).is_ok() {
                self.consume_as(TokenType::LeftParen)?;

                let arguments = self.parse_comma_separated_expressions()?;

                self.consume_as(TokenType::RightParen)?;
                let alias = self.parse_alias_after_as_keyword()?;
                return Ok(FromClause::Function(SelectFromFunction {
                    function_name: id,
                    arguments,
                    alias,
                }));
            } else {
                let alias = self.parse_alias_after_as_keyword()?;
                let indexed_type = self.parse_indexed_type()?;

                let lhs = FromClause::Table(QualifiedTableName {
                    table_id: id,
                    alias,
                    indexed_type,
                });

                return self.parse_from_join_clause(lhs);
            }
        }
        // TODO: improve this error message
        Err(ParsingError::UnexpectedToken(
            self.peek_token()?.to_string(),
        ))
    }

    fn parse_from_join_clause(&mut self, lhs: FromClause) -> Result<FromClause, ParsingError> {
        let mut join_tables = Vec::new();

        while let Ok(join_type) = self.parse_from_clause_join_type() {
            let rhs = self.parse_from_clause_subquery()?;
            join_tables.push(JoinTable {
                join_type,
                table: Box::new(rhs),
                constraints: self.parse_from_clause_join_constraints()?,
            });
        }

        if join_tables.is_empty() {
            return Ok(lhs);
        }

        Ok(FromClause::Join(JoinClause {
            lhs_table: Box::new(lhs),
            join_tables,
        }))
    }

    fn parse_from_clause_join_type(&mut self) -> Result<JoinType, ParsingError> {
        let natural_join = self.consume_as_keyword(Keyword::Natural).is_ok();

        if self.consume_as(TokenType::Comma).is_ok() {
            return Ok(JoinType::Cross);
        }

        let keyword = self.peek_as_keyword()?;
        match keyword {
            Keyword::Join => {
                self.consume_as_keyword(Keyword::Join)?;
                Ok(JoinType::Inner(natural_join))
            }
            Keyword::Left => {
                self.consume_as_keyword(Keyword::Left)?;
                // An optional OUTER keyword
                let _ = self.consume_as_keyword(Keyword::Outer);
                // A mandatory JOIN keyword
                self.consume_as_keyword(Keyword::Join)?;
                Ok(JoinType::Left(natural_join))
            }
            Keyword::Right => {
                self.consume_as_keyword(Keyword::Right)?;
                // An optional OUTER keyword
                let _ = self.consume_as_keyword(Keyword::Outer);
                // A mandatory JOIN keyword
                self.consume_as_keyword(Keyword::Join)?;
                Ok(JoinType::Right(natural_join))
            }
            Keyword::Full => {
                self.consume_as_keyword(Keyword::Full)?;
                // An optional OUTER keyword
                let _ = self.consume_as_keyword(Keyword::Outer);
                // A mandatory JOIN keyword
                self.consume_as_keyword(Keyword::Join)?;
                Ok(JoinType::Full(natural_join))
            }
            Keyword::Inner => {
                self.consume_as_keyword(Keyword::Inner)?;
                self.consume_as_keyword(Keyword::Join)?;
                Ok(JoinType::Inner(natural_join))
            }
            Keyword::Cross => {
                if natural_join {
                    return Err(ParsingError::UnexpectedParsingState(
                        "CROSS NATURAL JOIN".to_string(),
                    ));
                }
                self.consume_as_keyword(Keyword::Cross)?;
                self.consume_as_keyword(Keyword::Join)?;
                Ok(JoinType::Cross)
            }

            _ => Err(ParsingError::UnexpectedToken(keyword.to_string())),
        }
    }

    fn parse_from_clause_join_constraints(
        &mut self,
    ) -> Result<Option<JoinConstraint>, ParsingError> {
        if self.consume_as_keyword(Keyword::On).is_ok() {
            let lhs = self.parse_expression()?;
            Ok(Some(JoinConstraint::On(lhs)))
        } else if self.consume_as_keyword(Keyword::Using).is_ok() {
            self.consume_as(TokenType::LeftParen)?;
            let mut columns = Vec::new();
            loop {
                columns.push(self.parse_identifier()?);
                if self.consume_as(TokenType::Comma).is_err() {
                    break;
                }
            }
            self.consume_as(TokenType::RightParen)?;
            Ok(Some(JoinConstraint::Using(columns)))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{
        DistinctType, Expression, FromClause, Identifier, QualifiedTableName, Select, SelectBody,
        SelectFromSubquery, SelectItem, SelectStatement,
    };

    pub fn select_from_table(table: QualifiedTableName) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
                from: Some(FromClause::Table(table)),
                ..Default::default()
            }),
            order_by: None,
            limit: None,
        }
    }

    pub fn select_from_subquery(subquery: SelectFromSubquery) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
                from: Some(FromClause::Subquery(subquery)),
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

#[cfg(test)]
mod test_select_from_subquery {
    use super::test_utils::{select_from_subquery, select_from_table};
    use crate::parser::test_utils::*;
    use crate::{Identifier, QualifiedTableName, SelectFromSubquery, Statement};

    #[test]
    fn select_from_subquery_test() {
        let expected_statement = select_from_subquery(SelectFromSubquery {
            subquery: Box::new(select_from_table(QualifiedTableName::from(
                Identifier::Single("table_1".to_string()),
            ))),
            alias: None,
        });

        run_sunny_day_test(
            "SELECT * FROM (SELECT * FROM table_1)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_subquery_aliased() {
        let expected_statement = select_from_subquery(SelectFromSubquery {
            subquery: Box::new(select_from_table(QualifiedTableName::from(
                Identifier::Single("table_1".to_string()),
            ))),
            alias: Some("alias".to_string()),
        });

        run_sunny_day_test(
            "SELECT * FROM (SELECT * FROM table_1) as alias",
            Statement::Select(expected_statement.clone()),
        );

        // without the as keyword
        run_sunny_day_test(
            "SELECT * FROM (SELECT * FROM table_1) alias",
            Statement::Select(expected_statement.clone()),
        );
    }
}

// #[cfg(test)]
// mod test_select_from_table_function {
//     use super::test_utils::select_from;
//     use crate::expression::test_utils::{
//         binary_op_expression, identifier_expression, numeric_literal_expression,
//     };
//     use crate::parser::test_utils::*;
//     use crate::{BinaryOp, FromClause, Identifier, SelectFromFunction, Statement};

//     #[test]
//     fn test_select_from_table_function() {
//         let expected_statement = select_from(FromClause::Function(SelectFromFunction {
//             function_name: Identifier::Single("function_1".to_string()),
//             arguments: vec![numeric_literal_expression("1")],
//             alias: None,
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM function_1(1)",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_table_function_with_schema() {
//         let expected_statement = select_from(FromClause::Function(SelectFromFunction {
//             function_name: Identifier::Compound(vec![
//                 "schema_1".to_string(),
//                 "function_1".to_string(),
//             ]),
//             arguments: vec![binary_op_expression(
//                 BinaryOp::Plus,
//                 numeric_literal_expression("1"),
//                 numeric_literal_expression("2"),
//             )],
//             alias: None,
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM schema_1.function_1(1+2)",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_table_function_with_multiple_arguments() {
//         let expected_statement = select_from(FromClause::Function(SelectFromFunction {
//             function_name: Identifier::Compound(vec![
//                 "schema_1".to_string(),
//                 "function_1".to_string(),
//             ]),
//             arguments: vec![
//                 numeric_literal_expression("1"),
//                 identifier_expression(&["col1"]),
//                 numeric_literal_expression("3"),
//             ],
//             alias: None,
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM schema_1.function_1(1, col1, 3)",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_table_function_with_alias() {
//         let expected_statement = select_from(FromClause::Function(SelectFromFunction {
//             function_name: Identifier::Compound(vec![
//                 "schema_1".to_string(),
//                 "function_1".to_string(),
//             ]),
//             arguments: vec![
//                 numeric_literal_expression("1"),
//                 numeric_literal_expression("2"),
//                 numeric_literal_expression("3"),
//             ],
//             alias: Some("alias".to_string()),
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM schema_1.function_1(1, 2, 3) AS alias",
//             Statement::Select(expected_statement.clone()),
//         );

//         run_sunny_day_test(
//             "SELECT * FROM schema_1.function_1(1, 2, 3) alias",
//             Statement::Select(expected_statement.clone()),
//         );
//     }
// }

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

// #[cfg(test)]
// mod test_select_from_with_join_clause {
//     use super::test_utils::select_from;
//     use crate::expression::test_utils::{binary_op_expression, identifier_expression};
//     use crate::parser::test_utils::*;
//     use crate::{
//         BinaryOp, FromClause, Identifier, IndexedType, JoinClause, JoinConstraint, JoinTable,
//         JoinType, QualifiedTableName, SelectFromSubquery, Statement,
//     };

//     #[test]
//     fn test_select_from_with_join_clause() {
//         let expected_statement = select_from(FromClause::Join(JoinClause {
//             lhs_table: Box::new(FromClause::Table(QualifiedTableName::from(
//                 Identifier::Single("table_1".to_string()),
//             ))),
//             join_tables: vec![JoinTable {
//                 join_type: JoinType::Inner(false),
//                 table: Box::new(FromClause::Table(QualifiedTableName::from(
//                     Identifier::Single("table_2".to_string()),
//                 ))),
//                 constraints: None,
//             }],
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM table_1 INNER JOIN table_2",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_with_join_types() {
//         let join_types = vec![
//             JoinType::Inner(false),
//             JoinType::Left(false),
//             JoinType::Right(false),
//             JoinType::Full(false),
//             // NATURAL JOIN
//             JoinType::Inner(true),
//             JoinType::Left(true),
//             JoinType::Right(true),
//             JoinType::Full(true),
//             JoinType::Cross,
//         ];

//         for join_type in join_types {
//             let expected_statement = select_from(FromClause::Join(JoinClause {
//                 lhs_table: Box::new(FromClause::Table(QualifiedTableName::from(
//                     Identifier::Single("table_1".to_string()),
//                 ))),
//                 join_tables: vec![JoinTable {
//                     join_type: join_type.clone(),
//                     table: Box::new(FromClause::Table(QualifiedTableName::from(
//                         Identifier::Single("table_2".to_string()),
//                     ))),
//                     constraints: None,
//                 }],
//             }));

//             run_sunny_day_test(
//                 &format!("SELECT * FROM table_1 {} JOIN table_2", &join_type),
//                 Statement::Select(expected_statement),
//             );
//         }
//     }

//     #[test]
//     fn test_select_from_with_cross_join() {
//         let expected_statement = select_from(FromClause::Join(JoinClause {
//             lhs_table: Box::new(FromClause::Table(QualifiedTableName::from(
//                 Identifier::Single("table_1".to_string()),
//             ))),
//             join_tables: vec![JoinTable {
//                 join_type: JoinType::Cross,
//                 table: Box::new(FromClause::Table(QualifiedTableName::from(
//                     Identifier::Single("table_2".to_string()),
//                 ))),
//                 constraints: None,
//             }],
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM table_1, table_2",
//             Statement::Select(expected_statement.clone()),
//         );

//         run_sunny_day_test(
//             "SELECT * FROM table_1 CROSS JOIN table_2",
//             Statement::Select(expected_statement.clone()),
//         );
//     }

//     #[test]
//     fn test_select_from_with_join_clause_with_on_constraints() {
//         let expected_statement = select_from(FromClause::Join(JoinClause {
//             lhs_table: Box::new(FromClause::Table(QualifiedTableName::from(
//                 Identifier::Single("table_1".to_string()),
//             ))),
//             join_tables: vec![JoinTable {
//                 join_type: JoinType::Inner(false),
//                 table: Box::new(FromClause::Table(QualifiedTableName::from(
//                     Identifier::Single("table_2".to_string()),
//                 ))),
//                 constraints: Some(JoinConstraint::On(binary_op_expression(
//                     BinaryOp::Equals,
//                     identifier_expression(&["table_1", "col1"]),
//                     identifier_expression(&["table_2", "col1"]),
//                 ))),
//             }],
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM table_1 INNER JOIN table_2 ON table_1.col1 = table_2.col1",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_with_join_clause_with_using_constraints() {
//         let expected_statement = select_from(FromClause::Join(JoinClause {
//             lhs_table: Box::new(FromClause::Table(QualifiedTableName::from(
//                 Identifier::Single("table_1".to_string()),
//             ))),
//             join_tables: vec![JoinTable {
//                 join_type: JoinType::Inner(false),
//                 table: Box::new(FromClause::Table(QualifiedTableName::from(
//                     Identifier::Single("table_2".to_string()),
//                 ))),
//                 constraints: Some(JoinConstraint::Using(vec![Identifier::Single(
//                     "col1".to_string(),
//                 )])),
//             }],
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM table_1 INNER JOIN table_2 USING (col1)",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_with_join_clause_nested() {
//         let expected_statement = select_from(FromClause::Join(JoinClause {
//             lhs_table: Box::new(FromClause::Table(QualifiedTableName::from(
//                 Identifier::Single("table_1".to_string()),
//             ))),
//             join_tables: vec![
//                 JoinTable {
//                     join_type: JoinType::Inner(false),
//                     table: Box::new(FromClause::Table(QualifiedTableName::from(
//                         Identifier::Single("table_2".to_string()),
//                     ))),
//                     constraints: Some(JoinConstraint::On(binary_op_expression(
//                         BinaryOp::Equals,
//                         identifier_expression(&["table_1", "col1"]),
//                         identifier_expression(&["table_2", "col2"]),
//                     ))),
//                 },
//                 JoinTable {
//                     join_type: JoinType::Full(false),
//                     table: Box::new(FromClause::Table(QualifiedTableName::from(
//                         Identifier::Single("table_3".to_string()),
//                     ))),
//                     constraints: Some(JoinConstraint::Using(vec![Identifier::Single(
//                         "col3".to_string(),
//                     )])),
//                 },
//             ],
//         }));

//         run_sunny_day_test(
//             "SELECT *
//                 FROM table_1
//                 INNER JOIN table_2 ON table_1.col1 = table_2.col2
//                 FULL JOIN table_3 USING (col3)",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_with_join_clause_with_nested_keeping_ordering() {
//         let subquery = JoinTable {
//             join_type: JoinType::Inner(false),
//             table: Box::new(FromClause::Subquery(SelectFromSubquery {
//                 subquery: Box::new(select_from(FromClause::Join(JoinClause {
//                     lhs_table: Box::new(FromClause::Table(QualifiedTableName {
//                         table_id: Identifier::Single("table_2".to_string()),
//                         alias: Some("t2".to_string()),
//                         indexed_type: None,
//                     })),
//                     join_tables: vec![JoinTable {
//                         join_type: JoinType::Left(false),
//                         table: Box::new(FromClause::Table(QualifiedTableName {
//                             table_id: Identifier::Single("table_3".to_string()),
//                             alias: Some("t3".to_string()),
//                             indexed_type: Some(IndexedType::Indexed("index_3".to_string())),
//                         })),
//                         constraints: Some(JoinConstraint::On(binary_op_expression(
//                             BinaryOp::Equals,
//                             identifier_expression(&["t2", "col2"]),
//                             identifier_expression(&["t3", "col3"]),
//                         ))),
//                     }],
//                 }))),
//                 alias: Some("t_complex".to_string()),
//             })),
//             constraints: Some(JoinConstraint::On(binary_op_expression(
//                 BinaryOp::Equals,
//                 identifier_expression(&["t1", "col1"]),
//                 identifier_expression(&["t_complex", "col1"]),
//             ))),
//         };

//         let expected_statement = select_from(FromClause::Join(JoinClause {
//             lhs_table: Box::new(FromClause::Table(QualifiedTableName {
//                 table_id: Identifier::Single("table_1".to_string()),
//                 alias: Some("t1".to_string()),
//                 indexed_type: None,
//             })),
//             join_tables: vec![subquery],
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM table_1 as t1 INNER JOIN
//                 (
//                     SELECT * FROM table_2 as t2 LEFT JOIN table_3 as t3 INDEXED BY index_3
//                     ON t2.col2 = t3.col3
//                 ) as t_complex
//                 ON t1.col1 = t_complex.col1",
//             Statement::Select(expected_statement),
//         );
//     }
// }
