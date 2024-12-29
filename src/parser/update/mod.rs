use crate::{ConflictClause, Keyword, SetClause, TokenType, UpdateStatement};

use super::{
    delete::DeleteStatementParser,
    expression::{ExpressionParser, IdentifierParser},
    select::SelectStatementParser,
    Parser, ParsingError, SelectFromParser,
};

pub trait UpdateStatementParser {
    fn parse_update_statement(&mut self) -> Result<UpdateStatement, ParsingError>;

    fn parse_on_conflict_clause(&mut self) -> Result<ConflictClause, ParsingError>;

    fn parse_set_clauses(&mut self) -> Result<Vec<SetClause>, ParsingError>;

    fn parse_set_clause(&mut self) -> Result<SetClause, ParsingError>;
}

impl<'a> UpdateStatementParser for Parser<'a> {
    fn parse_update_statement(&mut self) -> Result<UpdateStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Update)?;

        Ok(UpdateStatement {
            with_cte: None,
            conflict_clause: self.parse_on_conflict_clause()?,
            table_name: self.parse_qualified_table_name()?,
            set_clause: self.parse_set_clauses()?,
            from_clause: self.parse_from_clause()?,
            where_clause: self.parse_where_clause()?,
            returning_clause: self.parse_returning_clause()?,
            order_by: self.parse_order_by_clause()?,
            limit: self.parse_limit_clause()?,
        })
    }

    fn parse_on_conflict_clause(&mut self) -> Result<ConflictClause, ParsingError> {
        if self.consume_as_keyword(Keyword::Or).is_ok() {
            return if self.consume_as_keyword(Keyword::Abort).is_ok() {
                Ok(ConflictClause::Abort)
            } else if self.consume_as_keyword(Keyword::Fail).is_ok() {
                Ok(ConflictClause::Fail)
            } else if self.consume_as_keyword(Keyword::Ignore).is_ok() {
                Ok(ConflictClause::Ignore)
            } else if self.consume_as_keyword(Keyword::Replace).is_ok() {
                Ok(ConflictClause::Replace)
            } else if self.consume_as_keyword(Keyword::Rollback).is_ok() {
                Ok(ConflictClause::Rollback)
            } else {
                return Err(ParsingError::UnexpectedToken(format!(
                    "Expected ON CONFLICT keyword, got: {}",
                    self.peek_token()?
                )));
            };
        }
        Ok(ConflictClause::None)
    }

    fn parse_set_clauses(&mut self) -> Result<Vec<SetClause>, ParsingError> {
        self.consume_as_keyword(Keyword::Set)?;

        let mut set_clauses = vec![];

        loop {
            set_clauses.push(self.parse_set_clause()?);
            if self.consume_as(TokenType::Comma).is_err() {
                break;
            }
        }
        Ok(set_clauses)
    }

    fn parse_set_clause(&mut self) -> Result<SetClause, ParsingError> {
        if self.peek_as(TokenType::LeftParen).is_ok() {
            self.consume_as(TokenType::LeftParen)?;
            let mut ids = vec![];

            loop {
                ids.push(self.parse_identifier()?);
                if self.consume_as(TokenType::Comma).is_err() {
                    break;
                }
            }
            self.consume_as(TokenType::RightParen)?;
            self.consume_as(TokenType::Equals)?;
            Ok(SetClause::MultipleColumnAssignment(
                ids,
                self.parse_expression()?,
            ))
        } else {
            let id = self.parse_identifier()?;
            self.consume_as(TokenType::Equals)?;
            Ok(SetClause::ColumnAssignment(id, self.parse_expression()?))
        }
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{
        expression::test_utils::numeric_literal_expression, ConflictClause, CteExpression,
        Expression, FromClause, Identifier, LimitClause, OrderingTerm, QualifiedTableName,
        ReturningClause, SetClause, Statement, UpdateStatement, WithCteStatement,
    };

    pub fn update_statement(
        table_name: QualifiedTableName,
        set_clause: Vec<SetClause>,
    ) -> Statement {
        Statement::Update(UpdateStatement {
            with_cte: None,
            conflict_clause: ConflictClause::None,
            table_name,
            set_clause,
            from_clause: None,
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: None,
        })
    }

    pub fn update_statement2() -> UpdateStatement {
        UpdateStatement {
            with_cte: None,
            conflict_clause: ConflictClause::None,
            table_name: QualifiedTableName::from(Identifier::from("table_name1")),
            set_clause: vec![SetClause::ColumnAssignment(
                Identifier::from("col1"),
                numeric_literal_expression("1"),
            )],
            from_clause: None,
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: None,
        }
    }

    pub fn update_statement_with_conflict_clause(conflict_clause: ConflictClause) -> Statement {
        Statement::Update(UpdateStatement {
            with_cte: None,
            conflict_clause,
            table_name: QualifiedTableName::from(Identifier::from("table1")),
            set_clause: vec![SetClause::ColumnAssignment(
                Identifier::from("column1"),
                numeric_literal_expression("1"),
            )],
            from_clause: None,
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: None,
        })
    }

    pub fn update_statement_with_where_clause(where_clause: Expression) -> Statement {
        Statement::Update(UpdateStatement {
            with_cte: None,
            conflict_clause: ConflictClause::None,
            table_name: QualifiedTableName::from(Identifier::from("table1")),
            set_clause: vec![SetClause::ColumnAssignment(
                Identifier::from("column1"),
                numeric_literal_expression("1"),
            )],
            from_clause: None,
            where_clause: Some(Box::new(where_clause)),
            returning_clause: vec![],
            order_by: None,
            limit: None,
        })
    }

    pub fn update_statement_with_returning_clause(
        returning_clause: Vec<ReturningClause>,
    ) -> Statement {
        Statement::Update(UpdateStatement {
            with_cte: None,
            conflict_clause: ConflictClause::None,
            table_name: QualifiedTableName::from(Identifier::from("table1")),
            set_clause: vec![SetClause::ColumnAssignment(
                Identifier::from("column1"),
                numeric_literal_expression("1"),
            )],
            from_clause: None,
            where_clause: None,
            returning_clause: returning_clause,
            order_by: None,
            limit: None,
        })
    }

    pub fn update_from(from_clause: FromClause) -> UpdateStatement {
        UpdateStatement {
            with_cte: None,
            conflict_clause: ConflictClause::None,
            table_name: QualifiedTableName::from(Identifier::from("table1")),
            set_clause: vec![SetClause::ColumnAssignment(
                Identifier::from("column1"),
                numeric_literal_expression("1"),
            )],
            from_clause: Some(from_clause),
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: None,
        }
    }

    pub fn update_statement_with_order_by_clause(order_by_clause: Vec<OrderingTerm>) -> Statement {
        Statement::Update(UpdateStatement {
            with_cte: None,
            conflict_clause: ConflictClause::None,
            table_name: QualifiedTableName::from(Identifier::from("table1")),
            set_clause: vec![SetClause::ColumnAssignment(
                Identifier::from("column1"),
                numeric_literal_expression("1"),
            )],
            from_clause: None,
            where_clause: None,
            returning_clause: vec![],
            order_by: Some(order_by_clause),
            limit: None,
        })
    }

    pub fn update_statement_with_limit_clause(limit_clause: LimitClause) -> Statement {
        Statement::Update(UpdateStatement {
            with_cte: None,
            conflict_clause: ConflictClause::None,
            table_name: QualifiedTableName::from(Identifier::from("table1")),
            set_clause: vec![SetClause::ColumnAssignment(
                Identifier::from("column1"),
                numeric_literal_expression("1"),
            )],
            from_clause: None,
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: Some(limit_clause),
        })
    }

    pub fn update_statement_with_cte_clause(
        recursive: bool,
        cte_expressions: Vec<CteExpression>,
        table_name: QualifiedTableName,
    ) -> Statement {
        Statement::Update(UpdateStatement {
            with_cte: Some(WithCteStatement {
                recursive,
                cte_expressions,
            }),
            conflict_clause: ConflictClause::None,
            table_name,
            set_clause: vec![SetClause::ColumnAssignment(
                Identifier::from("column1"),
                numeric_literal_expression("1"),
            )],
            from_clause: None,
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: None,
        })
    }
}

#[cfg(test)]
mod update_statement_tests {

    use super::test_utils::*;
    use crate::{
        expression::test_utils::{
            binary_op_expression, collate_expression, identifier_expression,
            numeric_literal_expression, string_literal_expression,
        },
        parser::test_utils::run_sunny_day_test,
        BinaryOp, ConflictClause, Identifier, IndexedType, LimitClause, NullsOrdering, Ordering,
        OrderingTerm, QualifiedTableName, ReturningClause, SetClause, Statement,
    };

    #[test]
    fn update_statement_basic() {
        let expected = update_statement2();
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1",
            Statement::Update(expected),
        );
    }

    #[test]
    fn update_statement_with_conflict_clause() {
        let conflict_options = vec![
            ConflictClause::None,
            ConflictClause::Replace,
            ConflictClause::Rollback,
            ConflictClause::Abort,
            ConflictClause::Fail,
            ConflictClause::Ignore,
        ];

        for conflict_option in conflict_options {
            let mut expected = update_statement2();
            expected.conflict_clause = conflict_option.clone();
            run_sunny_day_test(
                &format!("UPDATE {} table_name1 SET col1 = 1", conflict_option),
                Statement::Update(expected),
            );
        }
    }

    #[test]
    fn update_statement_with_table_and_schema() {
        let mut expected_statement = update_statement2();
        expected_statement.table_name = QualifiedTableName::from(Identifier::Compound(vec![
            "schema_1".to_string(),
            "table_1".to_string(),
        ]));

        run_sunny_day_test(
            "UPDATE schema_1.table_1 SET col1 = 1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_alias() {
        let mut expected_statement = update_statement2();
        expected_statement.table_name = QualifiedTableName {
            table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            alias: Some("alias_1".to_string()),
            indexed_type: None,
        };

        run_sunny_day_test(
            "UPDATE schema_1.table_1 AS alias_1 SET col1 = 1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_indexed_type() {
        let mut expected_statement = update_statement2();
        expected_statement.table_name.indexed_type =
            Some(IndexedType::Indexed("index_1".to_string()));

        run_sunny_day_test(
            "UPDATE table_name1 INDEXED BY index_1 SET col1 = 1",
            Statement::Update(expected_statement),
        );

        let mut expected_statement = update_statement2();
        expected_statement.table_name.indexed_type = Some(IndexedType::NotIndexed);

        run_sunny_day_test(
            "UPDATE table_name1 NOT INDEXED SET col1 = 1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_multiple_set_clauses() {
        let mut expected = update_statement2();
        expected.set_clause = vec![
            SetClause::ColumnAssignment(
                Identifier::from("column1"),
                numeric_literal_expression("1"),
            ),
            SetClause::MultipleColumnAssignment(
                vec![Identifier::from("column2"), Identifier::from("column3")],
                numeric_literal_expression("23"),
            ),
            SetClause::ColumnAssignment(
                Identifier::from("column4"),
                numeric_literal_expression("444"),
            ),
        ];

        run_sunny_day_test(
            "UPDATE table_name1 SET column1 = 1, (column2, column3) = 23, column4 = 444",
            Statement::Update(expected),
        );
    }

    #[test]
    fn update_statement_with_where_clause() {
        let mut expected_statement = update_statement2();
        expected_statement.where_clause = Some(Box::new(numeric_literal_expression("1")));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 WHERE 1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_where_clause_expression() {
        let mut expected_statement = update_statement2();
        expected_statement.where_clause = Some(Box::new(binary_op_expression(
            BinaryOp::Equals,
            identifier_expression(&["column1"]),
            string_literal_expression("'abc'"),
        )));
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 WHERE column1 = 'abc'",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_returning_clause() {
        let mut expected_statement = update_statement2();
        expected_statement.returning_clause = vec![ReturningClause::Wildcard];

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 RETURNING *",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_returning_clauses() {
        let mut expected_statement = update_statement2();
        expected_statement.returning_clause = vec![
            ReturningClause::Wildcard,
            ReturningClause::Expr(numeric_literal_expression("1")),
            ReturningClause::ExprWithAlias(
                identifier_expression(&["column1"]),
                "alias1".to_string(),
            ),
        ];

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 RETURNING *, 1, column1 AS alias1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_order_by_clause() {
        let mut expected_statement = update_statement2();
        expected_statement.order_by = Some(vec![
            OrderingTerm {
                expression: Box::new(identifier_expression(&["column_1"])),
                ordering: Some(Ordering::Asc),
                nulls_ordering: None,
            },
            OrderingTerm {
                expression: Box::new(collate_expression(
                    identifier_expression(&["column_2"]),
                    "binary".to_string(),
                )),
                ordering: None,
                nulls_ordering: Some(NullsOrdering::Last),
            },
        ]);
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 ORDER BY column_1 ASC, column_2 COLLATE binary NULLS LAST",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_limit_clause() {
        let mut expected_statement = update_statement2();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(numeric_literal_expression("10")),
            offset: None,
            additional_limit: None,
        });
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 LIMIT 10",
            Statement::Update(expected_statement),
        );

        let mut expected_statement = update_statement2();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(numeric_literal_expression("10")),
            offset: Some(Box::new(numeric_literal_expression("4"))),
            additional_limit: None,
        });
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 LIMIT 10 OFFSET 4",
            Statement::Update(expected_statement),
        );

        let mut expected_statement = update_statement2();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(numeric_literal_expression("10")),
            offset: None,
            additional_limit: Some(Box::new(numeric_literal_expression("40"))),
        });
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 LIMIT 10, 40",
            Statement::Update(expected_statement),
        );
    }
}

#[cfg(test)]
mod update_from_table_tests {
    use super::test_utils::update_statement2;
    use crate::parser::test_utils::*;
    use crate::{FromClause, Identifier, IndexedType, QualifiedTableName, Statement};

    #[test]
    fn update_from_table() {
        let mut expected_statement = update_statement2();
        expected_statement.from_clause = Some(FromClause::Table(QualifiedTableName::from(
            Identifier::Single("table_1".to_string()),
        )));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM table_1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_from_table_with_schema() {
        let mut expected_statement = update_statement2();
        expected_statement.from_clause = Some(FromClause::Table(QualifiedTableName::from(
            Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
        )));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM schema_1.table_1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_from_table_with_alias() {
        let mut expected_statement = update_statement2();
        expected_statement.from_clause = Some(FromClause::Table(QualifiedTableName {
            table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            alias: Some("alias".to_string()),
            indexed_type: None,
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM schema_1.table_1 AS alias",
            Statement::Update(expected_statement.clone()),
        );

        // without the as keyword
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM schema_1.table_1 alias",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_from_table_indexed() {
        let mut expected_statement = update_statement2();
        expected_statement.from_clause = Some(FromClause::Table(QualifiedTableName {
            table_id: Identifier::Single("table_1".to_string()),
            alias: None,
            indexed_type: Some(IndexedType::Indexed("index_1".to_string())),
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM table_1 INDEXED BY index_1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_from_table_not_indexed() {
        let mut expected_statement = update_statement2();
        expected_statement.from_clause = Some(FromClause::Table(QualifiedTableName {
            table_id: Identifier::Single("table_1".to_string()),
            alias: None,
            indexed_type: Some(IndexedType::NotIndexed),
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM table_1 NOT INDEXED",
            Statement::Update(expected_statement),
        );
    }
}

// #[cfg(test)]
// mod update_from_subquery_tests {
//     use super::test_utils::update_from;
//     use crate::parser::select::test_utils::select_statement_with_columns;
//     use crate::parser::test_utils::*;
//     use crate::{
//         DistinctType, Expression, FromClause, Identifier, SelectBody, SelectFromSubquery,
//         SelectItem, SelectStatement, Statement,
//     };

//     #[test]
//     fn test_update_from_subquery() {
//         let expected_statement = update_from(FromClause::Subquery(SelectFromSubquery {
//             subquery: Box::new(SelectStatement {
//                 with_cte: None,
//                 select: SelectBody::Select(select_statement_with_columns(
//                     DistinctType::None,
//                     vec![SelectItem::Expression(Expression::Identifier(
//                         Identifier::Single("col1".to_string()),
//                     ))],
//                 )),
//                 order_by: None,
//                 limit: None,
//             }),
//             alias: None,
//         }));

//         run_sunny_day_test(
//             "UPDATE table1 SET column1 = 1 FROM (SELECT col1)",
//             Statement::Update(expected_statement),
//         );
//     }

//     #[test]
//     fn test_update_from_subquery_aliased() {
//         let expected_statement = update_from(FromClause::Subquery(SelectFromSubquery {
//             subquery: Box::new(SelectStatement {
//                 with_cte: None,
//                 select: SelectBody::Select(select_statement_with_columns(
//                     DistinctType::None,
//                     vec![SelectItem::Expression(Expression::Identifier(
//                         Identifier::NameWithWildcard("t".to_string()),
//                     ))],
//                 )),
//                 order_by: None,
//                 limit: None,
//             }),
//             alias: Some("alias".to_string()),
//         }));

//         run_sunny_day_test(
//             "UPDATE table1 SET column1 = 1 FROM (SELECT t.* ) as alias",
//             Statement::Update(expected_statement.clone()),
//         );

//         // without the as keyword
//         run_sunny_day_test(
//             "UPDATE table1 SET column1 = 1 FROM (SELECT t.* ) alias",
//             Statement::Update(expected_statement.clone()),
//         );
//     }
// }

// #[cfg(test)]
// mod update_from_table_function_tests {
//     use super::test_utils::update_from;
//     use crate::expression::test_utils::{
//         binary_op_expression, identifier_expression, numeric_literal_expression,
//     };
//     use crate::parser::test_utils::*;
//     use crate::{BinaryOp, FromClause, Identifier, SelectFromFunction, Statement};

//     #[test]
//     fn test_update_from_table_function() {
//         let expected_statement = update_from(FromClause::Function(SelectFromFunction {
//             function_name: Identifier::Single("function_1".to_string()),
//             arguments: vec![numeric_literal_expression("1")],
//             alias: None,
//         }));

//         run_sunny_day_test(
//             "UPDATE table1 SET column1 = 1 FROM function_1(1)",
//             Statement::Update(expected_statement),
//         );
//     }

//     #[test]
//     fn test_update_from_table_function_with_schema() {
//         let expected_statement = update_from(FromClause::Function(SelectFromFunction {
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
//             "UPDATE table1 SET column1 = 1 FROM schema_1.function_1(1+2)",
//             Statement::Update(expected_statement),
//         );
//     }

//     #[test]
//     fn test_update_from_table_function_with_multiple_arguments() {
//         let expected_statement = update_from(FromClause::Function(SelectFromFunction {
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
//             "UPDATE table1 SET column1 = 1 FROM schema_1.function_1(1, col1, 3)",
//             Statement::Update(expected_statement),
//         );
//     }

//     #[test]
//     fn test_update_from_table_function_with_alias() {
//         let expected_statement = update_from(FromClause::Function(SelectFromFunction {
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
//             "UPDATE table1 SET column1 = 1 FROM schema_1.function_1(1, 2, 3) AS alias",
//             Statement::Update(expected_statement.clone()),
//         );

//         run_sunny_day_test(
//             "UPDATE table1 SET column1 = 1 FROM schema_1.function_1(1, 2, 3) alias",
//             Statement::Update(expected_statement.clone()),
//         );
//     }
// }

// #[cfg(test)]
// mod update_from_comma_separated_table_or_subqueries_tests {
//     use super::test_utils::update_from;
//     use crate::parser::select::test_utils::select_from;
//     use crate::parser::test_utils::*;
//     use crate::{
//         FromClause, Identifier, IndexedType, JoinClause, JoinTable, JoinType, QualifiedTableName,
//         SelectFromSubquery, Statement,
//     };

//     #[test]
//     fn test_update_from_comma_separated_table_or_subqueries() {
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

//         let expected_statement = update_from(FromClause::Froms(vec![join(
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
//             "UPDATE table1 SET column1 = 1 FROM (
//                     table_1,
//                     schema2.table2,
//                     schema3.table3 as table3_alias,
//                     indexed_table as t1 INDEXED BY index_1,
//                     not_indexed_table as t2 NOT INDEXED,
//                     (SELECT * FROM table_2) as select_alias
//                 )",
//             Statement::Update(expected_statement),
//         );
//     }
// }

// #[cfg(test)]
// mod update_from_with_join_clause_tests {
//     use super::test_utils::update_from;
//     use crate::expression::test_utils::{binary_op_expression, identifier_expression};
//     use crate::parser::select::test_utils::select_from;
//     use crate::parser::test_utils::*;
//     use crate::{
//         BinaryOp, FromClause, Identifier, IndexedType, JoinClause, JoinConstraint, JoinTable,
//         JoinType, QualifiedTableName, SelectFromSubquery, Statement,
//     };

//     #[test]
//     fn test_update_from_with_join_clause() {
//         let expected_statement = update_from(FromClause::Join(JoinClause {
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
//             "UPDATE table1 SET column1 = 1 FROM table_1 INNER JOIN table_2",
//             Statement::Update(expected_statement),
//         );
//     }

//     #[test]
//     fn test_update_from_with_join_types() {
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
//             let expected_statement = update_from(FromClause::Join(JoinClause {
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
//                 &format!(
//                     "UPDATE table1 SET column1 = 1 FROM table_1 {} JOIN table_2",
//                     &join_type
//                 ),
//                 Statement::Update(expected_statement),
//             );
//         }
//     }

//     #[test]
//     fn test_update_from_with_cross_join() {
//         let expected_statement = update_from(FromClause::Join(JoinClause {
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
//             "UPDATE table1 SET column1 = 1 FROM table_1, table_2",
//             Statement::Update(expected_statement.clone()),
//         );

//         run_sunny_day_test(
//             "UPDATE table1 SET column1 = 1 FROM table_1 CROSS JOIN table_2",
//             Statement::Update(expected_statement.clone()),
//         );
//     }

//     #[test]
//     fn test_update_from_with_join_clause_with_on_constraints() {
//         let expected_statement = update_from(FromClause::Join(JoinClause {
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
//             "UPDATE table1 SET column1 = 1 FROM table_1 INNER JOIN table_2 ON table_1.col1 = table_2.col1",
//             Statement::Update(expected_statement),
//         );
//     }

//     #[test]
//     fn test_update_from_with_join_clause_with_using_constraints() {
//         let expected_statement = update_from(FromClause::Join(JoinClause {
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
//             "UPDATE table1 SET column1 = 1 FROM table_1 INNER JOIN table_2 USING (col1)",
//             Statement::Update(expected_statement),
//         );
//     }

//     #[test]
//     fn test_update_from_with_join_clause_nested() {
//         let expected_statement = update_from(FromClause::Join(JoinClause {
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
//             "UPDATE table1 SET column1 = 1 FROM table_1
//                 INNER JOIN table_2 ON table_1.col1 = table_2.col2
//                 FULL JOIN table_3 USING (col3)",
//             Statement::Update(expected_statement),
//         );
//     }

//     #[test]
//     fn test_update_from_with_join_clause_with_nested_keeping_ordering() {
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

//         let expected_statement = update_from(FromClause::Join(JoinClause {
//             lhs_table: Box::new(FromClause::Table(QualifiedTableName {
//                 table_id: Identifier::Single("table_1".to_string()),
//                 alias: Some("t1".to_string()),
//                 indexed_type: None,
//             })),
//             join_tables: vec![subquery],
//         }));

//         run_sunny_day_test(
//             "UPDATE table1 SET column1 = 1 FROM table_1 as t1 INNER JOIN
//                 (
//                     SELECT * FROM table_2 as t2 LEFT JOIN table_3 as t3 INDEXED BY index_3
//                     ON t2.col2 = t3.col3
//                 ) as t_complex
//                 ON t1.col1 = t_complex.col1",
//             Statement::Update(expected_statement),
//         );
//     }
// }

// #[cfg(test)]
// mod update_statements_with_cte_tests {
//     use super::super::cte::test_utils::cte_expression;
//     use super::test_utils::update_statement_with_cte_clause;
//     use crate::parser::select::test_utils::select_from;
//     use crate::parser::test_utils::*;
//     use crate::{FromClause, Identifier, QualifiedTableName};

//     #[test]
//     fn test_update_with_cte() {
//         let expected_statement = update_statement_with_cte_clause(
//             true,
//             vec![cte_expression(
//                 Identifier::Single("cte_1".to_string()),
//                 vec![],
//                 None,
//                 select_from(FromClause::Table(QualifiedTableName::from(
//                     Identifier::from("cte_table"),
//                 ))),
//             )],
//             QualifiedTableName::from(Identifier::Single("cte_1".to_string())),
//         );

//         run_sunny_day_test(
//             "WITH RECURSIVE cte_1 AS (SELECT * FROM cte_table) UPDATE cte_1 SET column1 = 1",
//             expected_statement,
//         );
//     }

//     #[test]
//     fn test_update_with_multiple_ctes() {
//         let expected_statement = update_statement_with_cte_clause(
//             false,
//             vec![
//                 cte_expression(
//                     Identifier::Single("cte_1".to_string()),
//                     vec![],
//                     None,
//                     select_from(FromClause::Table(QualifiedTableName::from(
//                         Identifier::Single("cte_table1".to_string()),
//                     ))),
//                 ),
//                 cte_expression(
//                     Identifier::Single("cte_2".to_string()),
//                     vec![],
//                     None,
//                     select_from(FromClause::Table(QualifiedTableName::from(
//                         Identifier::Single("cte_table2".to_string()),
//                     ))),
//                 ),
//             ],
//             QualifiedTableName::from(Identifier::Single("cte_2".to_string())),
//         );

//         run_sunny_day_test(
//             "WITH cte_1 AS (SELECT * FROM cte_table1), cte_2 AS (SELECT * FROM cte_table2) UPDATE cte_2 SET column1 = 1",
//             expected_statement,
//         );
//     }
// }
