use crate::{ConflictClause, Keyword, SetClause, TokenType, UpdateStatement};

use super::{
    delete::DeleteStatementParser, select::SelectStatementParser, ExpressionParser,
    IdentifierParser, Parser, ParsingError, SelectFromParser,
};

pub trait UpdateStatementParser {
    fn parse_update_statement(&mut self) -> Result<UpdateStatement, ParsingError>;

    fn parse_on_conflict_clause(&mut self) -> Result<ConflictClause, ParsingError>;

    fn parse_set_clauses(&mut self) -> Result<Vec<SetClause>, ParsingError>;

    fn parse_set_clause(&mut self) -> Result<SetClause, ParsingError>;
}

impl UpdateStatementParser for Parser<'_> {
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
        expression::test_utils::numeric_expr, ConflictClause, Identifier, QualifiedTableName,
        SetClause, UpdateStatement,
    };

    pub fn update_statement() -> UpdateStatement {
        UpdateStatement {
            with_cte: None,
            conflict_clause: ConflictClause::None,
            table_name: QualifiedTableName::from(Identifier::from("table_name1")),
            set_clause: vec![SetClause::ColumnAssignment(
                Identifier::from("col1"),
                numeric_expr("1"),
            )],
            from_clause: None,
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: None,
        }
    }
}

#[cfg(test)]
mod update_statement_tests {

    use super::test_utils::*;
    use crate::{
        expression::test_utils::{
            binary_op, collate_expr, identifier_expr, numeric_expr, string_expr,
        },
        parser::test_utils::run_sunny_day_test,
        BinaryOp, ConflictClause, Identifier, IndexedType, LimitClause, NullsOrdering, Ordering,
        OrderingTerm, QualifiedTableName, ReturningClause, SetClause, Statement,
    };

    #[test]
    fn update_statement_basic() {
        let expected = update_statement();
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
            let mut expected = update_statement();
            expected.conflict_clause = conflict_option.clone();
            run_sunny_day_test(
                &format!("UPDATE {} table_name1 SET col1 = 1", conflict_option),
                Statement::Update(expected),
            );
        }
    }

    #[test]
    fn update_statement_with_table_and_schema() {
        let mut expected_statement = update_statement();
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
        let mut expected_statement = update_statement();
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
        let mut expected_statement = update_statement();
        expected_statement.table_name.indexed_type =
            Some(IndexedType::Indexed("index_1".to_string()));

        run_sunny_day_test(
            "UPDATE table_name1 INDEXED BY index_1 SET col1 = 1",
            Statement::Update(expected_statement),
        );

        let mut expected_statement = update_statement();
        expected_statement.table_name.indexed_type = Some(IndexedType::NotIndexed);

        run_sunny_day_test(
            "UPDATE table_name1 NOT INDEXED SET col1 = 1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_multiple_set_clauses() {
        let mut expected = update_statement();
        expected.set_clause = vec![
            SetClause::ColumnAssignment(Identifier::from("column1"), numeric_expr("1")),
            SetClause::MultipleColumnAssignment(
                vec![Identifier::from("column2"), Identifier::from("column3")],
                numeric_expr("23"),
            ),
            SetClause::ColumnAssignment(Identifier::from("column4"), numeric_expr("444")),
        ];

        run_sunny_day_test(
            "UPDATE table_name1 SET column1 = 1, (column2, column3) = 23, column4 = 444",
            Statement::Update(expected),
        );
    }

    #[test]
    fn update_statement_with_where_clause() {
        let mut expected_statement = update_statement();
        expected_statement.where_clause = Some(Box::new(numeric_expr("1")));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 WHERE 1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_where_clause_expression() {
        let mut expected_statement = update_statement();
        expected_statement.where_clause = Some(Box::new(binary_op(
            BinaryOp::Equals,
            identifier_expr(&["column1"]),
            string_expr("'abc'"),
        )));
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 WHERE column1 = 'abc'",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_returning_clause() {
        let mut expected_statement = update_statement();
        expected_statement.returning_clause = vec![ReturningClause::Wildcard];

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 RETURNING *",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_returning_clauses() {
        let mut expected_statement = update_statement();
        expected_statement.returning_clause = vec![
            ReturningClause::Wildcard,
            ReturningClause::Expr(numeric_expr("1")),
            ReturningClause::ExprWithAlias(identifier_expr(&["column1"]), "alias1".to_string()),
        ];

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 RETURNING *, 1, column1 AS alias1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_statement_with_order_by_clause() {
        let mut expected_statement = update_statement();
        expected_statement.order_by = Some(vec![
            OrderingTerm {
                expression: Box::new(identifier_expr(&["column_1"])),
                ordering: Some(Ordering::Asc),
                nulls_ordering: None,
            },
            OrderingTerm {
                expression: Box::new(collate_expr(
                    identifier_expr(&["column_2"]),
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
        let mut expected_statement = update_statement();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(numeric_expr("10")),
            offset: None,
            additional_limit: None,
        });
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 LIMIT 10",
            Statement::Update(expected_statement),
        );

        let mut expected_statement = update_statement();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(numeric_expr("10")),
            offset: Some(Box::new(numeric_expr("4"))),
            additional_limit: None,
        });
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 LIMIT 10 OFFSET 4",
            Statement::Update(expected_statement),
        );

        let mut expected_statement = update_statement();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(numeric_expr("10")),
            offset: None,
            additional_limit: Some(Box::new(numeric_expr("40"))),
        });
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 LIMIT 10, 40",
            Statement::Update(expected_statement),
        );
    }
}

#[cfg(test)]
mod update_from_table_tests {
    use super::test_utils::update_statement;
    use crate::parser::test_utils::*;
    use crate::{FromClause, Identifier, IndexedType, QualifiedTableName, Statement};

    #[test]
    fn update_from_table() {
        let mut expected_statement = update_statement();
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
        let mut expected_statement = update_statement();
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
        let mut expected_statement = update_statement();
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
        let mut expected_statement = update_statement();
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
        let mut expected_statement = update_statement();
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

#[cfg(test)]
mod update_from_subquery_tests {
    use super::test_utils::update_statement;
    use crate::parser::select::test_utils::select_star_from;
    use crate::parser::test_utils::*;
    use crate::{FromClause, Identifier, SelectFromSubquery, Statement};

    #[test]
    fn update_from_subquery() {
        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Subquery(SelectFromSubquery {
            subquery: Box::new(select_star_from(Identifier::Single("table_1".to_string()))),
            alias: None,
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM (SELECT * FROM table_1)",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn test_update_from_subquery_aliased() {
        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Subquery(SelectFromSubquery {
            subquery: Box::new(select_star_from(Identifier::Single("table_1".to_string()))),
            alias: Some("alias".to_string()),
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM (SELECT * FROM table_1) as alias",
            Statement::Update(expected_statement.clone()),
        );

        // without the as keyword
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM (SELECT * FROM table_1) alias",
            Statement::Update(expected_statement.clone()),
        );
    }
}

#[cfg(test)]
mod update_from_table_function_tests {
    use super::test_utils::update_statement;
    use crate::expression::test_utils::{binary_op, identifier_expr, numeric_expr};
    use crate::parser::test_utils::*;
    use crate::{BinaryOp, FromClause, Identifier, SelectFromFunction, Statement};

    #[test]
    fn update_from_table_function() {
        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Function(SelectFromFunction {
            function_name: Identifier::Single("function_1".to_string()),
            arguments: vec![numeric_expr("1")],
            alias: None,
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM function_1(1)",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_from_table_function_with_schema() {
        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Function(SelectFromFunction {
            function_name: Identifier::Compound(vec![
                "schema_1".to_string(),
                "function_1".to_string(),
            ]),
            arguments: vec![binary_op(
                BinaryOp::Plus,
                numeric_expr("1"),
                numeric_expr("2"),
            )],
            alias: None,
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM schema_1.function_1(1+2)",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_from_table_function_with_multiple_arguments() {
        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Function(SelectFromFunction {
            function_name: Identifier::Compound(vec![
                "schema_1".to_string(),
                "function_1".to_string(),
            ]),
            arguments: vec![
                numeric_expr("1"),
                identifier_expr(&["col1"]),
                numeric_expr("3"),
            ],
            alias: None,
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM schema_1.function_1(1, col1, 3)",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_from_table_function_with_alias() {
        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Function(SelectFromFunction {
            function_name: Identifier::Compound(vec![
                "schema_1".to_string(),
                "function_1".to_string(),
            ]),
            arguments: vec![numeric_expr("1"), numeric_expr("2"), numeric_expr("3")],
            alias: Some("alias".to_string()),
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM schema_1.function_1(1, 2, 3) AS alias",
            Statement::Update(expected_statement.clone()),
        );

        // without the AS keyword
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM schema_1.function_1(1, 2, 3) alias",
            Statement::Update(expected_statement.clone()),
        );
    }
}

#[cfg(test)]
mod update_from_comma_separated_subqueries_tests {
    use super::test_utils::update_statement;
    use crate::parser::select::test_utils::select_star_from;
    use crate::parser::test_utils::*;
    use crate::{
        FromClause, Identifier, IndexedType, JoinClause, JoinTable, JoinType, QualifiedTableName,
        SelectFromSubquery, Statement,
    };

    #[test]
    fn update_from_comma_separated_subqueries() {
        fn join(lhs: FromClause, rhs: FromClause) -> FromClause {
            FromClause::Join(JoinClause {
                lhs_table: Box::new(lhs),
                join_tables: vec![JoinTable {
                    join_type: JoinType::Cross,
                    table: Box::new(rhs),
                    constraints: None,
                }],
            })
        }

        let table_1 = FromClause::Table(QualifiedTableName::from(Identifier::Single(
            "table_1".to_string(),
        )));
        let schema2_table2 =
            FromClause::Table(QualifiedTableName::from(Identifier::Compound(vec![
                "schema2".to_string(),
                "table2".to_string(),
            ])));

        let schema3_table3 = FromClause::Table(QualifiedTableName {
            table_id: Identifier::Compound(vec!["schema3".to_string(), "table3".to_string()]),
            alias: Some("table3_alias".to_string()),
            indexed_type: None,
        });

        let indexed_table = FromClause::Table(QualifiedTableName {
            table_id: Identifier::Single("indexed_table".to_string()),
            alias: Some("t1".to_string()),
            indexed_type: Some(IndexedType::Indexed("index_1".to_string())),
        });

        let not_indexed_table = FromClause::Table(QualifiedTableName {
            table_id: Identifier::Single("not_indexed_table".to_string()),
            alias: Some("t2".to_string()),
            indexed_type: Some(IndexedType::NotIndexed),
        });

        let subquery = FromClause::Subquery(SelectFromSubquery {
            subquery: Box::new(select_star_from(Identifier::Single("table_2".to_string()))),
            alias: Some("select_alias".to_string()),
        });

        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Froms(vec![join(
            table_1,
            join(
                schema2_table2,
                join(
                    schema3_table3,
                    join(indexed_table, join(not_indexed_table, subquery)),
                ),
            ),
        )]));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM (
                    table_1,
                    schema2.table2,
                    schema3.table3 as table3_alias,
                    indexed_table as t1 INDEXED BY index_1,
                    not_indexed_table as t2 NOT INDEXED,
                    (SELECT * FROM table_2) as select_alias
                )",
            Statement::Update(expected_statement),
        );
    }
}

#[cfg(test)]
mod update_from_with_join_clause_tests {
    use super::test_utils::update_statement;
    use crate::expression::test_utils::{binary_op, identifier_expr};
    use crate::parser::select::test_utils::select_from;
    use crate::parser::test_utils::*;
    use crate::{
        BinaryOp, FromClause, Identifier, IndexedType, JoinClause, JoinConstraint, JoinTable,
        JoinType, QualifiedTableName, SelectFromSubquery, Statement,
    };

    fn to_table(table_name: &str) -> Box<FromClause> {
        Box::new(FromClause::Table(QualifiedTableName::from(
            Identifier::Single(table_name.to_string()),
        )))
    }

    #[test]
    fn update_from_with_join_clause() {
        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Join(JoinClause {
            lhs_table: to_table("table_1"),
            join_tables: vec![JoinTable {
                join_type: JoinType::Inner(false),
                table: to_table("table_2"),
                constraints: None,
            }],
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM table_1 INNER JOIN table_2",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_from_with_join_types() {
        let join_types = vec![
            JoinType::Inner(false),
            JoinType::Left(false),
            JoinType::Right(false),
            JoinType::Full(false),
            // NATURAL JOIN
            JoinType::Inner(true),
            JoinType::Left(true),
            JoinType::Right(true),
            JoinType::Full(true),
            JoinType::Cross,
        ];

        for join_type in join_types {
            let mut expected_statement = update_statement();
            expected_statement.from_clause = Some(FromClause::Join(JoinClause {
                lhs_table: to_table("table_1"),
                join_tables: vec![JoinTable {
                    join_type: join_type.clone(),
                    table: to_table("table_2"),
                    constraints: None,
                }],
            }));

            run_sunny_day_test(
                &format!(
                    "UPDATE table_name1 SET col1 = 1 FROM table_1 {} JOIN table_2",
                    &join_type
                ),
                Statement::Update(expected_statement),
            );
        }
    }

    #[test]
    fn update_from_with_cross_join() {
        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Join(JoinClause {
            lhs_table: to_table("table_1"),
            join_tables: vec![JoinTable {
                join_type: JoinType::Cross,
                table: to_table("table_2"),
                constraints: None,
            }],
        }));

        // implicit cross join
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM table_1, table_2",
            Statement::Update(expected_statement.clone()),
        );

        // explicit cross join
        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM table_1 CROSS JOIN table_2",
            Statement::Update(expected_statement.clone()),
        );
    }

    #[test]
    fn update_from_with_join_clause_with_on_constraints() {
        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Join(JoinClause {
            lhs_table: to_table("table_1"),
            join_tables: vec![JoinTable {
                join_type: JoinType::Inner(false),
                table: to_table("table_2"),
                constraints: Some(JoinConstraint::On(binary_op(
                    BinaryOp::Equals,
                    identifier_expr(&["table_1", "col1"]),
                    identifier_expr(&["table_2", "col1"]),
                ))),
            }],
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM table_1 INNER JOIN table_2 ON table_1.col1 = table_2.col1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_from_with_join_clause_with_using_constraints() {
        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Join(JoinClause {
            lhs_table: to_table("table_1"),
            join_tables: vec![JoinTable {
                join_type: JoinType::Inner(false),
                table: to_table("table_2"),
                constraints: Some(JoinConstraint::Using(vec![Identifier::Single(
                    "col1".to_string(),
                )])),
            }],
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM table_1 INNER JOIN table_2 USING (col1)",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_from_with_multiple_join_clauses() {
        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Join(JoinClause {
            lhs_table: to_table("table_1"),
            join_tables: vec![
                JoinTable {
                    join_type: JoinType::Inner(false),
                    table: to_table("table_2"),
                    constraints: Some(JoinConstraint::On(binary_op(
                        BinaryOp::Equals,
                        identifier_expr(&["table_1", "col1"]),
                        identifier_expr(&["table_2", "col2"]),
                    ))),
                },
                JoinTable {
                    join_type: JoinType::Full(false),
                    table: to_table("table_3"),
                    constraints: Some(JoinConstraint::Using(vec![Identifier::Single(
                        "col3".to_string(),
                    )])),
                },
            ],
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM table_1
                INNER JOIN table_2 ON table_1.col1 = table_2.col2
                FULL JOIN table_3 USING (col3)",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn update_from_with_nested_join_clauses() {
        let subquery = JoinTable {
            join_type: JoinType::Inner(false),
            table: Box::new(FromClause::Subquery(SelectFromSubquery {
                subquery: Box::new(select_from(FromClause::Join(JoinClause {
                    lhs_table: Box::new(FromClause::Table(QualifiedTableName {
                        table_id: Identifier::Single("table_2".to_string()),
                        alias: Some("t2".to_string()),
                        indexed_type: None,
                    })),
                    join_tables: vec![JoinTable {
                        join_type: JoinType::Left(false),
                        table: Box::new(FromClause::Table(QualifiedTableName {
                            table_id: Identifier::Single("table_3".to_string()),
                            alias: Some("t3".to_string()),
                            indexed_type: Some(IndexedType::Indexed("index_3".to_string())),
                        })),
                        constraints: Some(JoinConstraint::On(binary_op(
                            BinaryOp::Equals,
                            identifier_expr(&["t2", "col2"]),
                            identifier_expr(&["t3", "col3"]),
                        ))),
                    }],
                }))),
                alias: Some("t_complex".to_string()),
            })),
            constraints: Some(JoinConstraint::On(binary_op(
                BinaryOp::Equals,
                identifier_expr(&["t1", "col1"]),
                identifier_expr(&["t_complex", "col1"]),
            ))),
        };

        let mut expected_statement = update_statement();
        expected_statement.from_clause = Some(FromClause::Join(JoinClause {
            lhs_table: to_table("table_1"),
            join_tables: vec![subquery],
        }));

        run_sunny_day_test(
            "UPDATE table_name1 SET col1 = 1 FROM table_1 INNER JOIN
                (
                    SELECT * FROM table_2 as t2 LEFT JOIN table_3 as t3 INDEXED BY index_3
                    ON t2.col2 = t3.col3
                ) as t_complex
                ON t1.col1 = t_complex.col1",
            Statement::Update(expected_statement),
        );
    }
}

#[cfg(test)]
mod update_statements_with_cte_tests {
    use super::super::cte::test_utils::cte_expression;
    use super::test_utils::update_statement;
    use crate::parser::select::test_utils::select_from;
    use crate::parser::test_utils::*;
    use crate::{FromClause, Identifier, QualifiedTableName, Statement, WithCteStatement};

    #[test]
    fn update_with_cte() {
        let cte = cte_expression(
            Identifier::Single("cte_1".to_string()),
            vec![],
            None,
            select_from(FromClause::Table(QualifiedTableName::from(
                Identifier::from("cte_table"),
            ))),
        );
        let mut expected_statement = update_statement();
        expected_statement.table_name =
            QualifiedTableName::from(Identifier::Single("cte_1".to_string()));
        expected_statement.with_cte = Some(WithCteStatement {
            recursive: true,
            cte_expressions: vec![cte],
        });

        run_sunny_day_test(
            "WITH RECURSIVE cte_1 AS (SELECT * FROM cte_table) UPDATE cte_1 SET col1 = 1",
            Statement::Update(expected_statement),
        );
    }

    #[test]
    fn test_update_with_multiple_ctes() {
        let cte1 = cte_expression(
            Identifier::Single("cte_1".to_string()),
            vec![],
            None,
            select_from(FromClause::Table(QualifiedTableName::from(
                Identifier::Single("cte_table1".to_string()),
            ))),
        );

        let cte2 = cte_expression(
            Identifier::Single("cte_2".to_string()),
            vec![],
            None,
            select_from(FromClause::Table(QualifiedTableName::from(
                Identifier::Single("cte_table2".to_string()),
            ))),
        );

        let mut expected_statement = update_statement();
        expected_statement.table_name =
            QualifiedTableName::from(Identifier::Single("cte_2".to_string()));
        expected_statement.with_cte = Some(WithCteStatement {
            recursive: false,
            cte_expressions: vec![cte1, cte2],
        });

        run_sunny_day_test(
            "WITH cte_1 AS (SELECT * FROM cte_table1), cte_2 AS (SELECT * FROM cte_table2) UPDATE cte_2 SET col1 = 1",
            Statement::Update(expected_statement),
        );
    }
}
