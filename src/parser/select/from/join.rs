use crate::{
    parser::{ExpressionParser, IdentifierParser, ParsingError},
    FromClause, JoinClause, JoinConstraint, JoinTable, JoinType, Keyword, Parser, TokenType,
};

use super::SelectFromSubqueryParser;

pub trait SelectFromJoinParser {
    fn parse_from_join_clause(&mut self, lhs: FromClause) -> Result<FromClause, ParsingError>;

    fn parse_from_clause_join_type(&mut self) -> Result<JoinType, ParsingError>;

    fn parse_from_clause_join_constraints(
        &mut self,
    ) -> Result<Option<JoinConstraint>, ParsingError>;
}

impl SelectFromJoinParser for Parser<'_> {
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
mod test_utils {
    use crate::select::from::test_utils::select_from_join;
    use crate::{
        DistinctType, Expression, FromClause, Identifier, JoinClause, JoinConstraint, JoinTable,
        JoinType, QualifiedTableName, Select, SelectBody, SelectItem, SelectStatement,
    };

    pub fn join(
        join_type: JoinType,
        table: QualifiedTableName,
        constraints: Option<JoinConstraint>,
    ) -> SelectStatement {
        select_from_join(JoinClause {
            lhs_table: Box::new(FromClause::Table(QualifiedTableName::from(
                Identifier::Single("table_1".to_string()),
            ))),
            join_tables: vec![JoinTable {
                join_type,
                table: Box::new(FromClause::Table(table)),
                constraints,
            }],
        })
    }

    pub fn joins(join_tables: Vec<JoinTable>) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
                from: Some(FromClause::Join(JoinClause {
                    lhs_table: Box::new(FromClause::Table(QualifiedTableName::from(
                        Identifier::Single("table_1".to_string()),
                    ))),
                    join_tables,
                })),
                ..Default::default()
            }),
            order_by: None,
            limit: None,
        }
    }
}

#[cfg(test)]
mod select_from_with_join_clause_tests {
    use super::test_utils::{join, joins};
    use crate::expression::test_utils::{binary_op, identifier_expr};
    use crate::parser::test_utils::*;
    use crate::select::from::test_utils::select_from_join;
    use crate::{
        BinaryOp, FromClause, Identifier, IndexedType, JoinClause, JoinConstraint, JoinTable,
        JoinType, QualifiedTableName, SelectFromSubquery, Statement,
    };

    #[test]
    fn select_from_with_join() {
        let expected_statement = join(
            JoinType::Inner(false),
            QualifiedTableName::from(Identifier::Single("table_2".to_string())),
            None,
        );

        run_sunny_day_test(
            "SELECT * FROM table_1 INNER JOIN table_2",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_with_join_types() {
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
            let expected_statement = join(
                join_type.clone(),
                QualifiedTableName::from(Identifier::Single("table_2".to_string())),
                None,
            );

            run_sunny_day_test(
                &format!("SELECT * FROM table_1 {} JOIN table_2", &join_type),
                Statement::Select(expected_statement),
            );
        }
    }

    #[test]
    fn select_from_with_cross_join() {
        let expected_statement = join(
            JoinType::Cross,
            QualifiedTableName::from(Identifier::Single("table_2".to_string())),
            None,
        );

        run_sunny_day_test(
            "SELECT * FROM table_1, table_2",
            Statement::Select(expected_statement.clone()),
        );

        run_sunny_day_test(
            "SELECT * FROM table_1 CROSS JOIN table_2",
            Statement::Select(expected_statement.clone()),
        );
    }

    #[test]
    fn select_from_with_join_clause_with_on_constraints() {
        let expected_statement = join(
            JoinType::Inner(false),
            QualifiedTableName::from(Identifier::Single("table_2".to_string())),
            Some(JoinConstraint::On(binary_op(
                BinaryOp::Equals,
                identifier_expr(&["table_1", "col1"]),
                identifier_expr(&["table_2", "col1"]),
            ))),
        );

        run_sunny_day_test(
            "SELECT * FROM table_1 INNER JOIN table_2 ON table_1.col1 = table_2.col1",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_with_join_clause_with_using_constraints() {
        let expected_statement = join(
            JoinType::Inner(false),
            QualifiedTableName::from(Identifier::Single("table_2".to_string())),
            Some(JoinConstraint::Using(vec![Identifier::Single(
                "col1".to_string(),
            )])),
        );

        run_sunny_day_test(
            "SELECT * FROM table_1 INNER JOIN table_2 USING (col1)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_with_multiple_join_clauses() {
        let expected_statement = joins(vec![
            JoinTable {
                join_type: JoinType::Inner(false),
                table: Box::new(FromClause::Table(QualifiedTableName::from(
                    Identifier::Single("table_2".to_string()),
                ))),
                constraints: Some(JoinConstraint::On(binary_op(
                    BinaryOp::Equals,
                    identifier_expr(&["table_1", "col1"]),
                    identifier_expr(&["table_2", "col2"]),
                ))),
            },
            JoinTable {
                join_type: JoinType::Full(false),
                table: Box::new(FromClause::Table(QualifiedTableName::from(
                    Identifier::Single("table_3".to_string()),
                ))),
                constraints: Some(JoinConstraint::Using(vec![Identifier::Single(
                    "col3".to_string(),
                )])),
            },
        ]);

        run_sunny_day_test(
            "SELECT *
                FROM table_1
                INNER JOIN table_2 ON table_1.col1 = table_2.col2
                FULL JOIN table_3 USING (col3)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_with_multiple_join_clauses_with_ordering() {
        let subquery = JoinTable {
            join_type: JoinType::Inner(false),
            table: Box::new(FromClause::Subquery(SelectFromSubquery {
                subquery: Box::new(select_from_join(JoinClause {
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
                })),
                alias: Some("t_complex".to_string()),
            })),
            constraints: Some(JoinConstraint::On(binary_op(
                BinaryOp::Equals,
                identifier_expr(&["t1", "col1"]),
                identifier_expr(&["t_complex", "col1"]),
            ))),
        };

        let expected_statement = select_from_join(JoinClause {
            lhs_table: Box::new(FromClause::Table(QualifiedTableName {
                table_id: Identifier::Single("table_1".to_string()),
                alias: Some("t1".to_string()),
                indexed_type: None,
            })),
            join_tables: vec![subquery],
        });

        run_sunny_day_test(
            "SELECT * FROM table_1 as t1 INNER JOIN
                (
                    SELECT * FROM table_2 as t2 LEFT JOIN table_3 as t3 INDEXED BY index_3
                    ON t2.col2 = t3.col3
                ) as t_complex
                ON t1.col1 = t_complex.col1",
            Statement::Select(expected_statement),
        );
    }
}

#[cfg(test)]
mod select_from_comma_separated_entries {
    use crate::parser::test_utils::*;
    use crate::select::from::test_utils::select_from;
    use crate::{
        FromClause, Identifier, IndexedType, JoinClause, JoinTable, JoinType, QualifiedTableName,
        SelectFromSubquery, Statement,
    };

    #[test]
    fn test_select_from_comma_separated_table_or_subqueries() {
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
            subquery: Box::new(select_from(FromClause::Table(QualifiedTableName::from(
                Identifier::Single("table_2".to_string()),
            )))),
            alias: Some("select_alias".to_string()),
        });

        let expected_statement = select_from(FromClause::Froms(vec![join(
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
            "SELECT * FROM (
                    table_1,
                    schema2.table2,
                    schema3.table3 as table3_alias,
                    indexed_table as t1 INDEXED BY index_1,
                    not_indexed_table as t2 NOT INDEXED,
                    (SELECT * FROM table_2) as select_alias
                )",
            Statement::Select(expected_statement),
        );
    }
}
