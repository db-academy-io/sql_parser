use crate::{
    parser::select::SelectStatementParser, ConflictClause, Identifier, IndexedColumn,
    InsertStatement, InsertValues, Keyword, Ordering, QualifiedTableName, TokenType, UpsertAction,
    UpsertClause, UpsertConflictTarget, UpsertUpdate,
};

use super::{
    delete::DeleteStatementParser,
    expression::{ExpressionParser, IdentifierParser},
    update::UpdateStatementParser,
    Parser, ParsingError,
};

pub trait InsertStatementParser {
    fn parse_insert_statement(&mut self) -> Result<InsertStatement, ParsingError>;

    fn parse_table_name(&mut self) -> Result<QualifiedTableName, ParsingError>;

    fn parse_columns_names(&mut self) -> Result<Vec<Identifier>, ParsingError>;

    fn parse_insert_values(&mut self) -> Result<InsertValues, ParsingError>;

    fn parse_upsert_clauses(&mut self) -> Result<Option<Vec<UpsertClause>>, ParsingError>;

    fn parse_upsert_clause(&mut self) -> Result<UpsertClause, ParsingError>;

    fn parse_upsert_conflict_target(
        &mut self,
    ) -> Result<Option<UpsertConflictTarget>, ParsingError>;

    fn parse_upsert_action(&mut self) -> Result<UpsertAction, ParsingError>;

    fn parse_indexed_columns(&mut self) -> Result<Vec<IndexedColumn>, ParsingError>;

    fn parse_indexed_column(&mut self) -> Result<IndexedColumn, ParsingError>;

    fn parse_ordering(&mut self) -> Result<Option<Ordering>, ParsingError>;
}

impl<'a> InsertStatementParser for Parser<'a> {
    fn parse_insert_statement(&mut self) -> Result<InsertStatement, ParsingError> {
        if self.consume_as_keyword(Keyword::Replace).is_ok() {
            return Ok(InsertStatement {
                conflict_clause: ConflictClause::None,
                table_name: self.parse_table_name()?,
                columns: self.parse_columns_names()?,
                values: self.parse_insert_values()?,
                upsert_clause: self.parse_upsert_clauses()?,
                returning_clause: self.parse_returning_clause()?,
            });
        }

        self.consume_as_keyword(Keyword::Insert)?;

        Ok(InsertStatement {
            conflict_clause: self.parse_on_conflict_clause()?,
            table_name: self.parse_table_name()?,
            columns: self.parse_columns_names()?,
            values: self.parse_insert_values()?,
            upsert_clause: self.parse_upsert_clauses()?,
            returning_clause: self.parse_returning_clause()?,
        })
    }

    fn parse_table_name(&mut self) -> Result<QualifiedTableName, ParsingError> {
        self.consume_as_keyword(Keyword::Into)?;
        self.parse_qualified_table_name()
    }

    fn parse_columns_names(&mut self) -> Result<Vec<Identifier>, ParsingError> {
        let mut columns = vec![];

        if self.consume_as(TokenType::LeftParen).is_ok() {
            loop {
                let column = self.parse_identifier()?;
                columns.push(column);
                if self.consume_as(TokenType::Comma).is_err() {
                    break;
                }
            }
            self.consume_as(TokenType::RightParen)?;
        }

        Ok(columns)
    }

    fn parse_insert_values(&mut self) -> Result<InsertValues, ParsingError> {
        if self.consume_as_keyword(Keyword::Values).is_ok() {
            let mut values = vec![];

            // Parsing the values clause, which can be a list of list expressions
            loop {
                if self.consume_as(TokenType::LeftParen).is_err() {
                    break;
                }

                let mut value = vec![];
                loop {
                    value.push(self.parse_expression()?);
                    if self.consume_as(TokenType::Comma).is_err() {
                        break;
                    }
                }

                values.push(value);
                self.consume_as(TokenType::RightParen)?;

                if self.consume_as(TokenType::Comma).is_err() {
                    break;
                }
            }

            return Ok(InsertValues::Values(values));
        }

        if let Ok(Keyword::Select) = self.peek_as_keyword() {
            return Ok(InsertValues::Select(self.parse_select_statement()?));
        }

        if self.consume_as_keyword(Keyword::Default).is_ok() {
            self.consume_as_keyword(Keyword::Values)?;
            return Ok(InsertValues::DefaultValues);
        }

        Err(ParsingError::UnexpectedToken(
            self.peek_token()?.to_string(),
        ))
    }

    fn parse_upsert_clauses(&mut self) -> Result<Option<Vec<UpsertClause>>, ParsingError> {
        if let Ok(Keyword::On) = self.peek_as_keyword() {
            let mut clauses = vec![];

            while let Ok(Keyword::On) = self.peek_as_keyword() {
                clauses.push(self.parse_upsert_clause()?);
            }

            Ok(Some(clauses))
        } else {
            Ok(None)
        }
    }

    fn parse_upsert_clause(&mut self) -> Result<UpsertClause, ParsingError> {
        self.consume_as_keyword(Keyword::On)?;
        self.consume_as_keyword(Keyword::Conflict)?;

        let conflict_target = self.parse_upsert_conflict_target()?;
        let action = self.parse_upsert_action()?;

        Ok(UpsertClause {
            conflict_target,
            action,
        })
    }

    fn parse_upsert_conflict_target(
        &mut self,
    ) -> Result<Option<UpsertConflictTarget>, ParsingError> {
        if self.peek_as(TokenType::LeftParen).is_ok() {
            let columns = self.parse_indexed_columns()?;
            let where_clause = self.parse_where_clause()?;
            Ok(Some(UpsertConflictTarget {
                columns,
                where_clause,
            }))
        } else {
            Ok(None)
        }
    }

    fn parse_upsert_action(&mut self) -> Result<UpsertAction, ParsingError> {
        self.consume_as_keyword(Keyword::Do)?;

        if self.consume_as_keyword(Keyword::Nothing).is_ok() {
            return Ok(UpsertAction::Nothing);
        }

        if self.consume_as_keyword(Keyword::Update).is_ok() {
            let set_clauses = self.parse_set_clauses()?;

            let where_clause = self.parse_where_clause()?;

            return Ok(UpsertAction::Update(UpsertUpdate {
                set_clauses,
                where_clause,
            }));
        }

        Err(ParsingError::UnexpectedToken(
            self.peek_token()?.to_string(),
        ))
    }

    fn parse_indexed_columns(&mut self) -> Result<Vec<IndexedColumn>, ParsingError> {
        self.consume_as(TokenType::LeftParen)?;

        let mut columns = vec![];
        loop {
            columns.push(self.parse_indexed_column()?);
            if self.consume_as(TokenType::Comma).is_err() {
                break;
            }
        }
        self.consume_as(TokenType::RightParen)?;
        Ok(columns)
    }

    fn parse_indexed_column(&mut self) -> Result<IndexedColumn, ParsingError> {
        let column = self.parse_expression()?;
        let ordering = self.parse_ordering()?;
        Ok(IndexedColumn { column, ordering })
    }

    fn parse_ordering(&mut self) -> Result<Option<Ordering>, ParsingError> {
        if self.consume_as_keyword(Keyword::Asc).is_ok() {
            return Ok(Some(Ordering::Asc));
        } else if self.consume_as_keyword(Keyword::Desc).is_ok() {
            return Ok(Some(Ordering::Desc));
        }
        Ok(None)
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{ConflictClause, Identifier, InsertValues, QualifiedTableName};

    use super::*;

    pub fn insert_statement() -> InsertStatement {
        InsertStatement {
            conflict_clause: ConflictClause::None,
            table_name: QualifiedTableName::from(Identifier::Single("table_name1".into())),
            columns: vec![],
            values: InsertValues::DefaultValues,
            upsert_clause: None,
            returning_clause: vec![],
        }
    }
}

#[cfg(test)]
mod tests_insert_statement {
    use super::test_utils::*;
    use crate::{
        expression::test_utils::{
            binary_op_expression, collate_expression, expression_list, identifier_expression,
            numeric_literal_expression,
        },
        parser::{
            cte::test_utils::cte_expression, select::test_utils::select_from,
            test_utils::run_sunny_day_test,
        },
        BinaryOp, ConflictClause, CteExpression, FromClause, Identifier, IndexedColumn,
        InsertValues, QualifiedTableName, ReturningClause, SetClause, Statement, UpsertAction,
        UpsertClause, UpsertConflictTarget, UpsertUpdate, WithCteStatement,
    };

    #[test]
    fn test_parse_insert_statement_basic() {
        let expected_statement = insert_statement();
        run_sunny_day_test(
            "INSERT INTO table_name1 DEFAULT VALUES",
            Statement::Insert(expected_statement),
        );
    }

    #[test]
    fn test_parse_insert_with_qualified_table_and_schema() {
        let mut expected = insert_statement();
        expected.table_name = QualifiedTableName::from(Identifier::Compound(vec![
            "schema_1".to_string(),
            "table_name1".to_string(),
        ]));
        run_sunny_day_test(
            "INSERT INTO schema_1.table_name1 DEFAULT VALUES",
            Statement::Insert(expected),
        );
    }

    #[test]
    fn test_parse_insert_statement_with_alias() {
        let mut expected = insert_statement();
        expected.table_name = QualifiedTableName {
            table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            alias: Some("alias_1".to_string()),
            indexed_type: None,
        };
        run_sunny_day_test(
            "INSERT INTO schema_1.table_1 AS alias_1 DEFAULT VALUES",
            Statement::Insert(expected),
        );
    }

    #[test]
    fn test_parse_insert_statement_with_values() {
        let mut statement = insert_statement();
        statement.values = InsertValues::Values(vec![
            vec![
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
            ],
            vec![
                numeric_literal_expression("3"),
                numeric_literal_expression("4"),
            ],
        ]);
        run_sunny_day_test(
            "INSERT INTO table_name1 VALUES (1, 2), (3, 4)",
            Statement::Insert(statement),
        );
    }

    #[test]
    fn test_parse_insert_statement_with_columns_and_values() {
        let mut statement = insert_statement();
        statement.columns = vec![
            Identifier::Single("col1".to_string()),
            Identifier::Single("col2".to_string()),
        ];

        statement.values = InsertValues::Values(vec![
            vec![
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
            ],
            vec![
                numeric_literal_expression("3"),
                numeric_literal_expression("4"),
            ],
        ]);
        run_sunny_day_test(
            "INSERT INTO table_name1 (col1, col2) VALUES (1, 2), (3, 4)",
            Statement::Insert(statement),
        );
    }

    #[test]
    fn test_parse_insert_statement_with_values_from_select_statement() {
        let mut statement = insert_statement();
        statement.values = InsertValues::Select(select_from(FromClause::Table(
            QualifiedTableName::from(Identifier::Single("table2".to_string())),
        )));
        run_sunny_day_test(
            "INSERT INTO table_name1 SELECT * FROM table2",
            Statement::Insert(statement),
        );
    }

    #[test]
    fn test_parse_insert_statement_with_conflict_clauses() {
        let conflict_clauses = vec![
            ConflictClause::None,
            ConflictClause::Rollback,
            ConflictClause::Abort,
            ConflictClause::Fail,
            ConflictClause::Ignore,
            ConflictClause::Replace,
        ];

        for conflict_clause in conflict_clauses {
            let sql = format!(
                "INSERT {} INTO table_name1 DEFAULT VALUES;",
                conflict_clause
            );
            let mut expected_statement = insert_statement();
            expected_statement.conflict_clause = conflict_clause;

            run_sunny_day_test(&sql, Statement::Insert(expected_statement));
        }
    }

    #[test]
    fn test_parse_insert_statement_with_upsert_clauses() {
        let sql = r#"
            INSERT INTO table_name1 DEFAULT VALUES 
                ON CONFLICT (col1) DO NOTHING 
                ON CONFLICT (col2 COLLATE utf8) 
                    WHERE col2 > 10
                    DO UPDATE SET col2 = col2 + 1
                ON CONFLICT (col3, col4)
                    WHERE col3 == 1
                    DO UPDATE SET (col3, col4) = (col4, col3)
                    WHERE col4 = 2

        "#;
        let mut expected_statement = insert_statement();
        expected_statement.upsert_clause = Some(vec![
            UpsertClause {
                conflict_target: Some(UpsertConflictTarget {
                    columns: vec![IndexedColumn {
                        column: identifier_expression(&["col1"]),
                        ordering: None,
                    }],
                    where_clause: None,
                }),
                action: UpsertAction::Nothing,
            },
            UpsertClause {
                conflict_target: Some(UpsertConflictTarget {
                    columns: vec![IndexedColumn {
                        column: collate_expression(
                            identifier_expression(&["col2"]),
                            "utf8".to_string(),
                        ),
                        ordering: None,
                    }],
                    where_clause: Some(Box::new(binary_op_expression(
                        BinaryOp::GreaterThan,
                        identifier_expression(&["col2"]),
                        numeric_literal_expression("10"),
                    ))),
                }),
                action: UpsertAction::Update(UpsertUpdate {
                    set_clauses: vec![SetClause::ColumnAssignment(
                        Identifier::Single("col2".to_string()),
                        binary_op_expression(
                            BinaryOp::Plus,
                            identifier_expression(&["col2"]),
                            numeric_literal_expression("1"),
                        ),
                    )],
                    where_clause: None,
                }),
            },
            UpsertClause {
                conflict_target: Some(UpsertConflictTarget {
                    columns: vec![
                        IndexedColumn {
                            column: identifier_expression(&["col3"]),
                            ordering: None,
                        },
                        IndexedColumn {
                            column: identifier_expression(&["col4"]),
                            ordering: None,
                        },
                    ],
                    where_clause: Some(Box::new(binary_op_expression(
                        BinaryOp::EqualsEquals,
                        identifier_expression(&["col3"]),
                        numeric_literal_expression("1"),
                    ))),
                }),
                action: UpsertAction::Update(UpsertUpdate {
                    set_clauses: vec![SetClause::MultipleColumnAssignment(
                        vec![
                            Identifier::Single("col3".to_string()),
                            Identifier::Single("col4".to_string()),
                        ],
                        expression_list(vec![
                            identifier_expression(&["col4"]),
                            identifier_expression(&["col3"]),
                        ]),
                    )],
                    where_clause: Some(Box::new(binary_op_expression(
                        BinaryOp::Equals,
                        identifier_expression(&["col4"]),
                        numeric_literal_expression("2"),
                    ))),
                }),
            },
        ]);

        run_sunny_day_test(&sql, Statement::Insert(expected_statement));
    }

    #[test]
    fn test_parse_insert_statement_with_returning_clause() {
        let mut expected_statement = insert_statement();
        expected_statement.returning_clause = vec![ReturningClause::Wildcard];
        run_sunny_day_test(
            "INSERT INTO table_name1 DEFAULT VALUES RETURNING *",
            Statement::Insert(expected_statement),
        );
    }

    #[test]
    fn test_parse_insert_statement_with_returning_clauses() {
        let mut expected_statement = insert_statement();
        expected_statement.returning_clause = vec![
            ReturningClause::Wildcard,
            ReturningClause::Expr(numeric_literal_expression("1")),
            ReturningClause::ExprWithAlias(
                identifier_expression(&["column1"]),
                "alias1".to_string(),
            ),
        ];
        run_sunny_day_test(
            "INSERT INTO table_name1 DEFAULT VALUES RETURNING *, 1, column1 AS alias1",
            Statement::Insert(expected_statement),
        );
    }

    #[test]
    fn test_insert_with_cte() {
        let expected_insert_statement = insert_statement();

        let expected_statement = WithCteStatement {
            recursive: true,
            cte_expressions: vec![CteExpression {
                name: Identifier::Single("cte_1".to_string()),
                column_names: vec![],
                materialized: None,
                select: select_from(FromClause::Table(QualifiedTableName::from(
                    Identifier::from("cte_table"),
                ))),
            }],
            statement: Box::new(Statement::Insert(expected_insert_statement)),
        };
        run_sunny_day_test(
            "WITH RECURSIVE cte_1 AS (SELECT * FROM cte_table) INSERT INTO table_name1 DEFAULT VALUES",
            Statement::WithCte(expected_statement),
        );
    }

    #[test]
    fn test_insert_with_multiple_ctes() {
        let mut expected_insert_statement = insert_statement();
        expected_insert_statement.table_name =
            QualifiedTableName::from(Identifier::Single("cte_2".to_string()));

        let expected_statement = WithCteStatement {
            recursive: false,
            cte_expressions: vec![
                cte_expression(
                    Identifier::Single("cte_1".to_string()),
                    vec![],
                    None,
                    select_from(FromClause::Table(QualifiedTableName::from(
                        Identifier::Single("cte_table1".to_string()),
                    ))),
                ),
                cte_expression(
                    Identifier::Single("cte_2".to_string()),
                    vec![],
                    None,
                    select_from(FromClause::Table(QualifiedTableName::from(
                        Identifier::Single("cte_1".to_string()),
                    ))),
                ),
            ],
            statement: Box::new(Statement::Insert(expected_insert_statement)),
        };

        run_sunny_day_test(
            "WITH cte_1 AS (SELECT * FROM cte_table1), cte_2 AS (SELECT * FROM cte_1) INSERT INTO cte_2 DEFAULT VALUES",
            Statement::WithCte(expected_statement),
        );
    }

    #[test]
    fn test_parse_insert_statement_with_replace() {
        let mut expected_statement = insert_statement();
        expected_statement.conflict_clause = ConflictClause::None;
        run_sunny_day_test(
            "REPLACE INTO table_name1 DEFAULT VALUES",
            Statement::Insert(expected_statement),
        );
    }
}
