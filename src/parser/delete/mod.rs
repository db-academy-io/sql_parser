use crate::{
    DeleteStatement, IdentifierParser, Keyword, QualifiedTableName, ReturningClause, TokenType,
};

use super::{expression::ExpressionParser, select::SelectStatementParser, Parser, ParsingError};

pub trait DeleteStatementParser {
    fn parse_delete_statement(&mut self) -> Result<DeleteStatement, ParsingError>;

    fn parse_qualified_table_name(&mut self) -> Result<QualifiedTableName, ParsingError>;

    fn parse_returning_clause(&mut self) -> Result<Vec<ReturningClause>, ParsingError>;
}

impl<'a> DeleteStatementParser for Parser<'a> {
    fn parse_delete_statement(&mut self) -> Result<DeleteStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Delete)?;
        self.consume_as_keyword(Keyword::From)?;

        Ok(DeleteStatement {
            with_cte: None,
            table_name: self.parse_qualified_table_name()?,
            where_clause: self.parse_where_clause()?,
            returning_clause: self.parse_returning_clause()?,
            order_by: self.parse_order_by_clause()?,
            limit: self.parse_limit_clause()?,
        })
    }

    fn parse_qualified_table_name(&mut self) -> Result<QualifiedTableName, ParsingError> {
        Ok(QualifiedTableName {
            table_id: self.parse_identifier()?,
            alias: self.parse_alias_after_as_keyword()?,
            indexed_type: self.parse_indexed_type()?,
        })
    }

    fn parse_returning_clause(&mut self) -> Result<Vec<ReturningClause>, ParsingError> {
        if self.consume_as_keyword(Keyword::Returning).is_ok() {
            let mut returning_clauses = vec![];

            loop {
                if self.peek_as(TokenType::Star).is_ok() {
                    self.consume_as(TokenType::Star)?;
                    returning_clauses.push(ReturningClause::Wildcard);
                } else if let Ok(expression) = self.parse_expression() {
                    if let Some(alias) = self.parse_alias_after_as_keyword()? {
                        returning_clauses.push(ReturningClause::ExprWithAlias(expression, alias));
                    } else {
                        returning_clauses.push(ReturningClause::Expr(expression));
                    }
                }

                if self.consume_as(TokenType::Comma).is_err() {
                    break;
                }
            }

            Ok(returning_clauses)
        } else {
            Ok(vec![])
        }
    }
}

#[cfg(test)]
pub mod test_utils {

    use crate::{CteExpression, DeleteStatement, Identifier, QualifiedTableName, WithCteStatement};

    pub fn delete_statement() -> DeleteStatement {
        DeleteStatement {
            with_cte: None,
            table_name: QualifiedTableName::from(Identifier::from("table_name1")),
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: None,
        }
    }

    pub fn delete_statement_with_cte_clause(
        recursive: bool,
        cte_expressions: Vec<CteExpression>,
        table_name: QualifiedTableName,
    ) -> DeleteStatement {
        DeleteStatement {
            with_cte: Some(WithCteStatement {
                recursive,
                cte_expressions,
            }),
            table_name,
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: None,
        }
    }
}

#[cfg(test)]
mod delete_statement_tests {
    use super::test_utils::delete_statement;
    use crate::{
        expression::test_utils::{
            binary_op_expression, collate_expression, identifier_expression,
            numeric_literal_expression, string_literal_expression,
        },
        parser::test_utils::run_sunny_day_test,
        BinaryOp, Identifier, IndexedType, LimitClause, NullsOrdering, Ordering, OrderingTerm,
        QualifiedTableName, ReturningClause, Statement,
    };

    #[test]
    fn delete_statement_test() {
        let expected_statement = delete_statement();

        run_sunny_day_test(
            "DELETE FROM table_name1",
            Statement::Delete(expected_statement),
        );
    }

    #[test]
    fn delete_statement_with_schema() {
        let mut expected_statement = delete_statement();
        expected_statement.table_name.table_id =
            Identifier::Compound(vec!["schema_1".to_string(), "table_name1".to_string()]);

        run_sunny_day_test(
            "DELETE FROM schema_1.table_name1",
            Statement::Delete(expected_statement),
        );
    }

    #[test]
    fn delete_statement_with_alias() {
        let mut expected_statement = delete_statement();
        expected_statement.table_name = QualifiedTableName {
            table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            alias: Some("alias_1".to_string()),
            indexed_type: None,
        };

        run_sunny_day_test(
            "DELETE FROM schema_1.table_1 AS alias_1",
            Statement::Delete(expected_statement),
        );
    }

    #[test]
    fn delete_statement_with_indexed_type() {
        let mut expected_statement = delete_statement();
        expected_statement.table_name = QualifiedTableName {
            table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            alias: Some("alias_1".to_string()),
            indexed_type: Some(IndexedType::Indexed("index_1".to_string())),
        };

        run_sunny_day_test(
            "DELETE FROM schema_1.table_1 AS alias_1 INDEXED BY index_1",
            Statement::Delete(expected_statement),
        );

        let mut expected_statement = delete_statement();
        expected_statement.table_name = QualifiedTableName {
            table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            alias: Some("alias_1".to_string()),
            indexed_type: Some(IndexedType::NotIndexed),
        };

        run_sunny_day_test(
            "DELETE FROM schema_1.table_1 AS alias_1 NOT INDEXED",
            Statement::Delete(expected_statement),
        );
    }

    #[test]
    fn delete_statement_with_where_clause() {
        let mut expected_statement = delete_statement();
        expected_statement.where_clause = Some(Box::new(numeric_literal_expression("1")));

        run_sunny_day_test(
            "DELETE FROM table_name1 WHERE 1",
            Statement::Delete(expected_statement),
        );
    }

    #[test]
    fn delete_statement_with_where_clause_and_column_expression() {
        let mut expected_statement = delete_statement();
        expected_statement.where_clause = Some(Box::new(binary_op_expression(
            BinaryOp::Equals,
            identifier_expression(&["column_1"]),
            string_literal_expression("'abc'"),
        )));

        run_sunny_day_test(
            "DELETE FROM table_name1 WHERE column_1 = 'abc'",
            Statement::Delete(expected_statement),
        );
    }

    #[test]
    fn delete_statement_with_returning_clause() {
        let mut expected_statement = delete_statement();
        expected_statement.returning_clause = vec![ReturningClause::Wildcard];

        run_sunny_day_test(
            "DELETE FROM table_name1 RETURNING *",
            Statement::Delete(expected_statement),
        );
    }

    #[test]
    fn delete_statement_with_multiple_returning_clauses() {
        let mut expected_statement = delete_statement();
        expected_statement.returning_clause = vec![
            ReturningClause::Wildcard,
            ReturningClause::Expr(numeric_literal_expression("1")),
            ReturningClause::ExprWithAlias(
                identifier_expression(&["column_1"]),
                "alias_1".to_string(),
            ),
        ];

        run_sunny_day_test(
            "DELETE FROM table_name1 RETURNING *, 1, column_1 AS alias_1",
            Statement::Delete(expected_statement),
        );
    }

    #[test]
    fn delete_statement_with_order_by_clause() {
        let mut expected_statement = delete_statement();
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
            "DELETE FROM table_name1 ORDER BY column_1 ASC, column_2 COLLATE binary NULLS LAST",
            Statement::Delete(expected_statement),
        );
    }

    #[test]
    fn delete_statement_with_limit_clause() {
        let mut expected_statement = delete_statement();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(numeric_literal_expression("10")),
            offset: None,
            additional_limit: None,
        });
        run_sunny_day_test(
            "DELETE FROM table_name1 LIMIT 10",
            Statement::Delete(expected_statement),
        );

        let mut expected_statement = delete_statement();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(numeric_literal_expression("10")),
            offset: Some(Box::new(numeric_literal_expression("4"))),
            additional_limit: None,
        });
        run_sunny_day_test(
            "DELETE FROM table_name1 LIMIT 10 OFFSET 4",
            Statement::Delete(expected_statement),
        );

        let mut expected_statement = delete_statement();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(numeric_literal_expression("10")),
            offset: None,
            additional_limit: Some(Box::new(numeric_literal_expression("40"))),
        });
        run_sunny_day_test(
            "DELETE FROM table_name1 LIMIT 10, 40",
            Statement::Delete(expected_statement),
        );
    }

    #[test]
    fn delete_statement_with_all_clauses() {
        let mut expected_statement = delete_statement();
        expected_statement.table_name = QualifiedTableName {
            table_id: Identifier::Single("table_name1".to_string()),
            alias: Some("alias_1".to_string()),
            indexed_type: Some(IndexedType::Indexed("index_1".to_string())),
        };
        expected_statement.where_clause = Some(Box::new(binary_op_expression(
            BinaryOp::Equals,
            identifier_expression(&["column_1"]),
            string_literal_expression("'abc'"),
        )));
        expected_statement.returning_clause = vec![
            ReturningClause::Wildcard,
            ReturningClause::Expr(numeric_literal_expression("1")),
            ReturningClause::ExprWithAlias(
                identifier_expression(&["column_1"]),
                "alias_1".to_string(),
            ),
        ];
        expected_statement.order_by = None;
        expected_statement.limit = None;

        run_sunny_day_test(
            "DELETE FROM table_name1 AS alias_1 INDEXED BY index_1 WHERE column_1 = 'abc' RETURNING *, 1, column_1 AS alias_1",
            Statement::Delete(expected_statement),
        );
    }
}

#[cfg(test)]
mod delete_statements_with_cte_tests {
    use super::super::cte::test_utils::cte_expression;
    use super::test_utils::delete_statement_with_cte_clause;
    use crate::parser::select::test_utils::select_from;
    use crate::parser::test_utils::*;
    use crate::{FromClause, Identifier, QualifiedTableName, Statement};

    #[test]
    fn delete_with_cte() {
        let expected_statement = delete_statement_with_cte_clause(
            true,
            vec![cte_expression(
                Identifier::Single("cte_1".to_string()),
                vec![],
                None,
                select_from(FromClause::Table(QualifiedTableName::from(
                    Identifier::from("cte_table"),
                ))),
            )],
            QualifiedTableName::from(Identifier::Single("cte_1".to_string())),
        );

        run_sunny_day_test(
            "WITH RECURSIVE cte_1 AS (SELECT * FROM cte_table) DELETE FROM cte_1",
            Statement::Delete(expected_statement),
        );
    }

    #[test]
    fn delete_with_multiple_ctes() {
        let expected_statement = delete_statement_with_cte_clause(
            false,
            vec![
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
                        Identifier::Single("cte_table2".to_string()),
                    ))),
                ),
            ],
            QualifiedTableName::from(Identifier::Single("cte_2".to_string())),
        );

        run_sunny_day_test(
            "WITH cte_1 AS (SELECT * FROM cte_table1), cte_2 AS (SELECT * FROM cte_table2) DELETE FROM cte_2",
            Statement::Delete(expected_statement),
        );
    }
}
