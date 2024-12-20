use crate::{
    expression::IdentifierParser, DeleteStatement, Keyword, QualifiedTableName, ReturningClause,
    TokenType,
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

    use crate::{
        CteExpression, DeleteStatement, Expression, Identifier, LimitClause, OrderingTerm,
        QualifiedTableName, ReturningClause, Statement, WithCteStatement,
    };

    pub fn delete_statement(table_name: QualifiedTableName) -> Statement {
        Statement::Delete(DeleteStatement {
            with_cte: None,
            table_name,
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: None,
        })
    }

    pub fn delete_statement2() -> DeleteStatement {
        DeleteStatement {
            with_cte: None,
            table_name: QualifiedTableName::from(Identifier::from("table_name1")),
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: None,
        }
    }

    pub fn delete_statement_with_where_clause(
        table_name: QualifiedTableName,
        where_clause: Expression,
    ) -> Statement {
        Statement::Delete(DeleteStatement {
            with_cte: None,
            table_name,
            where_clause: Some(Box::new(where_clause)),
            returning_clause: vec![],
            order_by: None,
            limit: None,
        })
    }

    pub fn delete_statement_with_returning_clause(
        table_name: QualifiedTableName,
        returning_clause: Vec<ReturningClause>,
    ) -> Statement {
        Statement::Delete(DeleteStatement {
            with_cte: None,
            table_name,
            where_clause: None,
            returning_clause,
            order_by: None,
            limit: None,
        })
    }

    pub fn delete_statement_with_cte_clause(
        recursive: bool,
        cte_expressions: Vec<CteExpression>,
        table_name: QualifiedTableName,
    ) -> Statement {
        Statement::Delete(DeleteStatement {
            with_cte: Some(WithCteStatement {
                recursive,
                cte_expressions,
            }),
            table_name,
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: None,
        })
    }

    pub fn delete_statement_with_order_by_clause(
        table_name: QualifiedTableName,
        order_by: Vec<OrderingTerm>,
    ) -> Statement {
        Statement::Delete(DeleteStatement {
            with_cte: None,
            table_name,
            where_clause: None,
            returning_clause: vec![],
            order_by: Some(order_by),
            limit: None,
        })
    }

    pub fn delete_statement_with_limit_clause(
        table_name: QualifiedTableName,
        limit: LimitClause,
    ) -> Statement {
        Statement::Delete(DeleteStatement {
            with_cte: None,
            table_name,
            where_clause: None,
            returning_clause: vec![],
            order_by: None,
            limit: Some(limit),
        })
    }
}

#[cfg(test)]
mod tests_delete_statements {
    use test_utils::{
        delete_statement, delete_statement_with_limit_clause,
        delete_statement_with_order_by_clause, delete_statement_with_returning_clause,
        delete_statement_with_where_clause,
    };

    use crate::{
        expression::test_utils::{
            binary_op_expression, collate_expression, identifier_expression,
            numeric_literal_expression, string_literal_expression,
        },
        parser::test_utils::run_sunny_day_test,
        BinaryOp, Identifier, IndexedType, LimitClause, NullsOrdering, Ordering, OrderingTerm,
        Statement,
    };

    use super::*;

    #[test]
    fn test_parse_delete_statement_basic() {
        let expected_statement = delete_statement(QualifiedTableName {
            table_id: Identifier::Single("table_1".to_string()),
            alias: None,
            indexed_type: None,
        });

        run_sunny_day_test("DELETE FROM table_1", expected_statement);
    }

    #[test]
    fn test_parse_delete_statement_with_schema() {
        let expected_statement = delete_statement(QualifiedTableName {
            table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            alias: None,
            indexed_type: None,
        });

        run_sunny_day_test("DELETE FROM schema_1.table_1", expected_statement);
    }

    #[test]
    fn test_parse_delete_statement_with_alias() {
        let expected_statement = delete_statement(QualifiedTableName {
            table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            alias: Some("alias_1".to_string()),
            indexed_type: None,
        });

        run_sunny_day_test(
            "DELETE FROM schema_1.table_1 AS alias_1",
            expected_statement,
        );
    }

    #[test]
    fn test_parse_delete_statement_with_indexed_type() {
        let expected_statement = delete_statement(QualifiedTableName {
            table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            alias: Some("alias_1".to_string()),
            indexed_type: Some(IndexedType::Indexed("index_1".to_string())),
        });

        run_sunny_day_test(
            "DELETE FROM schema_1.table_1 AS alias_1 INDEXED BY index_1",
            expected_statement,
        );

        let expected_statement = delete_statement(QualifiedTableName {
            table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            alias: Some("alias_1".to_string()),
            indexed_type: Some(IndexedType::NotIndexed),
        });

        run_sunny_day_test(
            "DELETE FROM schema_1.table_1 AS alias_1 NOT INDEXED",
            expected_statement,
        );
    }

    #[test]
    fn test_parse_delete_statement_with_where_clause() {
        let expected_statement = delete_statement_with_where_clause(
            QualifiedTableName::from(Identifier::Single("table_1".to_string())),
            numeric_literal_expression("1"),
        );
        run_sunny_day_test("DELETE FROM table_1 WHERE 1", expected_statement);
    }

    #[test]
    fn test_parse_delete_statement_with_where_clause_with_column_expression() {
        let expected_statement = delete_statement_with_where_clause(
            QualifiedTableName::from(Identifier::Single("table_1".to_string())),
            binary_op_expression(
                BinaryOp::Equals,
                identifier_expression(&["column_1"]),
                string_literal_expression("'abc'"),
            ),
        );
        run_sunny_day_test(
            "DELETE FROM table_1 WHERE column_1 = 'abc'",
            expected_statement,
        );
    }

    #[test]
    fn test_parse_delete_statement_with_returning_clause() {
        let expected_statement = delete_statement_with_returning_clause(
            QualifiedTableName::from(Identifier::Single("table_1".to_string())),
            vec![ReturningClause::Wildcard],
        );

        run_sunny_day_test("DELETE FROM table_1 RETURNING *", expected_statement);
    }

    #[test]
    fn test_parse_delete_statement_with_returning_clauses() {
        let expected_statement = delete_statement_with_returning_clause(
            QualifiedTableName::from(Identifier::Single("table_1".to_string())),
            vec![
                ReturningClause::Wildcard,
                ReturningClause::Expr(numeric_literal_expression("1")),
                ReturningClause::ExprWithAlias(
                    identifier_expression(&["column_1"]),
                    "alias_1".to_string(),
                ),
            ],
        );

        run_sunny_day_test(
            "DELETE FROM table_1 RETURNING *, 1, column_1 AS alias_1",
            expected_statement,
        );
    }

    #[test]
    fn test_parse_delete_statement_with_order_by_clause() {
        let expected_statement = delete_statement_with_order_by_clause(
            QualifiedTableName::from(Identifier::Single("table_1".to_string())),
            vec![
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
            ],
        );
        run_sunny_day_test(
            "DELETE FROM table_1 ORDER BY column_1 ASC, column_2 COLLATE binary NULLS LAST",
            expected_statement,
        );
    }

    #[test]
    fn test_parse_delete_statement_with_limit_clause() {
        let expected_statement = delete_statement_with_limit_clause(
            QualifiedTableName::from(Identifier::Single("table_1".to_string())),
            LimitClause {
                limit: Box::new(numeric_literal_expression("10")),
                offset: None,
                additional_limit: None,
            },
        );
        run_sunny_day_test("DELETE FROM table_1 LIMIT 10", expected_statement);

        let expected_statement = delete_statement_with_limit_clause(
            QualifiedTableName::from(Identifier::Single("table_1".to_string())),
            LimitClause {
                limit: Box::new(numeric_literal_expression("10")),
                offset: Some(Box::new(numeric_literal_expression("4"))),
                additional_limit: None,
            },
        );
        run_sunny_day_test("DELETE FROM table_1 LIMIT 10 OFFSET 4", expected_statement);

        let expected_statement = delete_statement_with_limit_clause(
            QualifiedTableName::from(Identifier::Single("table_1".to_string())),
            LimitClause {
                limit: Box::new(numeric_literal_expression("10")),
                offset: None,
                additional_limit: Some(Box::new(numeric_literal_expression("40"))),
            },
        );
        run_sunny_day_test("DELETE FROM table_1 LIMIT 10, 40", expected_statement);
    }

    #[test]
    fn test_parse_delete_statement_with_all_clauses() {
        let expected_statement = Statement::Delete(DeleteStatement {
            with_cte: None,
            table_name: QualifiedTableName {
                table_id: Identifier::Single("table_1".to_string()),
                alias: Some("alias_1".to_string()),
                indexed_type: Some(IndexedType::Indexed("index_1".to_string())),
            },
            where_clause: Some(Box::new(binary_op_expression(
                BinaryOp::Equals,
                identifier_expression(&["column_1"]),
                string_literal_expression("'abc'"),
            ))),
            returning_clause: vec![
                ReturningClause::Wildcard,
                ReturningClause::Expr(numeric_literal_expression("1")),
                ReturningClause::ExprWithAlias(
                    identifier_expression(&["column_1"]),
                    "alias_1".to_string(),
                ),
            ],
            order_by: None,
            limit: None,
        });

        run_sunny_day_test(
            "DELETE FROM table_1 AS alias_1 INDEXED BY index_1 WHERE column_1 = 'abc' RETURNING *, 1, column_1 AS alias_1",
            expected_statement,
        );
    }
}

#[cfg(test)]
mod test_delete_statements_with_cte {
    use super::super::cte::test_utils::cte_expression;
    use super::test_utils::delete_statement_with_cte_clause;
    use crate::parser::select::test_utils::select_from;
    use crate::parser::test_utils::*;
    use crate::{FromClause, Identifier, QualifiedTableName};

    #[test]
    fn test_delete_with_cte() {
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
            expected_statement,
        );
    }

    #[test]
    fn test_delete_with_multiple_ctes() {
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
            expected_statement,
        );
    }
}
