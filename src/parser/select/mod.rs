mod column;
mod from;
mod values;

use crate::ast::{
    DistinctType, Expression, NamedWindowDefinition, UnionStatement, UnionStatementType,
};

use crate::{Keyword, SelectBody, TokenType};

use super::expression::ExpressionParser;
use super::window::WindowDefinitionParser;
use super::Parser;
use crate::ast::{Select, SelectStatement};
use crate::parser::errors::ParsingError;

pub use column::SelectColumnsParser;
pub use from::SelectFromParser;
use values::ValuesStatementParser;

/// Trait for parsing SELECT statements
/// The SELECT statement documentation can be found here:
/// https://www.sqlite.org/lang_select.html
pub trait SelectStatementParser {
    fn parse_select_statement(&mut self) -> Result<SelectStatement, ParsingError>;

    fn parse_select_statement_core(&mut self) -> Result<SelectStatement, ParsingError>;

    fn parse_distinct_type(&mut self) -> Result<DistinctType, ParsingError>;

    fn parse_where_clause(&mut self) -> Result<Option<Box<Expression>>, ParsingError>;

    fn parse_group_by_clause(&mut self) -> Result<Option<Vec<Expression>>, ParsingError>;

    fn parse_having_clause(&mut self) -> Result<Option<Box<Expression>>, ParsingError>;

    fn parse_window_clause(&mut self) -> Result<Option<Vec<NamedWindowDefinition>>, ParsingError>;

    fn parse_named_window_definition(&mut self) -> Result<NamedWindowDefinition, ParsingError>;

    fn parse_union_clause(&mut self) -> Result<Option<UnionStatementType>, ParsingError>;
}

impl SelectStatementParser for Parser<'_> {
    fn parse_select_statement(&mut self) -> Result<SelectStatement, ParsingError> {
        if let Ok(Keyword::Values) = self.peek_as_keyword() {
            return Ok(SelectStatement {
                with_cte: None,
                select: SelectBody::Values(self.parse_values_statement()?),
                order_by: None,
                limit: None,
            });
        }

        let select_statement = self.parse_select_statement_core()?;

        let mut select_statement1 = if let Some(union_type) = self.parse_union_clause()? {
            let left = Box::new(select_statement);
            let right = Box::new(self.parse_select_statement()?);
            SelectStatement {
                with_cte: None,
                select: SelectBody::Union(UnionStatement {
                    union_type,
                    left,
                    right,
                }),
                order_by: None,
                limit: None,
            }
        } else {
            select_statement
        };

        select_statement1.order_by = self.parse_order_by_clause()?;
        select_statement1.limit = self.parse_limit_clause()?;

        Ok(select_statement1)
    }

    fn parse_select_statement_core(&mut self) -> Result<SelectStatement, ParsingError> {
        // Consume the SELECT keyword
        self.consume_as_keyword(Keyword::Select)?;

        Ok(SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: self.parse_distinct_type()?,
                columns: self.parse_select_columns()?,
                from: self.parse_from_clause()?,
                where_clause: self.parse_where_clause()?,
                group_by: self.parse_group_by_clause()?,
                having: self.parse_having_clause()?,
                window: self.parse_window_clause()?,
            }),
            order_by: None,
            limit: None,
        })
    }

    fn parse_distinct_type(&mut self) -> Result<DistinctType, ParsingError> {
        if self.consume_as_keyword(Keyword::Distinct).is_ok() {
            Ok(DistinctType::Distinct)
        } else if self.consume_as_keyword(Keyword::All).is_ok() {
            Ok(DistinctType::All)
        } else {
            Ok(DistinctType::None)
        }
    }

    fn parse_where_clause(&mut self) -> Result<Option<Box<Expression>>, ParsingError> {
        if self.consume_as_keyword(Keyword::Where).is_ok() {
            Ok(Some(Box::new(self.parse_expression()?)))
        } else {
            Ok(None)
        }
    }

    fn parse_group_by_clause(&mut self) -> Result<Option<Vec<Expression>>, ParsingError> {
        if self.consume_as_keyword(Keyword::Group).is_ok() {
            self.consume_as_keyword(Keyword::By)?;
            let mut expressions = Vec::new();
            loop {
                expressions.push(self.parse_expression()?);
                if self.consume_as(TokenType::Comma).is_err() {
                    break;
                }
            }
            Ok(Some(expressions))
        } else {
            Ok(None)
        }
    }

    fn parse_having_clause(&mut self) -> Result<Option<Box<Expression>>, ParsingError> {
        if self.consume_as_keyword(Keyword::Having).is_ok() {
            Ok(Some(Box::new(self.parse_expression()?)))
        } else {
            Ok(None)
        }
    }

    fn parse_window_clause(&mut self) -> Result<Option<Vec<NamedWindowDefinition>>, ParsingError> {
        if self.consume_as_keyword(Keyword::Window).is_ok() {
            let mut windows = Vec::new();
            loop {
                windows.push(self.parse_named_window_definition()?);
                if self.consume_as(TokenType::Comma).is_err() {
                    break;
                }
            }
            Ok(Some(windows))
        } else {
            Ok(None)
        }
    }

    fn parse_named_window_definition(&mut self) -> Result<NamedWindowDefinition, ParsingError> {
        let window_name = self.consume_as_id()?;
        self.consume_as_keyword(Keyword::As)?;
        let window_definition = self.parse_window_definition()?;
        Ok(NamedWindowDefinition {
            window_name,
            window_definition,
        })
    }

    fn parse_union_clause(&mut self) -> Result<Option<UnionStatementType>, ParsingError> {
        if self.consume_as_keyword(Keyword::Except).is_ok() {
            Ok(Some(UnionStatementType::Except))
        } else if self.consume_as_keyword(Keyword::Intersect).is_ok() {
            Ok(Some(UnionStatementType::Intersect))
        } else if self.consume_as_keyword(Keyword::Union).is_ok() {
            if self.consume_as_keyword(Keyword::All).is_ok() {
                Ok(Some(UnionStatementType::UnionAll))
            } else {
                Ok(Some(UnionStatementType::Union))
            }
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{
        DistinctType, Expression, FromClause, Identifier, QualifiedTableName, Select, SelectBody,
        SelectItem, SelectStatement, Statement,
    };

    impl From<SelectStatement> for Statement {
        fn from(val: SelectStatement) -> Self {
            Statement::Select(val)
        }
    }

    impl From<Select> for Statement {
        fn from(val: Select) -> Self {
            Statement::Select(SelectStatement {
                with_cte: None,
                select: SelectBody::Select(val),
                order_by: None,
                limit: None,
            })
        }
    }

    pub fn select() -> Select {
        Select {
            distinct_type: DistinctType::None,
            columns: vec![SelectItem::Expression(Expression::Identifier(
                Identifier::Wildcard,
            ))],
            ..Default::default()
        }
    }

    pub fn select_stmt() -> SelectStatement {
        select_from(FromClause::Table(QualifiedTableName {
            table_id: Identifier::Single("table_name1".to_string()),
            alias: None,
            indexed_type: None,
        }))
    }

    pub fn select_columns(columns: Vec<SelectItem>) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                columns,
                ..Default::default()
            }),
            order_by: None,
            limit: None,
        }
    }

    pub fn select_expr(expression: Expression) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(expression)],
                from: None,
                where_clause: None,
                group_by: None,
                having: None,
                window: None,
            }),
            order_by: None,
            limit: None,
        }
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

    pub fn select_star_from(table_name: Identifier) -> SelectStatement {
        select_from(FromClause::Table(QualifiedTableName::from(table_name)))
    }
}

#[cfg(test)]
mod where_clause_tests {
    use super::test_utils::{select, select_star_from};
    use crate::expression::test_utils::{binary_op, expr_list, identifier_expr, numeric_expr};
    use crate::parser::test_utils::*;
    use crate::{BinaryMatchingExpression, BinaryOp, Expression, Identifier, InExpression};

    #[test]
    fn select_where_clause() {
        let mut expected_statement = select();
        expected_statement.where_clause = Some(Box::new(binary_op(
            BinaryOp::Equals,
            numeric_expr("1"),
            numeric_expr("1"),
        )));
        run_sunny_day_test("SELECT * WHERE 1 = 1", expected_statement.into());
    }

    #[test]
    fn select_where_clause_with_binary_ops() {
        use BinaryOp::*;

        let binary_ops = vec![
            Plus,
            Minus,
            Mul,
            Div,
            Remainder,
            GreaterThan,
            GreaterThanOrEquals,
            LessThan,
            LessThanOrEquals,
            Equals,
            EqualsEquals,
            NotEquals,
            Concat,
            BitAnd,
            BitOr,
            LeftShift,
            RightShift,
        ];

        for op in binary_ops {
            let mut expected_statement = select();
            expected_statement.where_clause = Some(Box::new(binary_op(
                op.clone(),
                identifier_expr(&["col1"]),
                identifier_expr(&["col2"]),
            )));
            run_sunny_day_test(
                &format!("SELECT * WHERE col1 {} col2", op),
                expected_statement.into(),
            );
        }
    }

    #[test]
    fn select_where_clause_with_subquery() {
        let subquery = select_star_from(Identifier::Single("table_2".to_string()));

        let expression = Expression::BinaryMatchingExpression(
            Box::new(identifier_expr(&["col1"])),
            BinaryMatchingExpression::Not(Box::new(BinaryMatchingExpression::In(
                InExpression::Select(subquery),
            ))),
        );
        let mut expected_statement = select();
        expected_statement.where_clause = Some(Box::new(expression));
        run_sunny_day_test(
            "SELECT * WHERE col1 NOT IN (SELECT * FROM table_2)",
            expected_statement.into(),
        );
    }

    #[test]
    fn select_where_clause_with_parentheses() {
        let mut expected_statement = select();
        expected_statement.where_clause = Some(Box::new(binary_op(
            BinaryOp::And,
            expr_list(vec![binary_op(
                BinaryOp::Or,
                identifier_expr(&["a"]),
                identifier_expr(&["b"]),
            )]),
            identifier_expr(&["c"]),
        )));

        run_sunny_day_test("SELECT * WHERE (a OR b) AND c", expected_statement.into());
    }
}

#[cfg(test)]
mod group_by_clause_tests {
    use crate::expression::test_utils::{binary_op, identifier_expr};
    use crate::parser::test_utils::*;
    use crate::BinaryOp;

    use super::test_utils::select;

    #[test]
    fn group_by_clause() {
        let mut expected_statement = select();
        expected_statement.group_by = Some(vec![identifier_expr(&["col1"])]);

        run_sunny_day_test("SELECT * GROUP BY col1", expected_statement.into());
    }

    #[test]
    fn group_by_clause_with_multiple_columns() {
        let mut expected_statement = select();
        expected_statement.group_by =
            Some(vec![identifier_expr(&["col1"]), identifier_expr(&["col2"])]);

        run_sunny_day_test("SELECT * GROUP BY col1, col2", expected_statement.into());
    }

    #[test]
    fn group_by_clause_with_expressions() {
        let mut expected_statement = select();
        expected_statement.group_by = Some(vec![
            identifier_expr(&["col1"]),
            binary_op(
                BinaryOp::Plus,
                identifier_expr(&["col2"]),
                identifier_expr(&["col3"]),
            ),
        ]);

        run_sunny_day_test(
            "SELECT * GROUP BY col1, col2 + col3",
            expected_statement.into(),
        );
    }
}

#[cfg(test)]
mod having_clause_tests {
    use super::test_utils::select;
    use crate::expression::test_utils::{binary_op, identifier_expr, numeric_expr};
    use crate::parser::test_utils::*;
    use crate::BinaryOp;

    #[test]
    fn having_clause() {
        let mut expected_statement = select();
        expected_statement.having = Some(Box::new(binary_op(
            BinaryOp::GreaterThan,
            identifier_expr(&["col1"]),
            numeric_expr("1"),
        )));

        run_sunny_day_test("SELECT * HAVING col1 > 1", expected_statement.into());
    }
}

#[cfg(test)]
mod window_clause_tests {
    use super::test_utils::select;
    use crate::expression::test_utils::{collate_expr, identifier_expr, numeric_expr};
    use crate::parser::test_utils::*;
    use crate::{
        BetweenFrameSpec, BetweenFrameSpecType, FrameSpec, FrameSpecExclude, FrameSpecType,
        FrameType, NamedWindowDefinition, NullsOrdering, Ordering, OrderingTerm, WindowDefinition,
    };

    #[test]
    fn single_window_clause() {
        let mut expected_statement = select();
        expected_statement.window = Some(vec![NamedWindowDefinition {
            window_name: "window_1".to_string(),
            window_definition: WindowDefinition::default(),
        }]);

        run_sunny_day_test("SELECT * WINDOW window_1 as ()", expected_statement.into());
    }

    #[test]
    fn single_complex_window_clause() {
        let mut expected_statement = select();
        expected_statement.window = Some(vec![NamedWindowDefinition {
            window_name: "window_1".to_string(),
            window_definition: WindowDefinition {
                window_name: Some("base_window_name".to_string()),
                partition_by: Some(vec![identifier_expr(&["col1"]), identifier_expr(&["col2"])]),
                order_by: Some(vec![
                    OrderingTerm {
                        expression: Box::new(identifier_expr(&["col3"])),
                        ordering: None,
                        nulls_ordering: None,
                    },
                    OrderingTerm {
                        expression: Box::new(collate_expr(
                            identifier_expr(&["col4"]),
                            "binary".to_string(),
                        )),
                        ordering: Some(Ordering::Asc),
                        nulls_ordering: Some(NullsOrdering::Last),
                    },
                ]),
                frame_spec: Some(FrameSpec {
                    frame_type: FrameType::Range,
                    frame_spec_type: FrameSpecType::Between(BetweenFrameSpec {
                        start: BetweenFrameSpecType::Preceding(Box::new(numeric_expr("1"))),
                        end: BetweenFrameSpecType::Following(Box::new(numeric_expr("2"))),
                    }),
                    exclude: Some(FrameSpecExclude::CurrentRow),
                }),
            },
        }]);

        run_sunny_day_test(
            "SELECT * 
                    WINDOW window_1 as (
                        base_window_name
                        PARTITION BY col1, col2
                        ORDER BY col3, col4 COLLATE binary ASC NULLS LAST
                        RANGE BETWEEN 1 PRECEDING AND 2 FOLLOWING
                        EXCLUDE CURRENT ROW
                    )",
            expected_statement.into(),
        );
    }

    #[test]
    fn multiple_window_clauses() {
        let mut expected_statement = select();
        expected_statement.window = Some(vec![
            NamedWindowDefinition {
                window_name: "window_1".to_string(),
                window_definition: WindowDefinition::default(),
            },
            NamedWindowDefinition {
                window_name: "window_2".to_string(),
                window_definition: WindowDefinition::default(),
            },
            NamedWindowDefinition {
                window_name: "window_3".to_string(),
                window_definition: WindowDefinition::default(),
            },
        ]);

        run_sunny_day_test(
            "SELECT * WINDOW window_1 as (), window_2 as (), window_3 as ()",
            expected_statement.into(),
        );
    }
}

#[cfg(test)]
mod union_clause_tests {
    use super::test_utils::select_stmt;
    use crate::parser::test_utils::*;
    use crate::{SelectBody, SelectStatement, UnionStatement, UnionStatementType};

    #[test]
    fn union_clauses() {
        let union_types = vec![
            UnionStatementType::Union,
            UnionStatementType::UnionAll,
            UnionStatementType::Intersect,
            UnionStatementType::Except,
        ];

        for union_type in union_types {
            let left_statement = select_stmt();
            let right_statement = select_stmt();

            let expected_statement = SelectStatement {
                with_cte: None,
                select: SelectBody::Union(UnionStatement {
                    union_type: union_type.clone(),
                    left: Box::new(left_statement),
                    right: Box::new(right_statement),
                }),
                order_by: None,
                limit: None,
            };

            run_sunny_day_test(
                &format!(
                    "SELECT * FROM table_name1 {} SELECT * FROM table_name1",
                    union_type
                ),
                expected_statement.into(),
            );
        }
    }
}

#[cfg(test)]
mod order_by_clause_tests {
    use super::test_utils::select_stmt;
    use crate::expression::test_utils::{collate_expr, identifier_expr};
    use crate::parser::test_utils::*;
    use crate::{NullsOrdering, Ordering, OrderingTerm, Statement};

    #[test]
    fn order_by_clause() {
        let mut expected_statement = select_stmt();
        expected_statement.order_by = Some(vec![OrderingTerm {
            expression: Box::new(identifier_expr(&["col1"])),
            ordering: Some(Ordering::Asc),
            nulls_ordering: None,
        }]);

        run_sunny_day_test(
            "SELECT * FROM table_name1 ORDER BY col1 ASC",
            expected_statement.into(),
        );
    }

    #[test]
    fn order_by_clause_with_multiple_columns() {
        let mut expected_statement = select_stmt();
        expected_statement.order_by = Some(vec![
            OrderingTerm {
                expression: Box::new(identifier_expr(&["col1"])),
                ordering: None,
                nulls_ordering: None,
            },
            OrderingTerm {
                expression: Box::new(identifier_expr(&["col2"])),
                ordering: Some(Ordering::Desc),
                nulls_ordering: None,
            },
        ]);

        run_sunny_day_test(
            "SELECT * FROM table_name1 ORDER BY col1, col2 DESC",
            expected_statement.into(),
        );
    }

    #[test]
    fn order_by_clause_with_nulls_ordering() {
        let mut expected_statement = select_stmt();
        expected_statement.order_by = Some(vec![OrderingTerm {
            expression: Box::new(identifier_expr(&["col1"])),
            ordering: Some(Ordering::Asc),
            nulls_ordering: Some(NullsOrdering::Last),
        }]);

        run_sunny_day_test(
            "SELECT * FROM table_name1 ORDER BY col1 ASC NULLS LAST",
            expected_statement.into(),
        );
    }

    #[test]
    fn order_by_clause_with_collation_and_nulls_ordering() {
        let mut expected_statement = select_stmt();
        expected_statement.order_by = Some(vec![
            OrderingTerm {
                expression: Box::new(collate_expr(
                    identifier_expr(&["col1"]),
                    "binary".to_string(),
                )),
                ordering: Some(Ordering::Asc),
                nulls_ordering: Some(NullsOrdering::Last),
            },
            OrderingTerm {
                expression: Box::new(collate_expr(identifier_expr(&["col2"]), "utf8".to_string())),
                ordering: Some(Ordering::Desc),
                nulls_ordering: Some(NullsOrdering::First),
            },
        ]);

        run_sunny_day_test(
            "SELECT * FROM table_name1
                    ORDER BY
                        col1 COLLATE binary ASC NULLS LAST,
                        col2 COLLATE utf8 DESC NULLS FIRST",
            Statement::Select(expected_statement),
        );
    }
}

#[cfg(test)]
mod limit_clause_tests {
    use super::test_utils::select_stmt;
    use crate::expression::test_utils::{binary_op, numeric_expr};
    use crate::parser::test_utils::*;
    use crate::{BinaryOp, LimitClause, Statement};

    #[test]
    fn limit_clause() {
        let mut expected_statement = select_stmt();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(numeric_expr("1")),
            offset: None,
            additional_limit: None,
        });

        run_sunny_day_test(
            "SELECT * FROM table_name1 LIMIT 1",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn limit_clause_basic_expression() {
        let mut expected_statement = select_stmt();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(binary_op(
                BinaryOp::Plus,
                numeric_expr("1"),
                numeric_expr("2"),
            )),
            offset: None,
            additional_limit: None,
        });

        run_sunny_day_test(
            "SELECT * FROM table_name1 LIMIT 1 + 2",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn limit_clause_with_offset() {
        let mut expected_statement = select_stmt();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(numeric_expr("1")),
            offset: Some(Box::new(numeric_expr("2"))),
            additional_limit: None,
        });

        run_sunny_day_test(
            "SELECT * FROM table_name1 LIMIT 1 OFFSET 2",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn limit_clause_with_additional_limit() {
        let mut expected_statement = select_stmt();
        expected_statement.limit = Some(LimitClause {
            limit: Box::new(numeric_expr("1")),
            offset: None,
            additional_limit: Some(Box::new(numeric_expr("2"))),
        });

        run_sunny_day_test(
            "SELECT * FROM table_name1 LIMIT 1, 2",
            Statement::Select(expected_statement),
        );
    }
}

#[cfg(test)]
mod with_cte_tests {
    use super::test_utils::{select_from, select_stmt};
    use crate::parser::cte::test_utils::cte_expression;
    use crate::parser::test_utils::*;
    use crate::{CteExpression, FromClause, Identifier, QualifiedTableName, WithCteStatement};

    #[test]
    fn cte_clause() {
        let mut expected_statement = select_stmt();
        expected_statement.with_cte = Some(WithCteStatement {
            recursive: true,
            cte_expressions: vec![CteExpression {
                name: Identifier::Single("cte_1".to_string()),
                column_names: vec![],
                materialized: None,
                select: select_from(FromClause::Table(QualifiedTableName::from(
                    Identifier::Single("cte_table".to_string()),
                ))),
            }],
        });

        run_sunny_day_test(
            "WITH RECURSIVE cte_1 AS (SELECT * FROM cte_table) SELECT * FROM table_name1",
            expected_statement.into(),
        );
    }

    #[test]
    fn with_multiple_ctes() {
        let mut expected_statement = select_stmt();

        expected_statement.with_cte = Some(WithCteStatement {
            recursive: true,
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
                        Identifier::Single("cte_table2".to_string()),
                    ))),
                ),
            ],
        });

        run_sunny_day_test(
            "WITH RECURSIVE 
                    cte_1 AS (SELECT * FROM cte_table1), 
                    cte_2 AS (SELECT * FROM cte_table2) 
                  SELECT * FROM table_name1",
            expected_statement.into(),
        );
    }
}
