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
pub use from::*;
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

impl<'a> SelectStatementParser for Parser<'a> {
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

        let select_statement1 = if let Some(union_type) = self.parse_union_clause()? {
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

    impl Into<Statement> for SelectStatement {
        fn into(self) -> Statement {
            Statement::Select(self)
        }
    }

    impl Into<Statement> for Select {
        fn into(self) -> Statement {
            Statement::Select(SelectStatement {
                with_cte: None,
                select: SelectBody::Select(self),
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
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
                from: Some(FromClause::Table(QualifiedTableName {
                    table_id: Identifier::Single("table_name1".to_string()),
                    alias: None,
                    indexed_type: None,
                })),
                ..Default::default()
            }),
            order_by: None,
            limit: None,
        }
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
mod select_where_clause_tests {
    use super::test_utils::{select, select_star_from};
    use crate::expression::test_utils::{binary_op, identifier_expr, numeric_expr};
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
}

// #[cfg(test)]
// mod test_select_group_by_clause {
//     use super::test_utils::select_statement_with_group_by_clause;
//     use crate::expression::test_utils::{binary_op_expression, identifier_expression};
//     use crate::parser::test_utils::*;
//     use crate::{BinaryOp, Statement};

//     #[test]
//     fn test_select_group_by_clause() {
//         let expected_statement =
//             select_statement_with_group_by_clause(vec![identifier_expression(&["col1"])]);
//         run_sunny_day_test(
//             "SELECT * FROM table_1 GROUP BY col1",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_group_by_clause_with_multiple_columns() {
//         let expected_statement = select_statement_with_group_by_clause(vec![
//             identifier_expression(&["col1"]),
//             identifier_expression(&["col2"]),
//         ]);

//         run_sunny_day_test(
//             "SELECT * FROM table_1 GROUP BY col1, col2",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_group_by_clause_with_expressions() {
//         let expected_statement = select_statement_with_group_by_clause(vec![
//             identifier_expression(&["col1"]),
//             binary_op_expression(
//                 BinaryOp::Plus,
//                 identifier_expression(&["col2"]),
//                 identifier_expression(&["col3"]),
//             ),
//         ]);

//         run_sunny_day_test(
//             "SELECT * FROM table_1 GROUP BY col1, col2 + col3",
//             Statement::Select(expected_statement),
//         );
//     }
// }

// #[cfg(test)]
// mod test_select_having_clause {
//     use super::test_utils::select_statement_with_having_clause;
//     use crate::expression::test_utils::{
//         binary_op_expression, identifier_expression, numeric_literal_expression,
//     };
//     use crate::parser::test_utils::*;
//     use crate::{BinaryOp, Statement};

//     #[test]
//     fn test_select_having_clause() {
//         let expected_statement = select_statement_with_having_clause(binary_op_expression(
//             BinaryOp::GreaterThan,
//             identifier_expression(&["col1"]),
//             numeric_literal_expression("1"),
//         ));

//         run_sunny_day_test(
//             "SELECT * FROM table_1 HAVING col1 > 1",
//             Statement::Select(expected_statement),
//         );
//     }
// }

// #[cfg(test)]
// mod test_select_window_clause {
//     use super::test_utils::select_statement_with_window_clause;
//     use crate::expression::test_utils::{
//         collate_expression, identifier_expression, numeric_literal_expression,
//     };
//     use crate::parser::test_utils::*;
//     use crate::{
//         BetweenFrameSpec, BetweenFrameSpecType, FrameSpec, FrameSpecExclude, FrameSpecType,
//         FrameType, NamedWindowDefinition, NullsOrdering, Ordering, OrderingTerm, Statement,
//         WindowDefinition,
//     };

//     #[test]
//     fn test_select_with_single_window_clause() {
//         let expected_statement = select_statement_with_window_clause(vec![NamedWindowDefinition {
//             window_name: "window_1".to_string(),
//             window_definition: WindowDefinition::default(),
//         }]);
//         run_sunny_day_test(
//             "SELECT * FROM table_1 WINDOW window_1 as ()",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_with_single_window_clause_complex() {
//         let expected_statement = select_statement_with_window_clause(vec![NamedWindowDefinition {
//             window_name: "window_1".to_string(),
//             window_definition: WindowDefinition {
//                 base_window_name: Some("base_window_name".to_string()),
//                 partition_by: Some(vec![
//                     identifier_expression(&["col1"]),
//                     identifier_expression(&["col2"]),
//                 ]),
//                 order_by: Some(vec![
//                     OrderingTerm {
//                         expression: Box::new(identifier_expression(&["col3"])),
//                         ordering: None,
//                         nulls_ordering: None,
//                     },
//                     OrderingTerm {
//                         expression: Box::new(collate_expression(
//                             identifier_expression(&["col4"]),
//                             "binary".to_string(),
//                         )),
//                         ordering: Some(Ordering::Asc),
//                         nulls_ordering: Some(NullsOrdering::Last),
//                     },
//                 ]),
//                 frame_spec: Some(FrameSpec {
//                     frame_type: FrameType::Range,
//                     frame_spec_type: FrameSpecType::Between(BetweenFrameSpec {
//                         start: BetweenFrameSpecType::Preceding(Box::new(
//                             numeric_literal_expression("1"),
//                         )),
//                         end: BetweenFrameSpecType::Following(Box::new(numeric_literal_expression(
//                             "2",
//                         ))),
//                     }),
//                     exclude: Some(FrameSpecExclude::CurrentRow),
//                 }),
//             },
//         }]);
//         run_sunny_day_test(
//             "SELECT * FROM table_1
//                     WINDOW window_1 as (
//                         base_window_name
//                         PARTITION BY col1, col2
//                         ORDER BY col3, col4 COLLATE binary ASC NULLS LAST
//                         RANGE BETWEEN 1 PRECEDING AND 2 FOLLOWING
//                         EXCLUDE CURRENT ROW
//                     )",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_with_multiple_window_clause() {
//         let expected_statement = select_statement_with_window_clause(vec![
//             NamedWindowDefinition {
//                 window_name: "window_1".to_string(),
//                 window_definition: WindowDefinition::default(),
//             },
//             NamedWindowDefinition {
//                 window_name: "window_2".to_string(),
//                 window_definition: WindowDefinition::default(),
//             },
//             NamedWindowDefinition {
//                 window_name: "window_3".to_string(),
//                 window_definition: WindowDefinition::default(),
//             },
//         ]);

//         run_sunny_day_test(
//             "SELECT * FROM table_1 WINDOW window_1 as (), window_2 as (), window_3 as ()",
//             Statement::Select(expected_statement),
//         );
//     }
// }

// #[cfg(test)]
// mod test_select_union_clause {
//     use super::test_utils::{select_from, select_statement_with_union_clause};
//     use crate::parser::test_utils::*;
//     use crate::{FromClause, Identifier, QualifiedTableName, Statement, UnionStatementType};

//     #[test]
//     fn test_select_union_clause() {
//         let union_types = vec![
//             UnionStatementType::Union,
//             UnionStatementType::UnionAll,
//             UnionStatementType::Intersect,
//             UnionStatementType::Except,
//         ];

//         let left = select_from(FromClause::Table(QualifiedTableName::from(
//             Identifier::Single("table_1".to_string()),
//         )));

//         let right = select_from(FromClause::Table(QualifiedTableName::from(
//             Identifier::Single("table_2".to_string()),
//         )));

//         for union_type in union_types {
//             let expected_statement =
//                 select_statement_with_union_clause(union_type.clone(), left.clone(), right.clone());

//             run_sunny_day_test(
//                 &format!("SELECT * FROM table_1 {} SELECT * FROM table_2", union_type),
//                 Statement::Select(expected_statement),
//             );
//         }
//     }
// }

// #[cfg(test)]
// mod test_select_order_by_clause {
//     use super::test_utils::select_statement_with_order_by_clause;
//     use crate::expression::test_utils::{collate_expression, identifier_expression};
//     use crate::parser::test_utils::*;
//     use crate::{NullsOrdering, Ordering, OrderingTerm, Statement};

//     #[test]
//     fn test_select_order_by_clause() {
//         let expected_statement = select_statement_with_order_by_clause(vec![OrderingTerm {
//             expression: Box::new(identifier_expression(&["col1"])),
//             ordering: Some(Ordering::Asc),
//             nulls_ordering: None,
//         }]);

//         run_sunny_day_test(
//             "SELECT * FROM table_1 ORDER BY col1 ASC",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_order_by_clause_with_multiple_columns() {
//         let expected_statement = select_statement_with_order_by_clause(vec![
//             OrderingTerm {
//                 expression: Box::new(identifier_expression(&["col1"])),
//                 ordering: None,
//                 nulls_ordering: None,
//             },
//             OrderingTerm {
//                 expression: Box::new(identifier_expression(&["col2"])),
//                 ordering: Some(Ordering::Desc),
//                 nulls_ordering: None,
//             },
//         ]);

//         run_sunny_day_test(
//             "SELECT * FROM table_1 ORDER BY col1, col2 DESC",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_order_by_clause_with_nulls_ordering() {
//         let expected_statement = select_statement_with_order_by_clause(vec![OrderingTerm {
//             expression: Box::new(identifier_expression(&["col1"])),
//             ordering: Some(Ordering::Asc),
//             nulls_ordering: Some(NullsOrdering::Last),
//         }]);

//         run_sunny_day_test(
//             "SELECT * FROM table_1 ORDER BY col1 ASC NULLS LAST",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_order_by_clause_with_collation_and_nulls_ordering() {
//         let expected_statement = select_statement_with_order_by_clause(vec![
//             OrderingTerm {
//                 expression: Box::new(collate_expression(
//                     identifier_expression(&["col1"]),
//                     "binary".to_string(),
//                 )),
//                 ordering: Some(Ordering::Asc),
//                 nulls_ordering: Some(NullsOrdering::Last),
//             },
//             OrderingTerm {
//                 expression: Box::new(collate_expression(
//                     identifier_expression(&["col2"]),
//                     "utf8".to_string(),
//                 )),
//                 ordering: Some(Ordering::Desc),
//                 nulls_ordering: Some(NullsOrdering::First),
//             },
//         ]);

//         run_sunny_day_test(
//             "SELECT * FROM table_1
//                     ORDER BY
//                         col1 COLLATE binary ASC NULLS LAST,
//                         col2 COLLATE utf8 DESC NULLS FIRST",
//             Statement::Select(expected_statement),
//         );
//     }
// }

// #[cfg(test)]
// mod test_select_limit_clause {
//     use super::test_utils::select_statement_with_limit_clause;
//     use crate::expression::test_utils::{binary_op_expression, numeric_literal_expression};
//     use crate::parser::test_utils::*;
//     use crate::{BinaryOp, LimitClause, Statement};

//     #[test]
//     fn test_select_limit_clause_basic() {
//         let expected_statement = select_statement_with_limit_clause(LimitClause {
//             limit: Box::new(numeric_literal_expression("1")),
//             offset: None,
//             additional_limit: None,
//         });

//         run_sunny_day_test(
//             "SELECT * FROM table_1 LIMIT 1",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_limit_clause_basic_expression() {
//         let expected_statement = select_statement_with_limit_clause(LimitClause {
//             limit: Box::new(binary_op_expression(
//                 BinaryOp::Plus,
//                 numeric_literal_expression("1"),
//                 numeric_literal_expression("2"),
//             )),
//             offset: None,
//             additional_limit: None,
//         });

//         run_sunny_day_test(
//             "SELECT * FROM table_1 LIMIT 1 + 2",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_limit_clause_with_offset() {
//         let expected_statement = select_statement_with_limit_clause(LimitClause {
//             limit: Box::new(numeric_literal_expression("1")),
//             offset: Some(Box::new(numeric_literal_expression("2"))),
//             additional_limit: None,
//         });

//         run_sunny_day_test(
//             "SELECT * FROM table_1 LIMIT 1 OFFSET 2",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_limit_clause_with_additional_limit() {
//         let expected_statement = select_statement_with_limit_clause(LimitClause {
//             limit: Box::new(numeric_literal_expression("1")),
//             offset: None,
//             additional_limit: Some(Box::new(numeric_literal_expression("2"))),
//         });

//         run_sunny_day_test(
//             "SELECT * FROM table_1 LIMIT 1, 2",
//             Statement::Select(expected_statement),
//         );
//     }
// }

// #[cfg(test)]
// mod test_select_with_cte {
//     use super::super::cte::test_utils::cte_expression;
//     use super::test_utils::{select_from, select_statement_with_cte_clause};
//     use crate::parser::test_utils::*;
//     use crate::{FromClause, Identifier, QualifiedTableName};

//     #[test]
//     fn test_select_cte_clause() {
//         let expected_statement = select_statement_with_cte_clause(
//             true,
//             vec![cte_expression(
//                 Identifier::Single("cte_1".to_string()),
//                 vec![],
//                 None,
//                 select_from(FromClause::Table(QualifiedTableName::from(
//                     Identifier::Single("cte_table".to_string()),
//                 ))),
//             )],
//         );

//         run_sunny_day_test(
//             "WITH RECURSIVE cte_1 AS (SELECT * FROM cte_table) SELECT * FROM table_1",
//             expected_statement,
//         );
//     }

//     #[test]
//     fn test_select_with_multiple_ctes() {
//         let expected_statement = select_statement_with_cte_clause(
//             true,
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
//         );

//         run_sunny_day_test(
//             "WITH RECURSIVE cte_1 AS (SELECT * FROM cte_table1), cte_2 AS (SELECT * FROM cte_table2) SELECT * FROM table_1",
//             expected_statement,
//         );
//     }
// }
