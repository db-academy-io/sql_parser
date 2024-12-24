mod values;

use crate::ast::{
    DistinctType, Expression, FromClause, Identifier, JoinClause, JoinConstraint, JoinTable,
    JoinType, NamedWindowDefinition, QualifiedTableName, SelectFromFunction, SelectFromSubquery,
    UnionStatement, UnionStatementType,
};
use crate::expression::IdentifierParser;

use crate::{Keyword, SelectBody, TokenType};

use super::expression::ExpressionParser;
use super::window::WindowDefinitionParser;
use super::Parser;
use crate::ast::{Select, SelectItem, SelectStatement};
use crate::parser::errors::ParsingError;
pub use values::ValuesStatementParser;

/// Trait for parsing SELECT statements
/// The SELECT statement documentation can be found here:
/// https://www.sqlite.org/lang_select.html
pub trait SelectStatementParser {
    fn parse_select_statement(&mut self) -> Result<SelectStatement, ParsingError>;

    fn parse_select_statement_core(&mut self) -> Result<SelectStatement, ParsingError>;

    fn parse_distinct_type(&mut self) -> Result<DistinctType, ParsingError>;

    fn parse_select_columns(&mut self) -> Result<Vec<SelectItem>, ParsingError>;

    fn parse_select_column(&mut self) -> Result<SelectItem, ParsingError>;

    fn parse_from_clause(&mut self) -> Result<Option<FromClause>, ParsingError>;

    fn parse_from_clause_subquery(&mut self) -> Result<FromClause, ParsingError>;

    fn parse_from_join_clause(&mut self, lhs: FromClause) -> Result<FromClause, ParsingError>;

    fn parse_from_clause_join_type(&mut self) -> Result<JoinType, ParsingError>;

    fn parse_from_clause_join_constraints(
        &mut self,
    ) -> Result<Option<JoinConstraint>, ParsingError>;

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

    fn parse_select_columns(&mut self) -> Result<Vec<SelectItem>, ParsingError> {
        let mut select_items = Vec::new();

        loop {
            let select_item = self.parse_select_column()?;
            select_items.push(select_item);

            if self.consume_as(TokenType::Comma).is_err() {
                break;
            }
        }
        Ok(select_items)
    }

    fn parse_select_column(&mut self) -> Result<SelectItem, ParsingError> {
        if self.consume_as(TokenType::Star).is_ok() {
            return Ok(SelectItem::Expression(Expression::Identifier(
                Identifier::Wildcard,
            )));
        }

        if let Ok(expression) = self.parse_expression() {
            // Consume the AS keyword if it exists
            let _ = self.consume_as_keyword(Keyword::As);
            if let Ok(alias) = self.consume_as_id() {
                return Ok(SelectItem::ExpressionWithAlias(expression, alias));
            }

            return Ok(SelectItem::Expression(expression));
        }

        if let Ok(table_name) = self.peek_as_string() {
            self.consume_token()?;
            self.consume_as(TokenType::Dot)?;
            self.consume_as(TokenType::Star)?;

            return Ok(SelectItem::Expression(Expression::Identifier(
                Identifier::NameWithWildcard(table_name),
            )));
        }

        Err(ParsingError::UnexpectedToken(
            self.peek_token()?.to_string(),
        ))
    }

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
        CteExpression, DistinctType, Expression, FromClause, Identifier, LimitClause,
        NamedWindowDefinition, OrderingTerm, QualifiedTableName, Select, SelectBody, SelectItem,
        SelectStatement, Statement, UnionStatement, UnionStatementType, WithCteStatement,
    };

    pub fn select_statement_with_columns(
        distinct_type: DistinctType,
        columns: Vec<SelectItem>,
    ) -> Select {
        Select {
            distinct_type,
            columns,
            ..Default::default()
        }
    }

    pub fn select_statement() -> SelectStatement {
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

    pub fn select_statement_with_where_clause(where_clause: Expression) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
                from: Some(FromClause::Table(QualifiedTableName {
                    table_id: Identifier::Single("table_1".to_string()),
                    alias: None,
                    indexed_type: None,
                })),
                where_clause: Some(Box::new(where_clause)),
                ..Default::default()
            }),
            order_by: None,
            limit: None,
        }
    }

    pub fn select_statement_with_group_by_clause(group_by: Vec<Expression>) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
                from: Some(FromClause::Table(QualifiedTableName {
                    table_id: Identifier::Single("table_1".to_string()),
                    alias: None,
                    indexed_type: None,
                })),
                where_clause: None,
                group_by: Some(group_by),
                ..Default::default()
            }),
            order_by: None,
            limit: None,
        }
    }

    pub fn select_statement_with_having_clause(having: Expression) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
                from: Some(FromClause::Table(QualifiedTableName {
                    table_id: Identifier::Single("table_1".to_string()),
                    alias: None,
                    indexed_type: None,
                })),
                having: Some(Box::new(having)),
                ..Default::default()
            }),
            order_by: None,
            limit: None,
        }
    }

    pub fn select_statement_with_window_clause(
        windows: Vec<NamedWindowDefinition>,
    ) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
                from: Some(FromClause::Table(QualifiedTableName {
                    table_id: Identifier::Single("table_1".to_string()),
                    alias: None,
                    indexed_type: None,
                })),
                window: Some(windows),
                ..Default::default()
            }),
            order_by: None,
            limit: None,
        }
    }

    pub fn select_statement_with_union_clause(
        union_type: UnionStatementType,
        left: SelectStatement,
        right: SelectStatement,
    ) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Union(UnionStatement {
                union_type,
                left: Box::new(left),
                right: Box::new(right),
            }),
            order_by: None,
            limit: None,
        }
    }

    pub fn select_statement_with_order_by_clause(order_by: Vec<OrderingTerm>) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
                from: Some(FromClause::Table(QualifiedTableName {
                    table_id: Identifier::Single("table_1".to_string()),
                    alias: None,
                    indexed_type: None,
                })),
                where_clause: None,
                group_by: None,
                having: None,
                window: None,
            }),
            order_by: Some(order_by),
            limit: None,
        }
    }

    pub fn select_statement_with_limit_clause(limit: LimitClause) -> SelectStatement {
        SelectStatement {
            with_cte: None,
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
                from: Some(FromClause::Table(QualifiedTableName {
                    table_id: Identifier::Single("table_1".to_string()),
                    alias: None,
                    indexed_type: None,
                })),
                where_clause: None,
                group_by: None,
                having: None,
                window: None,
            }),
            order_by: None,
            limit: Some(limit),
        }
    }

    pub fn select_statement_with_cte_clause(
        recursive: bool,
        expressions: Vec<CteExpression>,
    ) -> Statement {
        Statement::Select(SelectStatement {
            with_cte: Some(WithCteStatement {
                recursive,
                cte_expressions: expressions,
            }),
            select: SelectBody::Select(Select {
                distinct_type: DistinctType::None,
                columns: vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
                from: Some(FromClause::Table(QualifiedTableName {
                    table_id: Identifier::Single("table_1".to_string()),
                    alias: None,
                    indexed_type: None,
                })),
                ..Default::default()
            }),
            order_by: None,
            limit: None,
        })
    }
}

// #[cfg(test)]
// mod test_select_result_columns {
//     use crate::{
//         BinaryOp, DistinctType, Expression, Identifier, SelectItem, SelectStatement, Statement,
//     };

//     use super::test_utils::*;
//     use crate::parser::expression::test_utils::*;
//     use crate::parser::{test_utils::*, ParsingError};

//     #[test]
//     fn test_select_distinct() {
//         run_sunny_day_test(
//             "SELECT DISTINCT column1",
//             Statement::Select(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::Distinct,
//                 vec![SelectItem::Expression(identifier_expression(&["column1"]))],
//             ))),
//         );
//     }

//     #[test]
//     fn test_select_all() {
//         run_sunny_day_test(
//             "SELECT ALL column1",
//             Statement::Select(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::All,
//                 vec![SelectItem::Expression(identifier_expression(&["column1"]))],
//             ))),
//         );
//     }

//     #[test]
//     fn test_select_distinct_all() {
//         run_rainy_day_test(
//             "SELECT DISTINCT ALL column1",
//             ParsingError::UnexpectedToken("All".to_string()),
//         );

//         run_rainy_day_test(
//             "SELECT ALL DISTINCT column1",
//             ParsingError::UnexpectedToken("Distinct".to_string()),
//         );
//     }

//     #[test]
//     fn test_select_statement_parser_with_single_literal_value() {
//         run_sunny_day_test(
//             "SELECT 1",
//             Statement::Select(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::None,
//                 vec![SelectItem::Expression(numeric_literal_expression("1"))],
//             ))),
//         );
//     }

//     #[test]
//     fn test_select_statement_parser_with_single_identifier() {
//         run_sunny_day_test(
//             "SELECT id",
//             Statement::Select(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::None,
//                 vec![SelectItem::Expression(identifier_expression(&["id"]))],
//             ))),
//         );
//     }

//     #[test]
//     fn test_select_statement_parser_with_multiple_literal_values() {
//         run_sunny_day_test(
//             "SELECT 1, 2, 3",
//             Statement::Select(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::None,
//                 vec![
//                     SelectItem::Expression(numeric_literal_expression("1")),
//                     SelectItem::Expression(numeric_literal_expression("2")),
//                     SelectItem::Expression(numeric_literal_expression("3")),
//                 ],
//             ))),
//         );
//     }

//     #[test]
//     fn test_select_statement_parser_with_multiple_identifiers() {
//         run_sunny_day_test(
//             "SELECT id, name, age",
//             Statement::Select(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::None,
//                 vec![
//                     SelectItem::Expression(identifier_expression(&["id"])),
//                     SelectItem::Expression(identifier_expression(&["name"])),
//                     SelectItem::Expression(identifier_expression(&["age"])),
//                 ],
//             ))),
//         );
//     }

//     #[test]
//     fn test_select_statement_parser_with_wildcard() {
//         run_sunny_day_test(
//             "SELECT *",
//             Statement::Select(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::None,
//                 vec![SelectItem::Expression(Expression::Identifier(
//                     Identifier::Wildcard,
//                 ))],
//             ))),
//         );
//     }

//     #[test]
//     fn test_select_statement_parser_with_table_name_and_wildcard() {
//         run_sunny_day_test(
//             "SELECT table_1.*",
//             Statement::Select(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::None,
//                 vec![SelectItem::Expression(Expression::Identifier(
//                     Identifier::NameWithWildcard("table_1".to_string()),
//                 ))],
//             ))),
//         );
//     }

//     #[test]
//     fn test_select_statement_parser_with_table_name_and_column_name() {
//         run_rainy_day_test(
//             "SELECT table_1.column",
//             ParsingError::UnexpectedToken("Column".to_string()),
//         );
//     }

//     #[test]
//     fn test_select_statement_parser_with_alias() {
//         run_sunny_day_test(
//             "SELECT column1 AS alias",
//             Statement::Select(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::None,
//                 vec![SelectItem::ExpressionWithAlias(
//                     identifier_expression(&["column1"]),
//                     "alias".to_string(),
//                 )],
//             ))),
//         );
//     }

//     #[test]
//     fn test_select_statement_parser_with_alias_without_as_keyword() {
//         run_sunny_day_test(
//             "SELECT column1 alias",
//             Statement::Select(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::None,
//                 vec![SelectItem::ExpressionWithAlias(
//                     identifier_expression(&["column1"]),
//                     "alias".to_string(),
//                 )],
//             ))),
//         );
//     }

//     #[test]
//     fn test_select_statement_parser_with_multiple_columns_and_aliases() {
//         run_sunny_day_test(
//             "SELECT column1, column2 AS alias2",
//             Statement::Select(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::None,
//                 vec![
//                     SelectItem::Expression(identifier_expression(&["column1"])),
//                     SelectItem::ExpressionWithAlias(
//                         identifier_expression(&["column2"]),
//                         "alias2".to_string(),
//                     ),
//                 ],
//             ))),
//         );
//     }

//     #[test]
//     fn test_select_statement_parser_with_expression_and_alias() {
//         run_sunny_day_test(
//             "SELECT 1 + col1 as incremented, column2 * 2 - 1 as doubled",
//             Statement::Select(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::None,
//                 vec![
//                     SelectItem::ExpressionWithAlias(
//                         binary_op_expression(
//                             BinaryOp::Plus,
//                             numeric_literal_expression("1"),
//                             identifier_expression(&["col1"]),
//                         ),
//                         "incremented".to_string(),
//                     ),
//                     SelectItem::ExpressionWithAlias(
//                         binary_op_expression(
//                             BinaryOp::Minus,
//                             binary_op_expression(
//                                 BinaryOp::Mul,
//                                 identifier_expression(&["column2"]),
//                                 numeric_literal_expression("2"),
//                             ),
//                             numeric_literal_expression("1"),
//                         ),
//                         "doubled".to_string(),
//                     ),
//                 ],
//             ))),
//         );
//     }
// }

// #[cfg(test)]
// mod test_select_from_table_indexed {
//     use super::test_utils::select_from;
//     use crate::parser::test_utils::*;
//     use crate::{FromClause, Identifier, IndexedType, QualifiedTableName, Statement};

//     #[test]
//     fn test_select_from_table() {
//         let expected_statement = select_from(FromClause::Table(QualifiedTableName::from(
//             Identifier::Single("table_1".to_string()),
//         )));

//         run_sunny_day_test(
//             "SELECT * FROM table_1",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_table_with_schema() {
//         let expected_statement = select_from(FromClause::Table(QualifiedTableName::from(
//             Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
//         )));

//         run_sunny_day_test(
//             "SELECT * FROM schema_1.table_1",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_table_with_alias() {
//         let expected_statement = select_from(FromClause::Table(QualifiedTableName {
//             table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
//             alias: Some("alias".to_string()),
//             indexed_type: None,
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM schema_1.table_1 AS alias",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_table_with_alias_without_as_keyword() {
//         let expected_statement = select_from(FromClause::Table(QualifiedTableName {
//             table_id: Identifier::Single("table_1".to_string()),
//             alias: Some("alias".to_string()),
//             indexed_type: None,
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM table_1 alias",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_table_with_alias_indexed() {
//         let expected_statement = select_from(FromClause::Table(QualifiedTableName {
//             table_id: Identifier::Single("table_1".to_string()),
//             alias: Some("alias".to_string()),
//             indexed_type: Some(IndexedType::Indexed("index_1".to_string())),
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM table_1 alias INDEXED BY index_1",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_table_not_indexed() {
//         let expected_statement = select_from(FromClause::Table(QualifiedTableName {
//             table_id: Identifier::Single("table_1".to_string()),
//             alias: None,
//             indexed_type: Some(IndexedType::NotIndexed),
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM table_1 NOT INDEXED",
//             Statement::Select(expected_statement),
//         );
//     }
// }

// #[cfg(test)]
// mod test_select_from_subquery {
//     use super::test_utils::{select_from, select_statement_with_columns};
//     use crate::parser::test_utils::*;
//     use crate::{
//         DistinctType, Expression, FromClause, Identifier, SelectBody, SelectFromSubquery, SelectItem, SelectStatement, Statement
//     };

//     #[test]
//     fn test_select_from_subquery() {
//         let expected_statement = select_from(FromClause::Subquery(SelectFromSubquery {
//             subquery: Box::new(SelectStatement::Select(select_statement_with_columns(
//                 DistinctType::None,
//                 vec![SelectItem::Expression(Expression::Identifier(
//                     Identifier::Single("col1".to_string()),
//                 ))],
//             ))),
//             alias: None,
//         }));

//         run_sunny_day_test(
//             "SELECT * FROM (SELECT col1)",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_from_subquery_aliased() {
//         let expected_statement = select_from(FromClause::Subquery(SelectFromSubquery {
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
//             "SELECT * FROM (SELECT t.* ) as alias",
//             Statement::Select(expected_statement.clone()),
//         );

//         // without the as keyword
//         run_sunny_day_test(
//             "SELECT * FROM (SELECT t.* ) alias",
//             Statement::Select(expected_statement.clone()),
//         );
//     }
// }

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

// #[cfg(test)]
// mod test_select_where_clause {
//     use super::test_utils::{select_from, select_statement_with_where_clause};
//     use crate::expression::test_utils::{
//         binary_op_expression, identifier_expression, numeric_literal_expression,
//     };
//     use crate::parser::test_utils::*;
//     use crate::{
//         BinaryMatchingExpression, BinaryOp, Expression, FromClause, Identifier, InExpression,
//         QualifiedTableName, Statement,
//     };

//     #[test]
//     fn test_select_where_clause() {
//         let expected_statement = select_statement_with_where_clause(binary_op_expression(
//             BinaryOp::Equals,
//             numeric_literal_expression("1"),
//             numeric_literal_expression("1"),
//         ));
//         run_sunny_day_test(
//             "SELECT * FROM table_1 WHERE 1 = 1",
//             Statement::Select(expected_statement),
//         );
//     }

//     #[test]
//     fn test_select_where_clause_with_binary_ops() {
//         use BinaryOp::*;
//         let binary_ops = vec![
//             Plus,
//             Minus,
//             Mul,
//             Div,
//             Remainder,
//             GreaterThan,
//             GreaterThanOrEquals,
//             LessThan,
//             LessThanOrEquals,
//             Equals,
//             EqualsEquals,
//             NotEquals,
//             Concat,
//             BitAnd,
//             BitOr,
//             LeftShift,
//             RightShift,
//         ];

//         for binary_op in binary_ops {
//             let expected_statement = select_statement_with_where_clause(binary_op_expression(
//                 binary_op.clone(),
//                 identifier_expression(&["col1"]),
//                 identifier_expression(&["col2"]),
//             ));
//             run_sunny_day_test(
//                 &format!("SELECT * FROM table_1 WHERE col1 {} col2", binary_op),
//                 Statement::Select(expected_statement),
//             );
//         }
//     }

//     #[test]
//     fn test_select_where_clause_with_subquery() {
//         let subquery = select_from(FromClause::Table(QualifiedTableName::from(
//             Identifier::Single("table_2".to_string()),
//         )));

//         let expression = Expression::BinaryMatchingExpression(
//             Box::new(identifier_expression(&["col1"])),
//             BinaryMatchingExpression::Not(Box::new(BinaryMatchingExpression::In(
//                 InExpression::Select(subquery),
//             ))),
//         );

//         let expected_statement = select_statement_with_where_clause(expression);

//         run_sunny_day_test(
//             "SELECT * FROM table_1 WHERE col1 NOT IN (SELECT * FROM table_2)",
//             Statement::Select(expected_statement),
//         );
//     }
// }

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
