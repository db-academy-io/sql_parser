mod values;

use crate::expression::IdentifierParser;
use crate::{
    DistinctType, Expression, Identifier, IndexedType, JoinClause, JoinConstraint, JoinTable,
    JoinType, Keyword, SelectFrom, SelectFromFunction, SelectFromSubquery, SelectFromTable,
    TokenType,
};

use super::expression::ExpressionParser;
use super::{Parser, ParsingError};
use crate::ast::{SelectItem, SelectStatement, SelectStatementType};
pub use values::ValuesStatementParser;

/// Trait for parsing SELECT statements
/// The SELECT statement documentation can be found here:
/// https://www.sqlite.org/lang_select.html
pub trait SelectStatementParser {
    fn parse_select_statement(&mut self) -> Result<SelectStatementType, ParsingError>;

    fn parse_distinct_type(&mut self) -> Result<DistinctType, ParsingError>;

    fn parse_select_columns(&mut self) -> Result<Vec<SelectItem>, ParsingError>;

    fn parse_select_column(&mut self) -> Result<SelectItem, ParsingError>;

    fn parse_select_from_clause(&mut self) -> Result<Option<SelectFrom>, ParsingError>;

    fn parse_select_from_clause_subquery(&mut self) -> Result<SelectFrom, ParsingError>;

    fn parse_select_from_table_indexed_type(&mut self)
        -> Result<Option<IndexedType>, ParsingError>;

    fn parse_alias_if_exists(&mut self) -> Result<Option<String>, ParsingError>;

    fn parse_select_from_join_clause(
        &mut self,
        lhs: SelectFrom,
    ) -> Result<SelectFrom, ParsingError>;

    fn parse_select_from_clause_join_type(&mut self) -> Result<JoinType, ParsingError>;

    fn parse_select_from_clause_join_constraints(
        &mut self,
    ) -> Result<Option<JoinConstraint>, ParsingError>;

    fn parse_where_clause(&mut self) -> Result<Option<Box<Expression>>, ParsingError>;

    fn parse_group_by_clause(&mut self) -> Result<Option<Vec<Box<Expression>>>, ParsingError>;

    fn parse_having_clause(&mut self) -> Result<Option<Box<Expression>>, ParsingError>;
}

impl<'a> SelectStatementParser for Parser<'a> {
    fn parse_select_statement(&mut self) -> Result<SelectStatementType, ParsingError> {
        if let Ok(Keyword::Values) = self.peek_as_keyword() {
            return Ok(SelectStatementType::Values(self.parse_values_statement()?));
        }

        // Consume the SELECT keyword
        self.consume_as_keyword(Keyword::Select)?;

        let select_statement = SelectStatement {
            distinct_type: self.parse_distinct_type()?,
            columns: self.parse_select_columns()?,
            from: self.parse_select_from_clause()?,
            where_clause: self.parse_where_clause()?,
            ..Default::default()
        };

        Ok(SelectStatementType::Select(select_statement))
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

    fn parse_select_from_clause(&mut self) -> Result<Option<SelectFrom>, ParsingError> {
        if let Ok(Keyword::From) = self.peek_as_keyword() {
            self.consume_as_keyword(Keyword::From)?;
            return Ok(Some(self.parse_select_from_clause_subquery()?));
        }
        Ok(None)
    }

    fn parse_select_from_clause_subquery(&mut self) -> Result<SelectFrom, ParsingError> {
        dbg!("parse_select_from_clause");

        dbg!(&self.peek_token());
        if self.peek_as(TokenType::LeftParen).is_ok() {
            self.consume_as(TokenType::LeftParen)?;
            dbg!(&self.peek_token());

            if let Ok(Keyword::Select) = self.peek_as_keyword() {
                let subquery = self.parse_select_statement()?;
                // Here the right parenthesis is mandatory
                self.consume_as(TokenType::RightParen)?;
                let alias = self.parse_alias_if_exists()?;
                return Ok(SelectFrom::Subquery(SelectFromSubquery {
                    subquery: Box::new(subquery),
                    alias,
                }));
            } else {
                let mut froms = Vec::new();
                loop {
                    dbg!("parse_select_from_clause");
                    let table_or_subquery = self.parse_select_from_clause_subquery()?;
                    dbg!(&table_or_subquery);
                    froms.push(table_or_subquery);

                    if self.consume_as(TokenType::Comma).is_err() {
                        break;
                    }
                }
                // Here the right parenthesis is mandatory
                self.consume_as(TokenType::RightParen)?;
                return Ok(SelectFrom::Froms(froms));
            };
        }

        if let Ok(id) = self.parse_identifier() {
            dbg!("parse_select_from_clause");
            if self.peek_as(TokenType::LeftParen).is_ok() {
                self.consume_as(TokenType::LeftParen)?;

                let arguments = self.parse_comma_separated_expressions()?;

                self.consume_as(TokenType::RightParen)?;
                let alias = self.parse_alias_if_exists()?;
                return Ok(SelectFrom::Function(SelectFromFunction {
                    function_name: id,
                    arguments,
                    alias,
                }));
            } else {
                let alias = self.parse_alias_if_exists()?;
                let indexed_type = self.parse_select_from_table_indexed_type()?;

                let lhs = SelectFrom::Table(SelectFromTable {
                    table_id: id,
                    alias,
                    indexed_type,
                });

                return self.parse_select_from_join_clause(lhs);
            }
        }
        // TODO: improve this error message
        Err(ParsingError::UnexpectedToken(
            self.peek_token()?.to_string(),
        ))
    }

    fn parse_select_from_join_clause(
        &mut self,
        lhs: SelectFrom,
    ) -> Result<SelectFrom, ParsingError> {
        dbg!("parse_select_from_join_clause");
        let mut join_tables = Vec::new();

        while let Ok(join_type) = self.parse_select_from_clause_join_type() {
            let rhs = self.parse_select_from_clause_subquery()?;
            join_tables.push(JoinTable {
                join_type,
                table: Box::new(rhs),
                constraints: self.parse_select_from_clause_join_constraints()?,
            });
        }

        if join_tables.is_empty() {
            return Ok(lhs);
        }

        Ok(SelectFrom::Join(JoinClause {
            lhs_table: Box::new(lhs),
            join_tables,
        }))
    }

    fn parse_select_from_clause_join_type(&mut self) -> Result<JoinType, ParsingError> {
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

    fn parse_select_from_clause_join_constraints(
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

    fn parse_alias_if_exists(&mut self) -> Result<Option<String>, ParsingError> {
        if self.consume_as_keyword(Keyword::As).is_ok() {
            Ok(Some(self.consume_as_id()?))
        } else if let Ok(value) = self.consume_as_id() {
            Ok(Some(value.to_string()))
        } else {
            Ok(None)
        }
    }

    fn parse_select_from_table_indexed_type(
        &mut self,
    ) -> Result<Option<IndexedType>, ParsingError> {
        if self.consume_as_keyword(Keyword::Indexed).is_ok() {
            self.consume_as_keyword(Keyword::By)?;
            Ok(Some(IndexedType::Indexed(self.consume_as_id()?)))
        } else if self.consume_as_keyword(Keyword::Not).is_ok() {
            self.consume_as_keyword(Keyword::Indexed)?;
            Ok(Some(IndexedType::NotIndexed))
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

    fn parse_group_by_clause(&mut self) -> Result<Option<Vec<Box<Expression>>>, ParsingError> {
        todo!()
    }

    fn parse_having_clause(&mut self) -> Result<Option<Box<Expression>>, ParsingError> {
        todo!()
    }
}

#[cfg(test)]
mod test_utils {
    use crate::{
        DistinctType, Expression, Identifier, SelectFrom, SelectFromTable, SelectItem,
        SelectStatement, SelectStatementType,
    };

    pub fn select_statement_with_columns(
        distinct_type: DistinctType,
        columns: Vec<SelectItem>,
    ) -> SelectStatement {
        SelectStatement {
            distinct_type,
            columns,
            ..Default::default()
        }
    }

    pub fn select_statement_with_from(from: SelectFrom) -> SelectStatementType {
        SelectStatementType::Select(SelectStatement {
            distinct_type: DistinctType::None,
            columns: vec![SelectItem::Expression(Expression::Identifier(
                Identifier::Wildcard,
            ))],
            from: Some(from),
            ..Default::default()
        })
    }

    pub fn select_statement_with_where_clause(where_clause: Expression) -> SelectStatementType {
        SelectStatementType::Select(SelectStatement {
            distinct_type: DistinctType::None,
            columns: vec![SelectItem::Expression(Expression::Identifier(
                Identifier::Wildcard,
            ))],
            from: Some(SelectFrom::Table(SelectFromTable {
                table_id: Identifier::Single("table_1".to_string()),
                alias: None,
                indexed_type: None,
            })),
            where_clause: Some(Box::new(where_clause)),
            ..Default::default()
        })
    }
}

#[cfg(test)]
mod test_select_result_columns {
    use crate::{
        BinaryOp, DistinctType, Expression, Identifier, ParsingError, SelectItem,
        SelectStatementType, Statement,
    };

    use super::test_utils::*;
    use crate::parser::expression::test_utils::*;
    use crate::parser::test_utils::*;

    #[test]
    fn test_select_distinct() {
        run_sunny_day_test(
            "SELECT DISTINCT column1",
            Statement::Select(SelectStatementType::Select(select_statement_with_columns(
                DistinctType::Distinct,
                vec![SelectItem::Expression(identifier_expression(&["column1"]))],
            ))),
        );
    }

    #[test]
    fn test_select_all() {
        run_sunny_day_test(
            "SELECT ALL column1",
            Statement::Select(SelectStatementType::Select(select_statement_with_columns(
                DistinctType::All,
                vec![SelectItem::Expression(identifier_expression(&["column1"]))],
            ))),
        );
    }

    #[test]
    fn test_select_distinct_all() {
        run_rainy_day_test(
            "SELECT DISTINCT ALL column1",
            ParsingError::UnexpectedToken("All".to_string()),
        );

        run_rainy_day_test(
            "SELECT ALL DISTINCT column1",
            ParsingError::UnexpectedToken("Distinct".to_string()),
        );
    }

    #[test]
    fn test_select_statement_parser_with_single_literal_value() {
        run_sunny_day_test(
            "SELECT 1",
            Statement::Select(SelectStatementType::Select(select_statement_with_columns(
                DistinctType::None,
                vec![SelectItem::Expression(numeric_literal_expression("1"))],
            ))),
        );
    }

    #[test]
    fn test_select_statement_parser_with_single_identifier() {
        run_sunny_day_test(
            "SELECT id",
            Statement::Select(SelectStatementType::Select(select_statement_with_columns(
                DistinctType::None,
                vec![SelectItem::Expression(identifier_expression(&["id"]))],
            ))),
        );
    }

    #[test]
    fn test_select_statement_parser_with_multiple_literal_values() {
        run_sunny_day_test(
            "SELECT 1, 2, 3",
            Statement::Select(SelectStatementType::Select(select_statement_with_columns(
                DistinctType::None,
                vec![
                    SelectItem::Expression(numeric_literal_expression("1")),
                    SelectItem::Expression(numeric_literal_expression("2")),
                    SelectItem::Expression(numeric_literal_expression("3")),
                ],
            ))),
        );
    }

    #[test]
    fn test_select_statement_parser_with_multiple_identifiers() {
        run_sunny_day_test(
            "SELECT id, name, age",
            Statement::Select(SelectStatementType::Select(select_statement_with_columns(
                DistinctType::None,
                vec![
                    SelectItem::Expression(identifier_expression(&["id"])),
                    SelectItem::Expression(identifier_expression(&["name"])),
                    SelectItem::Expression(identifier_expression(&["age"])),
                ],
            ))),
        );
    }

    #[test]
    fn test_select_statement_parser_with_wildcard() {
        run_sunny_day_test(
            "SELECT *",
            Statement::Select(SelectStatementType::Select(select_statement_with_columns(
                DistinctType::None,
                vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
            ))),
        );
    }

    #[test]
    fn test_select_statement_parser_with_table_name_and_wildcard() {
        run_sunny_day_test(
            "SELECT table_1.*",
            Statement::Select(SelectStatementType::Select(select_statement_with_columns(
                DistinctType::None,
                vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::NameWithWildcard("table_1".to_string()),
                ))],
            ))),
        );
    }

    #[test]
    fn test_select_statement_parser_with_table_name_and_column_name() {
        run_rainy_day_test(
            "SELECT table_1.column",
            ParsingError::UnexpectedToken("Column".to_string()),
        );
    }

    #[test]
    fn test_select_statement_parser_with_alias() {
        run_sunny_day_test(
            "SELECT column1 AS alias",
            Statement::Select(SelectStatementType::Select(select_statement_with_columns(
                DistinctType::None,
                vec![SelectItem::ExpressionWithAlias(
                    identifier_expression(&["column1"]),
                    "alias".to_string(),
                )],
            ))),
        );
    }

    #[test]
    fn test_select_statement_parser_with_alias_without_as_keyword() {
        run_sunny_day_test(
            "SELECT column1 alias",
            Statement::Select(SelectStatementType::Select(select_statement_with_columns(
                DistinctType::None,
                vec![SelectItem::ExpressionWithAlias(
                    identifier_expression(&["column1"]),
                    "alias".to_string(),
                )],
            ))),
        );
    }

    #[test]
    fn test_select_statement_parser_with_multiple_columns_and_aliases() {
        run_sunny_day_test(
            "SELECT column1, column2 AS alias2",
            Statement::Select(SelectStatementType::Select(select_statement_with_columns(
                DistinctType::None,
                vec![
                    SelectItem::Expression(identifier_expression(&["column1"])),
                    SelectItem::ExpressionWithAlias(
                        identifier_expression(&["column2"]),
                        "alias2".to_string(),
                    ),
                ],
            ))),
        );
    }

    #[test]
    fn test_select_statement_parser_with_expression_and_alias() {
        run_sunny_day_test(
            "SELECT 1 + col1 as incremented, column2 * 2 - 1 as doubled",
            Statement::Select(SelectStatementType::Select(select_statement_with_columns(
                DistinctType::None,
                vec![
                    SelectItem::ExpressionWithAlias(
                        binary_op_expression(
                            BinaryOp::Plus,
                            numeric_literal_expression("1"),
                            identifier_expression(&["col1"]),
                        ),
                        "incremented".to_string(),
                    ),
                    SelectItem::ExpressionWithAlias(
                        binary_op_expression(
                            BinaryOp::Minus,
                            binary_op_expression(
                                BinaryOp::Mul,
                                identifier_expression(&["column2"]),
                                numeric_literal_expression("2"),
                            ),
                            numeric_literal_expression("1"),
                        ),
                        "doubled".to_string(),
                    ),
                ],
            ))),
        );
    }
}

#[cfg(test)]
mod test_select_from_table_indexed {
    use super::test_utils::select_statement_with_from;
    use crate::parser::test_utils::*;
    use crate::{Identifier, IndexedType, SelectFrom, SelectFromTable, Statement};

    #[test]
    fn test_select_from_table() {
        let expected_statement = select_statement_with_from(SelectFrom::Table(
            SelectFromTable::from(Identifier::Single("table_1".to_string())),
        ));

        run_sunny_day_test(
            "SELECT * FROM table_1",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_table_with_schema() {
        let expected_statement =
            select_statement_with_from(SelectFrom::Table(SelectFromTable::from(
                Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            )));

        run_sunny_day_test(
            "SELECT * FROM schema_1.table_1",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_table_with_alias() {
        let expected_statement = select_statement_with_from(SelectFrom::Table(SelectFromTable {
            table_id: Identifier::Compound(vec!["schema_1".to_string(), "table_1".to_string()]),
            alias: Some("alias".to_string()),
            indexed_type: None,
        }));

        run_sunny_day_test(
            "SELECT * FROM schema_1.table_1 AS alias",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_table_with_alias_without_as_keyword() {
        let expected_statement = select_statement_with_from(SelectFrom::Table(SelectFromTable {
            table_id: Identifier::Single("table_1".to_string()),
            alias: Some("alias".to_string()),
            indexed_type: None,
        }));

        run_sunny_day_test(
            "SELECT * FROM table_1 alias",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_table_with_alias_indexed() {
        let expected_statement = select_statement_with_from(SelectFrom::Table(SelectFromTable {
            table_id: Identifier::Single("table_1".to_string()),
            alias: Some("alias".to_string()),
            indexed_type: Some(IndexedType::Indexed("index_1".to_string())),
        }));

        run_sunny_day_test(
            "SELECT * FROM table_1 alias INDEXED BY index_1",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_table_not_indexed() {
        let expected_statement = select_statement_with_from(SelectFrom::Table(SelectFromTable {
            table_id: Identifier::Single("table_1".to_string()),
            alias: None,
            indexed_type: Some(IndexedType::NotIndexed),
        }));

        run_sunny_day_test(
            "SELECT * FROM table_1 NOT INDEXED",
            Statement::Select(expected_statement),
        );
    }
}

#[cfg(test)]
mod test_select_from_subquery {
    use super::test_utils::{select_statement_with_columns, select_statement_with_from};
    use crate::parser::test_utils::*;
    use crate::{
        DistinctType, Expression, Identifier, SelectFrom, SelectFromSubquery, SelectItem,
        SelectStatementType, Statement,
    };

    #[test]
    fn test_select_from_subquery() {
        let expected_statement =
            select_statement_with_from(SelectFrom::Subquery(SelectFromSubquery {
                subquery: Box::new(SelectStatementType::Select(select_statement_with_columns(
                    DistinctType::None,
                    vec![SelectItem::Expression(Expression::Identifier(
                        Identifier::Single("col1".to_string()),
                    ))],
                ))),
                alias: None,
            }));

        run_sunny_day_test(
            "SELECT * FROM (SELECT col1)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_subquery_aliased() {
        let expected_statement =
            select_statement_with_from(SelectFrom::Subquery(SelectFromSubquery {
                subquery: Box::new(SelectStatementType::Select(select_statement_with_columns(
                    DistinctType::None,
                    vec![SelectItem::Expression(Expression::Identifier(
                        Identifier::NameWithWildcard("t".to_string()),
                    ))],
                ))),
                alias: Some("alias".to_string()),
            }));

        run_sunny_day_test(
            "SELECT * FROM (SELECT t.* ) as alias",
            Statement::Select(expected_statement.clone()),
        );

        // without the as keyword
        run_sunny_day_test(
            "SELECT * FROM (SELECT t.* ) alias",
            Statement::Select(expected_statement.clone()),
        );
    }
}

#[cfg(test)]
mod test_select_from_table_function {
    use super::test_utils::select_statement_with_from;
    use crate::expression::test_utils::{
        binary_op_expression, identifier_expression, numeric_literal_expression,
    };
    use crate::parser::test_utils::*;
    use crate::{BinaryOp, Identifier, SelectFrom, SelectFromFunction, Statement};

    #[test]
    fn test_select_from_table_function() {
        let expected_statement =
            select_statement_with_from(SelectFrom::Function(SelectFromFunction {
                function_name: Identifier::Single("function_1".to_string()),
                arguments: vec![numeric_literal_expression("1")],
                alias: None,
            }));

        run_sunny_day_test(
            "SELECT * FROM function_1(1)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_table_function_with_schema() {
        let expected_statement =
            select_statement_with_from(SelectFrom::Function(SelectFromFunction {
                function_name: Identifier::Compound(vec![
                    "schema_1".to_string(),
                    "function_1".to_string(),
                ]),
                arguments: vec![binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                )],
                alias: None,
            }));

        run_sunny_day_test(
            "SELECT * FROM schema_1.function_1(1+2)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_table_function_with_multiple_arguments() {
        let expected_statement =
            select_statement_with_from(SelectFrom::Function(SelectFromFunction {
                function_name: Identifier::Compound(vec![
                    "schema_1".to_string(),
                    "function_1".to_string(),
                ]),
                arguments: vec![
                    numeric_literal_expression("1"),
                    identifier_expression(&["col1"]),
                    numeric_literal_expression("3"),
                ],
                alias: None,
            }));

        run_sunny_day_test(
            "SELECT * FROM schema_1.function_1(1, col1, 3)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_table_function_with_alias() {
        let expected_statement =
            select_statement_with_from(SelectFrom::Function(SelectFromFunction {
                function_name: Identifier::Compound(vec![
                    "schema_1".to_string(),
                    "function_1".to_string(),
                ]),
                arguments: vec![
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                    numeric_literal_expression("3"),
                ],
                alias: Some("alias".to_string()),
            }));

        run_sunny_day_test(
            "SELECT * FROM schema_1.function_1(1, 2, 3) AS alias",
            Statement::Select(expected_statement.clone()),
        );

        run_sunny_day_test(
            "SELECT * FROM schema_1.function_1(1, 2, 3) alias",
            Statement::Select(expected_statement.clone()),
        );
    }
}

#[cfg(test)]
mod test_select_from_comma_separated_table_or_subqueries {
    use super::test_utils::select_statement_with_from;
    use crate::parser::test_utils::*;
    use crate::{
        Identifier, IndexedType, JoinClause, JoinTable, JoinType, SelectFrom, SelectFromSubquery,
        SelectFromTable, Statement,
    };

    #[test]
    fn test_select_from_comma_separated_table_or_subqueries() {
        fn join(lhs: SelectFrom, rhs: SelectFrom) -> SelectFrom {
            SelectFrom::Join(JoinClause {
                lhs_table: Box::new(lhs),
                join_tables: vec![JoinTable {
                    join_type: JoinType::Cross,
                    table: Box::new(rhs),
                    constraints: None,
                }],
            })
        }

        let table_1 = SelectFrom::Table(SelectFromTable::from(Identifier::Single(
            "table_1".to_string(),
        )));
        let schema2_table2 = SelectFrom::Table(SelectFromTable::from(Identifier::Compound(vec![
            "schema2".to_string(),
            "table2".to_string(),
        ])));

        let schema3_table3 = SelectFrom::Table(SelectFromTable {
            table_id: Identifier::Compound(vec!["schema3".to_string(), "table3".to_string()]),
            alias: Some("table3_alias".to_string()),
            indexed_type: None,
        });

        let indexed_table = SelectFrom::Table(SelectFromTable {
            table_id: Identifier::Single("indexed_table".to_string()),
            alias: Some("t1".to_string()),
            indexed_type: Some(IndexedType::Indexed("index_1".to_string())),
        });

        let not_indexed_table = SelectFrom::Table(SelectFromTable {
            table_id: Identifier::Single("not_indexed_table".to_string()),
            alias: Some("t2".to_string()),
            indexed_type: Some(IndexedType::NotIndexed),
        });

        let subquery = SelectFrom::Subquery(SelectFromSubquery {
            subquery: Box::new(select_statement_with_from(SelectFrom::Table(
                SelectFromTable::from(Identifier::Single("table_2".to_string())),
            ))),
            alias: Some("select_alias".to_string()),
        });

        let expected_statement = select_statement_with_from(SelectFrom::Froms(vec![join(
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

#[cfg(test)]
mod test_select_from_with_join_clause {
    use super::test_utils::select_statement_with_from;
    use crate::expression::test_utils::{binary_op_expression, identifier_expression};
    use crate::parser::test_utils::*;
    use crate::{
        BinaryOp, Identifier, IndexedType, JoinClause, JoinConstraint, JoinTable, JoinType,
        SelectFrom, SelectFromSubquery, SelectFromTable, Statement,
    };

    #[test]
    fn test_select_from_with_join_clause() {
        let expected_statement = select_statement_with_from(SelectFrom::Join(JoinClause {
            lhs_table: Box::new(SelectFrom::Table(SelectFromTable::from(
                Identifier::Single("table_1".to_string()),
            ))),
            join_tables: vec![JoinTable {
                join_type: JoinType::Inner(false),
                table: Box::new(SelectFrom::Table(SelectFromTable::from(
                    Identifier::Single("table_2".to_string()),
                ))),
                constraints: None,
            }],
        }));

        run_sunny_day_test(
            "SELECT * FROM table_1 INNER JOIN table_2",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_with_join_types() {
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
            let expected_statement = select_statement_with_from(SelectFrom::Join(JoinClause {
                lhs_table: Box::new(SelectFrom::Table(SelectFromTable::from(
                    Identifier::Single("table_1".to_string()),
                ))),
                join_tables: vec![JoinTable {
                    join_type: join_type.clone(),
                    table: Box::new(SelectFrom::Table(SelectFromTable::from(
                        Identifier::Single("table_2".to_string()),
                    ))),
                    constraints: None,
                }],
            }));

            run_sunny_day_test(
                &format!("SELECT * FROM table_1 {} JOIN table_2", &join_type),
                Statement::Select(expected_statement),
            );
        }
    }

    #[test]
    fn test_select_from_with_cross_join() {
        let expected_statement = select_statement_with_from(SelectFrom::Join(JoinClause {
            lhs_table: Box::new(SelectFrom::Table(SelectFromTable::from(
                Identifier::Single("table_1".to_string()),
            ))),
            join_tables: vec![JoinTable {
                join_type: JoinType::Cross,
                table: Box::new(SelectFrom::Table(SelectFromTable::from(
                    Identifier::Single("table_2".to_string()),
                ))),
                constraints: None,
            }],
        }));

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
    fn test_select_from_with_join_clause_with_on_constraints() {
        let expected_statement = select_statement_with_from(SelectFrom::Join(JoinClause {
            lhs_table: Box::new(SelectFrom::Table(SelectFromTable::from(
                Identifier::Single("table_1".to_string()),
            ))),
            join_tables: vec![JoinTable {
                join_type: JoinType::Inner(false),
                table: Box::new(SelectFrom::Table(SelectFromTable::from(
                    Identifier::Single("table_2".to_string()),
                ))),
                constraints: Some(JoinConstraint::On(binary_op_expression(
                    BinaryOp::Equals,
                    identifier_expression(&["table_1", "col1"]),
                    identifier_expression(&["table_2", "col1"]),
                ))),
            }],
        }));

        run_sunny_day_test(
            "SELECT * FROM table_1 INNER JOIN table_2 ON table_1.col1 = table_2.col1",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_with_join_clause_with_using_constraints() {
        let expected_statement = select_statement_with_from(SelectFrom::Join(JoinClause {
            lhs_table: Box::new(SelectFrom::Table(SelectFromTable::from(
                Identifier::Single("table_1".to_string()),
            ))),
            join_tables: vec![JoinTable {
                join_type: JoinType::Inner(false),
                table: Box::new(SelectFrom::Table(SelectFromTable::from(
                    Identifier::Single("table_2".to_string()),
                ))),
                constraints: Some(JoinConstraint::Using(vec![Identifier::Single(
                    "col1".to_string(),
                )])),
            }],
        }));

        run_sunny_day_test(
            "SELECT * FROM table_1 INNER JOIN table_2 USING (col1)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_with_join_clause_nested() {
        let expected_statement = select_statement_with_from(SelectFrom::Join(JoinClause {
            lhs_table: Box::new(SelectFrom::Table(SelectFromTable::from(
                Identifier::Single("table_1".to_string()),
            ))),
            join_tables: vec![
                JoinTable {
                    join_type: JoinType::Inner(false),
                    table: Box::new(SelectFrom::Table(SelectFromTable::from(
                        Identifier::Single("table_2".to_string()),
                    ))),
                    constraints: Some(JoinConstraint::On(binary_op_expression(
                        BinaryOp::Equals,
                        identifier_expression(&["table_1", "col1"]),
                        identifier_expression(&["table_2", "col2"]),
                    ))),
                },
                JoinTable {
                    join_type: JoinType::Full(false),
                    table: Box::new(SelectFrom::Table(SelectFromTable::from(
                        Identifier::Single("table_3".to_string()),
                    ))),
                    constraints: Some(JoinConstraint::Using(vec![Identifier::Single(
                        "col3".to_string(),
                    )])),
                },
            ],
        }));

        run_sunny_day_test(
            "SELECT * 
                FROM table_1 
                INNER JOIN table_2 ON table_1.col1 = table_2.col2
                FULL JOIN table_3 USING (col3)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_with_join_clause_with_nested_keeping_ordering() {
        let subquery = JoinTable {
            join_type: JoinType::Inner(false),
            table: Box::new(SelectFrom::Subquery(SelectFromSubquery {
                subquery: Box::new(select_statement_with_from(SelectFrom::Join(JoinClause {
                    lhs_table: Box::new(SelectFrom::Table(SelectFromTable {
                        table_id: Identifier::Single("table_2".to_string()),
                        alias: Some("t2".to_string()),
                        indexed_type: None,
                    })),
                    join_tables: vec![JoinTable {
                        join_type: JoinType::Left(false),
                        table: Box::new(SelectFrom::Table(SelectFromTable {
                            table_id: Identifier::Single("table_3".to_string()),
                            alias: Some("t3".to_string()),
                            indexed_type: Some(IndexedType::Indexed("index_3".to_string())),
                        })),
                        constraints: Some(JoinConstraint::On(binary_op_expression(
                            BinaryOp::Equals,
                            identifier_expression(&["t2", "col2"]),
                            identifier_expression(&["t3", "col3"]),
                        ))),
                    }],
                }))),
                alias: Some("t_complex".to_string()),
            })),
            constraints: Some(JoinConstraint::On(binary_op_expression(
                BinaryOp::Equals,
                identifier_expression(&["t1", "col1"]),
                identifier_expression(&["t_complex", "col1"]),
            ))),
        };

        let expected_statement = select_statement_with_from(SelectFrom::Join(JoinClause {
            lhs_table: Box::new(SelectFrom::Table(SelectFromTable {
                table_id: Identifier::Single("table_1".to_string()),
                alias: Some("t1".to_string()),
                indexed_type: None,
            })),
            join_tables: vec![subquery],
        }));

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
mod test_select_where_clause {
    use super::test_utils::{select_statement_with_from, select_statement_with_where_clause};
    use crate::expression::test_utils::{
        binary_op_expression, identifier_expression, numeric_literal_expression,
    };
    use crate::parser::test_utils::*;
    use crate::{BinaryOp, Expression, Identifier, SelectFrom, SelectFromTable, Statement};

    #[test]
    fn test_select_where_clause() {
        let expected_statement = select_statement_with_where_clause(binary_op_expression(
            BinaryOp::Equals,
            numeric_literal_expression("1"),
            numeric_literal_expression("1"),
        ));
        run_sunny_day_test(
            "SELECT * FROM table_1 WHERE 1 = 1",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_where_clause_with_binary_ops() {
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

        for binary_op in binary_ops {
            let expected_statement = select_statement_with_where_clause(binary_op_expression(
                binary_op.clone(),
                identifier_expression(&["col1"]),
                identifier_expression(&["col2"]),
            ));
            run_sunny_day_test(
                &format!("SELECT * FROM table_1 WHERE col1 {} col2", binary_op),
                Statement::Select(expected_statement),
            );
        }
    }

    #[test]
    fn test_select_where_clause_with_subquery() {
        let subquery = select_statement_with_from(SelectFrom::Table(SelectFromTable::from(
            Identifier::Single("table_2".to_string()),
        )));

        let expression = Expression::BinaryMatchingExpression(
            Box::new(identifier_expression(&["col1"])),
            BinaryMatchingExpression::Not(Box::new(BinaryMatchingExpression::In(
                InExpression::Select(subquery),
            ))),
        );

        let expected_statement = select_statement_with_where_clause(expression);

        run_sunny_day_test(
            "SELECT * FROM table_1 WHERE col1 NOT IN (SELECT * FROM table_2)",
            Statement::Select(expected_statement),
        );
    }
}
