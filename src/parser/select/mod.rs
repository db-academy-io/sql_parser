mod values;

use crate::{
    DistinctType, Expression, Identifier, Keyword, SelectFrom, SelectFromSubquery, TokenType,
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

    fn parse_select_columns(&mut self) -> Result<Vec<SelectItem>, ParsingError>;

    fn parse_select_column(&mut self) -> Result<SelectItem, ParsingError>;

    fn parse_select_from_clause(&mut self) -> Result<Option<SelectFrom>, ParsingError>;
}

impl<'a> SelectStatementParser for Parser<'a> {
    fn parse_select_statement(&mut self) -> Result<SelectStatementType, ParsingError> {
        if let Ok(Keyword::Values) = self.peek_as_keyword() {
            return Ok(SelectStatementType::Values(self.parse_values_statement()?));
        }

        // Consume the SELECT keyword
        self.consume_as_keyword(Keyword::Select)?;

        let distinct_type = if self.consume_as_keyword(Keyword::Distinct).is_ok() {
            DistinctType::Distinct
        } else if self.consume_as_keyword(Keyword::All).is_ok() {
            DistinctType::All
        } else {
            DistinctType::None
        };

        let select_statement = SelectStatement {
            distinct_type,
            columns: self.parse_select_columns()?,
            from: self.parse_select_from_clause()?,
            ..Default::default()
        };

        Ok(SelectStatementType::Select(select_statement))
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
            dbg!("parse_select_from_clause");
            self.consume_as_keyword(Keyword::From)?;

            dbg!(&self.peek_token());
            if self.peek_as(TokenType::LeftParen).is_ok() {
                self.consume_as(TokenType::LeftParen)?;
                dbg!(&self.peek_token());

                if let Ok(Keyword::Select) = self.peek_as_keyword() {
                    dbg!(&self.peek_token());
                    let subquery = self.parse_select_statement()?;
                    dbg!(&subquery);

                    // Here the right parenthesis is mandatory
                    self.consume_as(TokenType::RightParen)?;
                    if let Ok(Keyword::As) = self.peek_as_keyword() {
                        self.consume_as_keyword(Keyword::As)?;
                        let alias = self.consume_as_id()?;
                        return Ok(Some(SelectFrom::Subquery(SelectFromSubquery {
                            subquery: Box::new(subquery),
                            alias: Some(alias.to_string()),
                        })));
                    }

                    if let Ok(value) = self.peek_as_id() {
                        return Ok(Some(SelectFrom::Subquery(SelectFromSubquery {
                            subquery: Box::new(subquery),
                            alias: Some(value.to_string()),
                        })));
                    }

                    return Ok(Some(SelectFrom::Subquery(SelectFromSubquery {
                        subquery: Box::new(subquery),
                        alias: None,
                    })));
                }
            }

            // TODO: Parse table-or-subquery
            return Ok(None);
        }
        Ok(None)
    }
}

#[cfg(test)]
mod test_utils {
    use crate::{
        DistinctType, Expression, Identifier, SelectFrom, SelectItem, SelectStatement,
        SelectStatementType,
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
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_subquery_aliased_without_as_keyword() {
        let expected_statement =
            select_statement_with_from(SelectFrom::Subquery(SelectFromSubquery {
                subquery: Box::new(SelectStatementType::Select(select_statement_with_columns(
                    DistinctType::None,
                    vec![SelectItem::Expression(Expression::Identifier(
                        Identifier::Single("t1".to_string()),
                    ))],
                ))),
                alias: Some("alias".to_string()),
            }));

        run_sunny_day_test(
            "SELECT * FROM (SELECT t1) alias",
            Statement::Select(expected_statement),
        );
    }
}
