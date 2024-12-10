mod values;

use crate::{DistinctType, Expression, Identifier, Keyword, TokenType};

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
}

#[cfg(test)]
mod test_utils {
    use crate::{DistinctType, SelectItem, SelectStatement, SelectStatementType, Statement};

    pub fn create_select_statement(
        distinct_type: DistinctType,
        columns: Vec<SelectItem>,
    ) -> Statement {
        Statement::Select(SelectStatementType::Select(SelectStatement {
            distinct_type,
            columns,
            ..Default::default()
        }))
    }
}

#[cfg(test)]
mod test_select_result_columns {
    use crate::{BinaryOp, DistinctType, Expression, Identifier, ParsingError, SelectItem};

    use super::test_utils::*;
    use crate::parser::expression::test_utils::*;
    use crate::parser::test_utils::*;

    #[test]
    fn test_select_distinct() {
        run_sunny_day_test(
            "SELECT DISTINCT column1",
            create_select_statement(
                DistinctType::Distinct,
                vec![SelectItem::Expression(identifier_expression(&["column1"]))],
            ),
        );
    }

    #[test]
    fn test_select_all() {
        run_sunny_day_test(
            "SELECT ALL column1",
            create_select_statement(
                DistinctType::All,
                vec![SelectItem::Expression(identifier_expression(&["column1"]))],
            ),
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
            create_select_statement(
                DistinctType::None,
                vec![SelectItem::Expression(numeric_literal_expression("1"))],
            ),
        );
    }

    #[test]
    fn test_select_statement_parser_with_single_identifier() {
        run_sunny_day_test(
            "SELECT id",
            create_select_statement(
                DistinctType::None,
                vec![SelectItem::Expression(identifier_expression(&["id"]))],
            ),
        );
    }

    #[test]
    fn test_select_statement_parser_with_multiple_literal_values() {
        run_sunny_day_test(
            "SELECT 1, 2, 3",
            create_select_statement(
                DistinctType::None,
                vec![
                    SelectItem::Expression(numeric_literal_expression("1")),
                    SelectItem::Expression(numeric_literal_expression("2")),
                    SelectItem::Expression(numeric_literal_expression("3")),
                ],
            ),
        );
    }

    #[test]
    fn test_select_statement_parser_with_multiple_identifiers() {
        run_sunny_day_test(
            "SELECT id, name, age",
            create_select_statement(
                DistinctType::None,
                vec![
                    SelectItem::Expression(identifier_expression(&["id"])),
                    SelectItem::Expression(identifier_expression(&["name"])),
                    SelectItem::Expression(identifier_expression(&["age"])),
                ],
            ),
        );
    }

    #[test]
    fn test_select_statement_parser_with_wildcard() {
        run_sunny_day_test(
            "SELECT *",
            create_select_statement(
                DistinctType::None,
                vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::Wildcard,
                ))],
            ),
        );
    }

    #[test]
    fn test_select_statement_parser_with_table_name_and_wildcard() {
        run_sunny_day_test(
            "SELECT table_1.*",
            create_select_statement(
                DistinctType::None,
                vec![SelectItem::Expression(Expression::Identifier(
                    Identifier::NameWithWildcard("table_1".to_string()),
                ))],
            ),
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
            create_select_statement(
                DistinctType::None,
                vec![SelectItem::ExpressionWithAlias(
                    identifier_expression(&["column1"]),
                    "alias".to_string(),
                )],
            ),
        );
    }

    #[test]
    fn test_select_statement_parser_with_alias_without_as_keyword() {
        run_sunny_day_test(
            "SELECT column1 alias",
            create_select_statement(
                DistinctType::None,
                vec![SelectItem::ExpressionWithAlias(
                    identifier_expression(&["column1"]),
                    "alias".to_string(),
                )],
            ),
        );
    }

    #[test]
    fn test_select_statement_parser_with_multiple_columns_and_aliases() {
        run_sunny_day_test(
            "SELECT column1, column2 AS alias2",
            create_select_statement(
                DistinctType::None,
                vec![
                    SelectItem::Expression(identifier_expression(&["column1"])),
                    SelectItem::ExpressionWithAlias(
                        identifier_expression(&["column2"]),
                        "alias2".to_string(),
                    ),
                ],
            ),
        );
    }

    #[test]
    fn test_select_statement_parser_with_expression_and_alias() {
        run_sunny_day_test(
            "SELECT 1 + col1 as incremented, column2 * 2 - 1 as doubled",
            create_select_statement(
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
            ),
        );
    }
}
