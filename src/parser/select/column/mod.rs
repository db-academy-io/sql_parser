use crate::{
    parser::{ExpressionParser, ParsingError},
    Expression, Identifier, Keyword, Parser, SelectItem, TokenType,
};

pub trait SelectColumnsParser {
    fn parse_select_columns(&mut self) -> Result<Vec<SelectItem>, ParsingError>;

    fn parse_select_column(&mut self) -> Result<SelectItem, ParsingError>;
}

impl SelectColumnsParser for Parser<'_> {
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
mod test_select_result_columns {
    use crate::{BinaryOp, DistinctType, Expression, Identifier, SelectItem};

    use crate::parser::expression::test_utils::*;
    use crate::parser::select::test_utils::*;
    use crate::parser::{test_utils::*, ParsingError};

    #[test]
    fn select_star() {
        let expected = select();

        run_sunny_day_test("SELECT * ", expected.into());
    }

    #[test]
    fn select_distinct() {
        let mut expected = select();
        expected.distinct_type = DistinctType::Distinct;

        run_sunny_day_test("SELECT DISTINCT *", expected.into());
    }

    #[test]
    fn select_all() {
        let mut expected = select();
        expected.distinct_type = DistinctType::All;

        run_sunny_day_test("SELECT ALL *", expected.into());
    }

    #[test]
    fn select_distinct_all() {
        run_rainy_day_test(
            "SELECT DISTINCT ALL column1",
            ParsingError::UnexpectedToken("All at position 16".to_string()),
        );

        run_rainy_day_test(
            "SELECT ALL DISTINCT column1",
            ParsingError::UnexpectedToken("Distinct at position 11".to_string()),
        );
    }

    #[test]
    fn select_single_literal_value() {
        let mut expected = select();
        expected.columns = vec![SelectItem::Expression(numeric_expr("1"))];

        run_sunny_day_test("SELECT 1", expected.into());
    }

    #[test]
    fn select_single_identifier() {
        let mut expected = select();
        expected.columns = vec![SelectItem::Expression(Expression::Identifier(
            Identifier::Single("column1".to_string()),
        ))];
        run_sunny_day_test("SELECT column1", expected.into());
    }

    #[test]
    fn select_multiple_literal_values() {
        let mut expected = select();
        expected.columns = vec![
            SelectItem::Expression(numeric_expr("1")),
            SelectItem::Expression(numeric_expr("2")),
            SelectItem::Expression(numeric_expr("3")),
        ];

        run_sunny_day_test("SELECT 1, 2, 3", expected.into());
    }

    #[test]
    fn select_multiple_identifiers() {
        let mut expected = select();
        expected.columns = vec![
            SelectItem::Expression(identifier_expr(&["id"])),
            SelectItem::Expression(identifier_expr(&["name"])),
            SelectItem::Expression(identifier_expr(&["age"])),
        ];

        run_sunny_day_test("SELECT id, name, age", expected.into());
    }

    #[test]
    fn select_table_name_and_wildcard() {
        let mut expected = select();
        expected.columns = vec![SelectItem::Expression(Expression::Identifier(
            Identifier::NameWithWildcard("table_1".to_string()),
        ))];

        run_sunny_day_test("SELECT table_1.*", expected.into());
    }

    #[test]
    fn select_table_name_and_column_name() {
        let mut expected = select();
        expected.columns = vec![SelectItem::Expression(Expression::Identifier(
            Identifier::Compound(vec!["table_1".to_string(), "column1".to_string()]),
        ))];

        run_sunny_day_test("SELECT table_1.column1", expected.into());
    }

    #[test]
    fn select_column_with_alias() {
        let mut expected = select();
        expected.columns = vec![SelectItem::ExpressionWithAlias(
            identifier_expr(&["column1"]),
            "alias".to_string(),
        )];

        run_sunny_day_test("SELECT column1 AS alias", expected.into());
    }

    #[test]
    fn select_column_with_alias_without_as_keyword() {
        let mut expected = select();
        expected.columns = vec![SelectItem::ExpressionWithAlias(
            identifier_expr(&["column1"]),
            "alias".to_string(),
        )];

        run_sunny_day_test("SELECT column1 alias", expected.into());
    }

    #[test]
    fn select_multiple_columns() {
        let mut expected = select();
        expected.columns = vec![
            SelectItem::Expression(identifier_expr(&["column1"])),
            SelectItem::ExpressionWithAlias(identifier_expr(&["column2"]), "alias2".to_string()),
        ];

        run_sunny_day_test("SELECT column1, column2 AS alias2", expected.into());
    }

    #[test]
    fn select_expressions_with_alias() {
        let mut expected = select();
        expected.columns = vec![
            SelectItem::ExpressionWithAlias(
                binary_op(
                    BinaryOp::Plus,
                    numeric_expr("1"),
                    identifier_expr(&["col1"]),
                ),
                "incremented".to_string(),
            ),
            SelectItem::ExpressionWithAlias(
                binary_op(
                    BinaryOp::Minus,
                    binary_op(
                        BinaryOp::Mul,
                        identifier_expr(&["column2"]),
                        numeric_expr("2"),
                    ),
                    numeric_expr("1"),
                ),
                "doubled".to_string(),
            ),
        ];

        run_sunny_day_test(
            "SELECT 1 + col1 as incremented, column2 * 2 - 1 as doubled",
            expected.into(),
        );
    }
}
