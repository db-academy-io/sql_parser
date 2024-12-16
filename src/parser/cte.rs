use crate::{
    expression::IdentifierParser, CteExpression, Identifier, Keyword, MaterializationType,
    TokenType, WithCteStatement,
};

use super::{select::SelectStatementParser, Parser, ParsingError};

pub trait CteStatementParser {
    fn parse_cte_statement(&mut self) -> Result<WithCteStatement, ParsingError>;

    fn parse_cte_expression(&mut self) -> Result<CteExpression, ParsingError>;

    fn parse_cte_column_names(&mut self) -> Result<Vec<Identifier>, ParsingError>;

    fn parse_cte_materialized(&mut self) -> Result<Option<MaterializationType>, ParsingError>;
}

impl<'a> CteStatementParser for Parser<'a> {
    fn parse_cte_statement(&mut self) -> Result<WithCteStatement, ParsingError> {
        self.consume_as_keyword(Keyword::With)?;

        let recursive = self.consume_as_keyword(Keyword::Recursive).is_ok();

        let mut cte_expressions = Vec::new();
        loop {
            let cte_expression = self.parse_cte_expression()?;
            cte_expressions.push(cte_expression);

            if self.consume_as(TokenType::Comma).is_err() {
                break;
            }
        }

        let statement = self.parse_statement()?;

        Ok(WithCteStatement {
            recursive,
            cte_expressions,
            statement: Box::new(statement),
        })
    }

    fn parse_cte_expression(&mut self) -> Result<CteExpression, ParsingError> {
        let name = self.parse_identifier()?;
        let column_names = self.parse_cte_column_names()?;

        self.consume_as_keyword(Keyword::As)?;

        let materialized = self.parse_cte_materialized()?;

        self.consume_as(TokenType::LeftParen)?;
        let select = self.parse_select_statement()?;
        self.consume_as(TokenType::RightParen)?;

        Ok(CteExpression {
            name,
            column_names,
            materialized,
            select,
        })
    }

    fn parse_cte_column_names(&mut self) -> Result<Vec<Identifier>, ParsingError> {
        let mut column_names = Vec::new();

        if self.consume_as(TokenType::LeftParen).is_ok() {
            loop {
                let column_name = self.parse_identifier()?;
                column_names.push(column_name);

                if self.consume_as(TokenType::Comma).is_err() {
                    break;
                }
            }
            self.consume_as(TokenType::RightParen)?;
        }

        Ok(column_names)
    }

    fn parse_cte_materialized(&mut self) -> Result<Option<MaterializationType>, ParsingError> {
        if self.consume_as_keyword(Keyword::Materialized).is_ok() {
            Ok(Some(MaterializationType::Materialized))
        } else if self.consume_as_keyword(Keyword::Not).is_ok() {
            self.consume_as_keyword(Keyword::Materialized)?;
            Ok(Some(MaterializationType::NotMaterialized))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{CteExpression, Identifier, MaterializationType, SelectStatement};

    pub fn cte_expression(
        name: Identifier,
        column_names: Vec<Identifier>,
        materialized: Option<MaterializationType>,
        select: SelectStatement,
    ) -> CteExpression {
        CteExpression {
            name,
            column_names,
            materialized,
            select,
        }
    }
}

#[cfg(test)]
pub mod test_cte_statement_parser {
    use super::super::select::test_utils::select_star_from;
    use super::test_utils::cte_expression;
    use crate::{
        parser::test_utils::run_sunny_day_test, Identifier, MaterializationType, Statement,
        WithCteStatement,
    };

    #[test]
    fn test_cte_statement_with_recursive() {
        let expected_statement = Statement::WithCte(WithCteStatement {
            recursive: true,
            cte_expressions: vec![cte_expression(
                Identifier::from("cte_1"),
                vec![],
                None,
                select_star_from(Identifier::from("cte_table")),
            )],
            statement: Box::new(Statement::Select(select_star_from(Identifier::from(
                "table_1",
            )))),
        });

        run_sunny_day_test(
            "WITH RECURSIVE cte_1 AS (SELECT * FROM cte_table) SELECT * FROM table_1",
            expected_statement,
        );
    }

    #[test]
    fn test_cte_statement_with_multiple_ctes() {
        let expected_statement = Statement::WithCte(WithCteStatement {
            recursive: false,
            cte_expressions: vec![
                cte_expression(
                    Identifier::from("cte_1"),
                    vec![],
                    None,
                    select_star_from(Identifier::from("cte_table1")),
                ),
                cte_expression(
                    Identifier::from("cte_2"),
                    vec![],
                    None,
                    select_star_from(Identifier::from("cte_1")),
                ),
            ],
            statement: Box::new(Statement::Select(select_star_from(Identifier::from(
                "cte_2",
            )))),
        });

        run_sunny_day_test(
            "WITH cte_1 AS (SELECT * FROM cte_table1), cte_2 AS (SELECT * FROM cte_1) SELECT * FROM cte_2",
            expected_statement,
        );
    }

    #[test]
    fn test_cte_statement_with_materialized() {
        let materialization_types = vec![
            MaterializationType::Materialized,
            MaterializationType::NotMaterialized,
        ];

        for materialization_type in materialization_types {
            let expected_statement = Statement::WithCte(WithCteStatement {
                recursive: false,
                cte_expressions: vec![cte_expression(
                    Identifier::from("cte_1"),
                    vec![],
                    Some(materialization_type.clone()),
                    select_star_from(Identifier::from("cte_table")),
                )],
                statement: Box::new(Statement::Select(select_star_from(Identifier::from(
                    "cte_1",
                )))),
            });

            run_sunny_day_test(
                &format!(
                    "WITH cte_1 AS {} (SELECT * FROM cte_table) SELECT * FROM cte_1",
                    materialization_type
                ),
                expected_statement,
            );
        }
    }

    #[test]
    fn test_cte_statement_with_column_names() {
        let expected_statement = Statement::WithCte(WithCteStatement {
            recursive: false,
            cte_expressions: vec![cte_expression(
                Identifier::from("cte_1"),
                vec![Identifier::from("col1"), Identifier::from("col2")],
                None,
                select_star_from(Identifier::from("cte_table")),
            )],
            statement: Box::new(Statement::Select(select_star_from(Identifier::from(
                "cte_1",
            )))),
        });

        run_sunny_day_test(
            "WITH cte_1 (col1, col2) AS (SELECT * FROM cte_table) SELECT * FROM cte_1",
            expected_statement,
        );
    }
}
