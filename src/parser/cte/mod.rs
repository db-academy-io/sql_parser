use crate::{
    CteExpression, Identifier, IdentifierParser, Keyword, MaterializationType, Statement,
    TokenType, WithCteStatement,
};

use super::{select::SelectStatementParser, Parser, ParsingError};

pub trait CteStatementParser {
    fn parse_cte_statement(&mut self) -> Result<Statement, ParsingError>;

    fn parse_cte_expression(&mut self) -> Result<CteExpression, ParsingError>;

    fn parse_cte_column_names(&mut self) -> Result<Vec<Identifier>, ParsingError>;

    fn parse_cte_materialized(&mut self) -> Result<Option<MaterializationType>, ParsingError>;
}

impl<'a> CteStatementParser for Parser<'a> {
    fn parse_cte_statement(&mut self) -> Result<Statement, ParsingError> {
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
        match statement {
            Statement::Delete(mut delete_statement) => {
                delete_statement.with_cte = Some(WithCteStatement {
                    recursive,
                    cte_expressions,
                });
                Ok(Statement::Delete(delete_statement))
            }
            Statement::Select(mut select_statement) => {
                select_statement.with_cte = Some(WithCteStatement {
                    recursive,
                    cte_expressions,
                });
                Ok(Statement::Select(select_statement))
            }
            Statement::Insert(mut insert_statement) => {
                insert_statement.with_cte = Some(WithCteStatement {
                    recursive,
                    cte_expressions,
                });
                Ok(Statement::Insert(insert_statement))
            }
            Statement::Update(mut update_statement) => {
                update_statement.with_cte = Some(WithCteStatement {
                    recursive,
                    cte_expressions,
                });
                Ok(Statement::Update(update_statement))
            }
            _ => Err(ParsingError::UnexpectedParsingState("Common Table Expressions can only be used with DELETE, INSERT, SELECT, and UPDATE statements".to_string())),
        }
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
pub mod cte_statement_tests {
    use super::super::select::test_utils::select_star_from;
    use super::test_utils::cte_expression;
    use crate::{
        parser::test_utils::run_sunny_day_test, select::test_utils::select_statement, Identifier,
        MaterializationType, Statement, WithCteStatement,
    };

    #[test]
    fn cte_statement_test() {
        let mut expected_stmt = select_statement();
        expected_stmt.with_cte = Some(WithCteStatement {
            recursive: false,
            cte_expressions: vec![cte_expression(
                Identifier::from("cte_1"),
                vec![],
                None,
                select_star_from(Identifier::from("cte_table")),
            )],
        });

        run_sunny_day_test(
            "WITH cte_1 AS (SELECT * FROM cte_table) SELECT * FROM table_name1",
            Statement::Select(expected_stmt),
        );
    }

    #[test]
    fn recursive_cte_statement() {
        let mut expected_stmt = select_statement();
        expected_stmt.with_cte = Some(WithCteStatement {
            recursive: true,
            cte_expressions: vec![cte_expression(
                Identifier::from("cte_1"),
                vec![],
                None,
                select_star_from(Identifier::from("cte_table")),
            )],
        });

        run_sunny_day_test(
            "WITH RECURSIVE cte_1 AS (SELECT * FROM cte_table) SELECT * FROM table_name1",
            Statement::Select(expected_stmt),
        );
    }

    #[test]
    fn cte_statement_with_multiple_ctes() {
        let ctes = WithCteStatement {
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
        };

        let mut expected_stmt = select_statement();
        expected_stmt.with_cte = Some(ctes);

        run_sunny_day_test(
            "WITH cte_1 AS (SELECT * FROM cte_table1), cte_2 AS (SELECT * FROM cte_1) SELECT * FROM table_name1",
            Statement::Select(expected_stmt),
        );
    }

    #[test]
    fn cte_statement_with_materialized() {
        let materialization_types = vec![
            MaterializationType::Materialized,
            MaterializationType::NotMaterialized,
        ];

        for materialization_type in materialization_types {
            let mut expected_stmt = select_statement();
            expected_stmt.with_cte = Some(WithCteStatement {
                recursive: false,
                cte_expressions: vec![cte_expression(
                    Identifier::from("cte_1"),
                    vec![],
                    Some(materialization_type.clone()),
                    select_star_from(Identifier::from("cte_table")),
                )],
            });

            run_sunny_day_test(
                &format!(
                    "WITH cte_1 AS {} (SELECT * FROM cte_table) SELECT * FROM table_name1",
                    materialization_type
                ),
                Statement::Select(expected_stmt),
            );
        }
    }

    #[test]
    fn cte_statement_with_column_names() {
        let mut expected_stmt = select_statement();
        expected_stmt.with_cte = Some(WithCteStatement {
            recursive: false,
            cte_expressions: vec![cte_expression(
                Identifier::from("cte_1"),
                vec![Identifier::from("col1"), Identifier::from("col2")],
                None,
                select_star_from(Identifier::from("cte_table")),
            )],
        });

        run_sunny_day_test(
            "WITH cte_1 (col1, col2) AS (SELECT * FROM cte_table) SELECT * FROM table_name1",
            Statement::Select(expected_stmt),
        );
    }
}
