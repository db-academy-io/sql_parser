use super::join::SelectFromJoinParser;
use super::SelectFromFunctionParser;
use crate::parser::select::SelectStatementParser;
use crate::{
    parser::{IdentifierParser, ParsingError},
    FromClause, Keyword, Parser, QualifiedTableName, SelectFromSubquery, TokenType,
};

pub trait SelectFromSubqueryParser {
    fn parse_from_clause_subquery(&mut self) -> Result<FromClause, ParsingError>;
}

impl<'a> SelectFromSubqueryParser for Parser<'a> {
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
                return self.parse_from_function(id);
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
}

#[cfg(test)]
mod test_select_from_subquery {
    use crate::parser::select::from::test_utils::*;
    use crate::parser::test_utils::*;
    use crate::{Identifier, QualifiedTableName, SelectFromSubquery, Statement};

    #[test]
    fn select_from_subquery_test() {
        let expected_statement = select_from_subquery(SelectFromSubquery {
            subquery: Box::new(select_from_table(QualifiedTableName::from(
                Identifier::Single("table_1".to_string()),
            ))),
            alias: None,
        });

        run_sunny_day_test(
            "SELECT * FROM (SELECT * FROM table_1)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn test_select_from_subquery_aliased() {
        let expected_statement = select_from_subquery(SelectFromSubquery {
            subquery: Box::new(select_from_table(QualifiedTableName::from(
                Identifier::Single("table_1".to_string()),
            ))),
            alias: Some("alias".to_string()),
        });

        run_sunny_day_test(
            "SELECT * FROM (SELECT * FROM table_1) as alias",
            Statement::Select(expected_statement.clone()),
        );

        // without the as keyword
        run_sunny_day_test(
            "SELECT * FROM (SELECT * FROM table_1) alias",
            Statement::Select(expected_statement.clone()),
        );
    }
}
