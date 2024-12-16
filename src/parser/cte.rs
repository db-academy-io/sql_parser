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
