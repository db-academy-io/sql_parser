use crate::parser::errors::ParsingError;
use crate::{Identifier, Parser, TokenType};

pub trait IdentifierParser {
    /// Parse an identifier
    fn parse_identifier(&mut self) -> Result<Identifier, ParsingError>;
}

impl IdentifierParser for Parser<'_> {
    /// Parse an identifier
    fn parse_identifier(&mut self) -> Result<Identifier, ParsingError> {
        let mut components = Vec::new();

        let mut expected_next_identifier = false;
        while let Ok(identifier) = self.peek_as_id_or_star() {
            components.push(identifier.to_string());
            // Consume the identifier token
            self.consume_token()?;
            expected_next_identifier = false;

            if self.consume_as(TokenType::Dot).is_ok() {
                expected_next_identifier = true;
            } else {
                break;
            }
        }

        if expected_next_identifier {
            return Err(ParsingError::UnexpectedToken(
                "Expected identifier".to_string(),
            ));
        }

        match components.len() {
            0 => Err(ParsingError::UnexpectedToken(
                "Expected identifier".to_string(),
            )),
            1 => {
                if components[0] == "*" {
                    Ok(Identifier::Wildcard)
                } else {
                    Ok(Identifier::Single(components[0].to_string()))
                }
            }
            2 => {
                if components[1] == "*" {
                    Ok(Identifier::NameWithWildcard(components[0].to_string()))
                } else {
                    Ok(Identifier::Compound(components))
                }
            }
            _ => Ok(Identifier::Compound(components)),
        }
    }
}

#[cfg(test)]
mod identifier_expression_tests {
    use crate::{
        parser::{expression::test_utils::*, test_utils::run_sunny_day_test},
        select::test_utils::select_expr,
    };

    #[test]
    fn identifier_valid() {
        run_sunny_day_test("SELECT id;", select_expr(identifier_expr(&["id"])).into());
        run_sunny_day_test(
            "SELECT table1.column1;",
            select_expr(identifier_expr(&["table1", "column1"])).into(),
        );
        run_sunny_day_test(
            "SELECT schema1.table1.column1;",
            select_expr(identifier_expr(&["schema1", "table1", "column1"])).into(),
        );
    }
}
