use crate::{Identifier, Parser, ParsingError, TokenType};

pub trait IdentifierParser {
    /// Parse an identifier
    fn parse_identifier(&mut self) -> Result<Identifier, ParsingError>;
}

impl<'a> IdentifierParser for Parser<'a> {
    /// Parse an identifier
    fn parse_identifier(&mut self) -> Result<Identifier, ParsingError> {
        let mut components = Vec::new();

        while let Ok(identifier) = self.peek_as_id() {
            components.push(identifier.to_string());
            // Consume the identifier token
            self.consume_as_id()?;

            if self.peek_as(TokenType::Dot).is_ok() {
                // Consume the dot token
                self.consume_as(TokenType::Dot)?;
            } else {
                break;
            }
        }

        match components.len() {
            0 => Err(ParsingError::UnexpectedToken(
                "Expected identifier".to_string(),
            )),
            1 => Ok(Identifier::Single(components[0].to_string())),
            _ => Ok(Identifier::Compound(components)),
        }
    }
}

#[cfg(test)]
mod identifier_expression_tests {
    use crate::parser::expression::test_utils::*;

    #[test]
    fn test_expression_identifier_valid() {
        run_sunny_day_test("SELECT id;", &identifier_expression(&["id"]));
        run_sunny_day_test(
            "SELECT table1.column1;",
            &identifier_expression(&["table1", "column1"]),
        );
        run_sunny_day_test(
            "SELECT schema1.table1.column1;",
            &identifier_expression(&["schema1", "table1", "column1"]),
        );
    }
}
