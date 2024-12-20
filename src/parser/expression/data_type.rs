use crate::{DataType, TokenType};

use super::Parser;
use crate::parser::errors::ParsingError;

/// Trait for parsing a [DataType](https://www.sqlite.org/syntax/type-name.html)
pub trait DataTypeParser {
    fn parse_data_type(&mut self) -> Result<DataType, ParsingError>;
}

impl<'a> DataTypeParser for Parser<'a> {
    fn parse_data_type(&mut self) -> Result<DataType, ParsingError> {
        let name = self.consume_as_id()?;

        if self.consume_as(TokenType::LeftParen).is_ok() {
            let lower_bound = self.parse_signed_number()?;

            if self.consume_as(TokenType::Comma).is_ok() {
                let upper_bound = self.parse_signed_number()?;

                // The right parenthesis must be in the last token in the cast expression
                self.consume_as(TokenType::RightParen)?;

                Ok(DataType::BoundedDataType(name, lower_bound, upper_bound))
            } else {
                self.consume_as(TokenType::RightParen)?;
                Ok(DataType::SizedDataType(name, lower_bound))
            }
        } else {
            Ok(DataType::PlainDataType(name))
        }
    }
}
