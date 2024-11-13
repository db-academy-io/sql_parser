mod drop;
mod errors;

pub use errors::*;
use std::iter::Peekable;

use crate::{Keyword, Statement, Token, TokenType, Tokenizer};
// use crate::parser::errors::ParsingError;
use drop::DropStatementParser;

pub struct Parser<'a> {
    tokenizer: Peekable<Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    /// Consume the current token and peek the next one
    fn consume_token(&mut self) -> Result<(), ParsingError> {
        match self.tokenizer.next() {
            Some(_token) => Ok(()),
            None => Err(ParsingError::UnexpectedEOF),
        }
    }

    /// Peek the current token without advancing the underlaying iterator
    fn peek_token(&mut self) -> Result<Token<'a>, ParsingError> {
        match self.tokenizer.peek() {
            Some(result) => match result {
                Ok(token) => Ok(token.clone()),
                Err(error) => Err(ParsingError::TokenizerError(error.to_string())),
            },
            None => Err(ParsingError::UnexpectedEOF),
        }
    }

    /// Peek the current token as a keyword without advancing the underlaying
    /// iterator. If there is a token which is not a [Keyword], it will
    /// throw an [ParsingError]  
    fn peek_as_keyword(&mut self) -> Result<Keyword, ParsingError> {
        if let Some(token_result) = self.tokenizer.peek() {
            match token_result {
                Ok(token) => Ok(token.try_into()?),
                Err(error) => Err(ParsingError::TokenizerError(error.to_string())),
            }
        } else {
            Err(ParsingError::UnexpectedEOF)
        }
    }

    /// Peek the current token as a [TokenType::Id] without advancing the underlaying
    /// iterator. If there is a token which is not a [Keyword], it will
    /// throw an [ParsingError]  
    fn peek_as_id(&mut self) -> Result<&'a str, ParsingError> {
        if let Some(token_result) = self.tokenizer.peek() {
            match token_result {
                Ok(token) => {
                    match token.token_type {
                        TokenType::Id(id) => Ok(id),
                        _ => Err(ParsingError::UnexpectedEOF), // TODO: Make proper error code
                    }
                }
                Err(error) => Err(ParsingError::TokenizerError(error.to_string())),
            }
        } else {
            Err(ParsingError::UnexpectedEOF)
        }
    }

    /// Parse a single statement from the tokenizer [Tokenizer]
    pub fn parse_statement(&mut self) -> Result<Statement, ParsingError> {
        match self.peek_as_keyword()? {
            Keyword::Drop => self.parse_drop_statement(),
            _ => todo!(),
        }
    }
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            tokenizer: Tokenizer::from(value).peekable(),
        }
    }
}
