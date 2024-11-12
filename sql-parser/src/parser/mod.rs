mod drop_statement_parser;
use std::iter::Peekable;

use crate::{Keyword, ParsingError, Statement, Token, TokenType, Tokenizer};

use drop_statement_parser::DropTableStatementParser;

pub struct Parser<'a> {
    tokenizer: Peekable<Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    /// Consume the current token and peek the next one
    fn consume_token(&mut self) -> Result<(), ParsingError<'a>> {
        match self.tokenizer.next() {
            Some(_token) => Ok(()),
            None => Err(ParsingError::UnexpectedEOF),
        }
    }

    /// Peek the current token without advancing the underlaying iterator
    fn peek_token(&mut self) -> Result<Token<'a>, ParsingError<'a>> {
        match self.tokenizer.peek() {
            Some(result) => match result {
                Ok(token) => Ok(token.clone()),
                Err(error) => Err(error.clone()),
            },
            None => Err(ParsingError::UnexpectedEOF),
        }
    }

    /// Peek the current token as a keyword without advancing the underlaying
    /// iterator. If there is a token which is not a [Keyword], it will
    /// throw an [ParsingError]  
    fn peek_as_keyword(&mut self) -> Result<Keyword, ParsingError<'a>> {
        if let Some(token_result) = self.tokenizer.peek() {
            match token_result {
                Ok(token) => Ok(token.try_into()?),
                Err(err) => Err(err.clone()),
            }
        } else {
            Err(ParsingError::UnexpectedEOF)
        }
    }

    /// Peek the current token as a [TokenType::Id] without advancing the underlaying
    /// iterator. If there is a token which is not a [Keyword], it will
    /// throw an [ParsingError]  
    fn peek_as_id(&mut self) -> Result<&'a str, ParsingError<'a>> {
        if let Some(token_result) = self.tokenizer.peek() {
            match token_result {
                Ok(token) => {
                    match token.token_type {
                        TokenType::Id(id) => Ok(id),
                        _ => Err(ParsingError::UnexpectedEOF), // TODO: Make proper error code
                    }
                }
                Err(err) => Err(err.clone()),
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
