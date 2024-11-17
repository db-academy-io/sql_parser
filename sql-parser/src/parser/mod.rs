mod drop;
mod errors;

pub use errors::*;
mod sqlite;

use std::iter::Peekable;

use crate::{Keyword, Statement, Token, TokenType, Tokenizer};
use drop::DropStatementParser;
use sqlite::SQLite3StatementParser;

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

    /// Peek the current token if it's not a [TokenType::SingleLineComment] or
    /// [TokenType::MultiLineComment], without advancing the underlaying iterator
    ///
    /// Advance the underlaying iterator until first non-comment token reached
    fn peek_token(&mut self) -> Result<Token<'a>, ParsingError> {
        match self.tokenizer.peek() {
            Some(result) => match result {
                Ok(token) => {
                    match token.token_type {
                        TokenType::SingleLineComment(_) | TokenType::MultiLineComment(_) => {
                            // Consume comment token
                            self.consume_token()?;
                            return self.peek_token();
                        }
                        _ => Ok(token.clone()),
                    }
                }
                Err(error) => Err(ParsingError::TokenizerError(error.to_string())),
            },
            None => Err(ParsingError::UnexpectedEOF),
        }
    }

    /// Peeks the current token and checks if it matches the specified `token_type`.
    /// If the current token's type does not match, a [ParsingError::UnexpectedToken] is thrown.
    /// If there is no any token in the stream, a [ParsingError::UnexpectedEOF] is thrown.
    fn peek_as(&mut self, token_type: TokenType) -> Result<Token, ParsingError> {
        let token = self.peek_token()?;

        if token.token_type == token_type {
            Ok(token)
        } else {
            Err(ParsingError::UnexpectedToken(token.to_string()))
        }
    }

    /// Peek the current token as a keyword without advancing the underlaying
    /// iterator. If there is a token which is not a [Keyword], it will
    /// throw an [ParsingError]  
    fn peek_as_keyword(&mut self) -> Result<Keyword, ParsingError> {
        let token = self.peek_token()?;
        token.try_into()
    }

    /// Peek the current token as a [TokenType::Id] without advancing the underlaying
    /// iterator. If there is a token which is not a [Keyword], it will
    /// throw an [ParsingError]  
    fn peek_as_id(&mut self) -> Result<&'a str, ParsingError> {
        let token = self.peek_token()?;

        if let TokenType::Id(id) = token.token_type {
            Ok(id)
        } else {
            Err(ParsingError::UnexpectedToken(token.to_string()))
        }
    }

    /// Peek the current token as a [TokenType::String] without advancing the underlaying
    /// iterator. If there is a token which is not a [Keyword], it will throw an [ParsingError]
    fn peek_as_string(&mut self) -> Result<String, ParsingError> {
        let token = self.peek_token()?;

        if let TokenType::String(string) = token.token_type {
            Ok(string.to_string())
        } else {
            Err(ParsingError::UnexpectedToken(token.to_string()))
        }
    }

    /// Check if the current token is end of the statement. If it is not throw a [ParsingError]
    fn is_end_of_statement(&mut self) -> Result<(), ParsingError> {
        let token = self.peek_token()?;
        match token.token_type {
            TokenType::Semi => {
                // consume the ';' token
                self.consume_token()?;
                Ok(())
            }
            _ => Err(ParsingError::UnexpectedToken(token.to_string())),
        }
    }

    /// Parse a single statement from the tokenizer [Tokenizer]
    pub fn parse_statement(&mut self) -> Result<Statement, ParsingError> {
        match self.peek_as_keyword()? {
            Keyword::Drop => self.parse_drop_statement(),
            Keyword::Vacuum => self.parse_vacuum_statement(),
            Keyword::Detach => self.parse_detach_statement(),
            keyword => Err(ParsingError::UnexpectedKeyword(keyword)),
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
