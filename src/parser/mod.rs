#[cfg(test)]
mod test_utils;

mod drop;
pub(crate) mod expression;
mod select;
mod sqlite;
mod trx;

mod errors;
use crate::{Keyword, Statement, Token, TokenType, Tokenizer};
use drop::DropStatementParser;
pub use errors::*;
use select::SelectStatementParser;
use sqlite::SQLite3StatementParser;
use std::iter::Peekable;
use trx::TransactionStatementParser;

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
        let value = match token.token_type {
            TokenType::String(value) | TokenType::Id(value) | TokenType::Variable(value) => {
                value.to_string()
            }
            _ => return Err(ParsingError::UnexpectedToken(token.to_string())), // TODO: Improve the error code
        };
        Ok(value)
    }

    /// Peek the current token as a [TokenType::String] without advancing the underlaying
    /// iterator. If there is a token which is not a [Keyword], it will throw an [ParsingError]
    fn peek_as_number(&mut self) -> Result<String, ParsingError> {
        let token = self.peek_token()?;
        let value = match token.token_type {
            // TokenType::Keyword(keyword_as_schema_name) => keyword_as_schema_name.to_string(),
            TokenType::Integer(value) | TokenType::Float(value) => value.to_string(),
            _ => return Err(ParsingError::UnexpectedToken(token.to_string())), // TODO: Improve the error code
        };
        Ok(value)
    }

    /// Check if the current token is end of the statement. Consume ';' token
    /// If it is not - throw a [ParsingError]
    fn finalize_statement_parsing(&mut self) -> Result<(), ParsingError> {
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
            Keyword::Analyze => self.parse_analyze_statement(),
            Keyword::Reindex => self.parse_reindex_statement(),
            Keyword::Begin => self.parse_begin_statement(),
            Keyword::Commit | Keyword::End => self.parse_commit_statement(),
            Keyword::Rollback => self.parse_rollback_statement(),
            Keyword::Release => self.parse_release_statement(),
            Keyword::Savepoint => self.parse_savepoint_statement(),
            Keyword::Pragma => self.parse_pragma_statement(),
            Keyword::Select => self.parse_select_statement(),
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
