use std::iter::Peekable;

mod alter;
mod column_definition;
mod drop;
mod errors;
pub(crate) mod expression;
mod select;
mod sqlite;
mod trx;
mod window_definition;

#[cfg(test)]
mod test_utils;

use crate::{Keyword, SelectStatement, Statement, Token, TokenType, Tokenizer};
use alter::AlterTableStatementParser;
use drop::DropStatementParser;
pub use errors::*;
use select::{SelectStatementParser, ValuesStatementParser};
use sqlite::SQLite3StatementParser;
use trx::TransactionStatementParser;

/// A parser for SQLite SQL statements
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

    /// Consume the current token if it matches the specified `token_type`.
    /// If the current token's type does not match, a [ParsingError::UnexpectedToken] is thrown.
    fn consume_as(&mut self, token_type: TokenType) -> Result<(), ParsingError> {
        let token = self.peek_token()?;
        if token.token_type == token_type {
            self.consume_token()?;
            Ok(())
        } else {
            Err(ParsingError::UnexpectedToken(token.to_string()))
        }
    }

    /// Consume the current token if it matches the specified `keyword`.
    /// If the current token's type does not match, a [ParsingError::UnexpectedToken] is thrown.
    fn consume_as_keyword(&mut self, keyword: Keyword) -> Result<(), ParsingError> {
        let token = self.peek_token()?;
        if token.token_type == TokenType::Keyword(keyword) {
            self.consume_token()?;
            Ok(())
        } else {
            Err(ParsingError::UnexpectedToken(format!(
                "Expected {} keyword, got: {}",
                keyword, token
            )))
        }
    }

    /// Consume the current token if it matches the [TokenType::Id]
    /// If the current token's type does not match, a [ParsingError::UnexpectedToken] is thrown.
    fn consume_as_id(&mut self) -> Result<String, ParsingError> {
        let token = self.peek_token()?;
        if let TokenType::Id(id) = token.token_type {
            self.consume_token()?;
            Ok(id.to_string())
        } else {
            Err(ParsingError::UnexpectedToken(token.to_string()))
        }
    }

    /// Consume the current token if it matches the [TokenType::String]
    /// If the current token's type does not match, a [ParsingError::UnexpectedToken] is thrown.
    fn consume_as_string(&mut self) -> Result<(), ParsingError> {
        let token = self.peek_token()?;
        if let TokenType::String(_) = token.token_type {
            self.consume_token()?;
            Ok(())
        } else {
            Err(ParsingError::UnexpectedToken(token.to_string()))
        }
    }

    /// Consume the current token if it matches the [TokenType::Integer] or [TokenType::Float]
    /// If the current token's type does not match, a [ParsingError::UnexpectedToken] is thrown.
    fn consume_as_number(&mut self) -> Result<String, ParsingError> {
        let token = self.peek_token()?;
        if let TokenType::Integer(value) | TokenType::Float(value) = token.token_type {
            self.consume_token()?;
            Ok(value.to_string())
        } else {
            Err(ParsingError::UnexpectedToken(token.to_string()))
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
    /// iterator. If there is a token which is not a [TokenType::Id], it will
    /// throw an [ParsingError]  
    fn peek_as_id(&mut self) -> Result<&'a str, ParsingError> {
        let token = self.peek_token()?;

        if let TokenType::Id(id) = token.token_type {
            Ok(id)
        } else {
            Err(ParsingError::UnexpectedToken(token.to_string()))
        }
    }

    /// Peek the current token as a [TokenType::Id] or [TokenType::Star] without advancing the underlaying
    /// iterator. If there is a token which is not a [TokenType::Id] or [TokenType::Star], it will
    /// throw an [ParsingError]  
    fn peek_as_id_or_star(&mut self) -> Result<&'a str, ParsingError> {
        let token = self.peek_token()?;

        if let TokenType::Id(id) = token.token_type {
            Ok(id)
        } else if token.token_type == TokenType::Star {
            Ok("*")
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

    /// Parse a signed number
    fn parse_signed_number(&mut self) -> Result<String, ParsingError> {
        // ignore the leading '+'
        let _ = self.consume_as(TokenType::Plus);
        let minus_sign = self.consume_as(TokenType::Minus).map(|_| "-").unwrap_or("");
        let number = self.consume_as_number()?;
        Ok(format!("{}{}", minus_sign, number))
    }

    /// Parse a single statement from the tokenizer [Tokenizer]
    pub fn parse_statement(&mut self) -> Result<Statement, ParsingError> {
        match self.peek_as_keyword()? {
            Keyword::Drop => DropStatementParser::parse_drop_statement(self),
            Keyword::Vacuum => SQLite3StatementParser::parse_vacuum_statement(self),
            Keyword::Attach => SQLite3StatementParser::parse_attach_statement(self),
            Keyword::Detach => SQLite3StatementParser::parse_detach_statement(self),
            Keyword::Analyze => SQLite3StatementParser::parse_analyze_statement(self),
            Keyword::Reindex => SQLite3StatementParser::parse_reindex_statement(self),
            Keyword::Pragma => SQLite3StatementParser::parse_pragma_statement(self),
            Keyword::Begin => TransactionStatementParser::parse_begin_statement(self),
            Keyword::Commit | Keyword::End => {
                TransactionStatementParser::parse_commit_statement(self)
            }
            Keyword::Rollback => TransactionStatementParser::parse_rollback_statement(self),
            Keyword::Release => TransactionStatementParser::parse_release_statement(self),
            Keyword::Savepoint => TransactionStatementParser::parse_savepoint_statement(self),
            Keyword::Select => {
                SelectStatementParser::parse_select_statement(self).map(Statement::Select)
            }
            Keyword::Values => ValuesStatementParser::parse_values_statement(self)
                .map(|stmt| Statement::Select(SelectStatement::Values(stmt))),

            Keyword::Alter => AlterTableStatementParser::parse_alter_table_statement(self)
                .map(Statement::AlterTable),
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
