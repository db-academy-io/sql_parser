pub mod begin;
pub mod commit;
pub mod release;
pub mod rollback;
pub mod savepoint;

use begin::BeginStatementParser;
use commit::CommitStatementParser;
use release::ReleaseStatementParser;
use rollback::RollbackStatementParser;
use savepoint::SavepointStatementParser;

use super::Parser;
use crate::parser::errors::ParsingError;
use crate::Statement;

/// Defines the interface for parsing SQL transaction statements
pub trait TransactionStatementParser {
    /// Parses a BEGIN statement with optional transaction type
    fn parse_begin_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a COMMIT statement
    fn parse_commit_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a ROLLBACK statement
    fn parse_rollback_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a RELEASE statement
    fn parse_release_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a SAVEPOINT statement
    fn parse_savepoint_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> TransactionStatementParser for Parser<'a> {
    fn parse_begin_statement(&mut self) -> Result<Statement, ParsingError> {
        BeginStatementParser::parse_begin_statement(self)
    }

    fn parse_commit_statement(&mut self) -> Result<Statement, ParsingError> {
        CommitStatementParser::parse_commit_statement(self)
    }

    fn parse_rollback_statement(&mut self) -> Result<Statement, ParsingError> {
        RollbackStatementParser::parse_rollback_statement(self)
    }

    fn parse_release_statement(&mut self) -> Result<Statement, ParsingError> {
        ReleaseStatementParser::parse_release_statement(self)
    }

    fn parse_savepoint_statement(&mut self) -> Result<Statement, ParsingError> {
        SavepointStatementParser::parse_savepoint_statement(self)
    }
}
