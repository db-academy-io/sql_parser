mod analyze;
mod attach;
mod pragma;
mod reindex;
mod sqlite3_name;
mod vacuum;

use analyze::AnalyzeStatementParser;
use attach::AttachStatementParser;
use pragma::PragmaStatementParser;
use reindex::ReindexStatementParser;
use vacuum::VacuumStatementParser;

use super::Parser;
use crate::{ParsingError, Statement};

pub trait SQLite3StatementParser {
    /// Parses a VACUUM statement
    fn parse_vacuum_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a DETACH statement
    fn parse_detach_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a ATTACH statement
    fn parse_attach_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses an ANALYZE statement
    fn parse_analyze_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a REINDEX statement
    fn parse_reindex_statement(&mut self) -> Result<Statement, ParsingError>;

    /// Parses a PRAGMA statement
    fn parse_pragma_statement(&mut self) -> Result<Statement, ParsingError>;
}

impl<'a> SQLite3StatementParser for Parser<'a> {
    fn parse_vacuum_statement(&mut self) -> Result<Statement, ParsingError> {
        VacuumStatementParser::parse_vacuum_statement(self)
    }

    fn parse_detach_statement(&mut self) -> Result<Statement, ParsingError> {
        AttachStatementParser::parse_detach_statement(self)
    }

    fn parse_attach_statement(&mut self) -> Result<Statement, ParsingError> {
        AttachStatementParser::parse_attach_statement(self)
    }

    fn parse_analyze_statement(&mut self) -> Result<Statement, ParsingError> {
        AnalyzeStatementParser::parse_analyze_statement(self)
    }

    fn parse_reindex_statement(&mut self) -> Result<Statement, ParsingError> {
        ReindexStatementParser::parse_reindex_statement(self)
    }

    fn parse_pragma_statement(&mut self) -> Result<Statement, ParsingError> {
        PragmaStatementParser::parse_pragma_statement(self)
    }
}
