use crate::Keyword;

/// The [ParsingError] enum represents the various types of errors that can
/// occur during the parsing of SQL input. Each variant corresponds to a
/// specific parsing error that the parser might encounter.

#[derive(Debug, PartialEq, Clone)]
pub enum ParsingError {
    /// Unexpected EndOfFile
    UnexpectedEOF,

    /// Expected a [Keyword], something else was given
    ExpectedKeyword(Keyword),

    /// Unexpected [Keyword]
    UnexpectedKeyword(Keyword),

    /// Unexpected token
    UnexpectedToken(String),

    /// Tokenizer error
    TokenizerError(String),

    /// Unexpected parsing state
    UnexpectedParsingState,
}
