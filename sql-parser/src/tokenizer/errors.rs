use std::fmt::Display;

use crate::{Keyword, Token};

/// The [ParsingError] enum represents the various types of errors that can
/// occur during the parsing of SQL input. Each variant corresponds to a
/// specific parsing error that the parser might encounter.

#[derive(Debug, PartialEq, Clone)]
pub enum TokenizerError<'input> {
    /// Token is not recognized by the parser
    UnrecognizedToken,

    /// Unexpected end of input encountered
    UnexpectedEOF,

    /// String literal is not properly closed
    UnterminatedLiteral(&'input str),

    /// Bracket is not properly closed
    UnterminatedBracket,

    /// Block comment is not properly closed
    UnterminatedCommentBlock,

    /// Variable name is invalid or malformed
    BadVariableName,

    /// Number is invalid or improperly formatted
    BadNumber,

    /// Expected an equals sign but found something else
    ExpectedEqualsSign,

    /// Blob literal is malformed (&str, position)
    MalformedBlobLiteral(&'input str, usize),

    /// Hexadecimal integer is malformed
    MalformedHexInteger,

    /// An empty ID is given
    EmptyId,

    /// Expected a [Keyword], something else was given
    ExpectedKeyword(Keyword),

    /// Unexpected [Keyword]
    UnexpectedKeyword(Keyword),

    /// Unexpected token
    UnexpectedToken(Token<'input>),
}

impl<'a> Display for TokenizerError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenizerError::UnrecognizedToken => write!(f, "UnrecognizedToken"),
            TokenizerError::UnexpectedEOF => write!(f, "UnexpectedEOF"),
            TokenizerError::UnterminatedLiteral(literal) => {
                write!(f, "UnterminatedLiteral: {}", literal)
            }
            TokenizerError::UnterminatedBracket => write!(f, "UnterminatedBracket"),
            TokenizerError::UnterminatedCommentBlock => write!(f, "UnterminatedCommentBlock"),
            TokenizerError::BadVariableName => write!(f, "BadVariableName"),
            TokenizerError::BadNumber => write!(f, "BadNumber"),
            TokenizerError::ExpectedEqualsSign => write!(f, "ExpectedEqualsSign"),
            TokenizerError::MalformedBlobLiteral(blob, pos) => {
                write!(f, "MalformedBlobLiteral {blob} at {pos}")
            }
            TokenizerError::MalformedHexInteger => write!(f, "MalformedHexInteger"),
            TokenizerError::EmptyId => write!(f, "EmptyId"),
            TokenizerError::ExpectedKeyword(keyword) => write!(f, "ExpectedKeyword {keyword}"),
            TokenizerError::UnexpectedKeyword(keyword) => write!(f, "UnexpectedKeyword {keyword}"),
            TokenizerError::UnexpectedToken(token) => write!(f, "UnexpectedToken {token}"),
        }
    }
}
