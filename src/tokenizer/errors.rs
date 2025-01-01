use std::fmt::Display;

/// The [TokenizerError] enum represents the various error types of the
/// tokenizer that can occur during the parsing of SQL input. Each variant
/// corresponds to a specific error that the tokenizer might encounter.
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

    /// Number is invalid or formatted incorrectly
    BadNumber,

    /// Blob literal is malformed (&str, position)
    MalformedBlobLiteral(&'input str, usize),

    /// Hexadecimal integer is malformed
    MalformedHexInteger,

    /// An empty ID is given
    EmptyId,
}

impl Display for TokenizerError<'_> {
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
            TokenizerError::MalformedBlobLiteral(blob, pos) => {
                write!(f, "MalformedBlobLiteral {blob} at {pos}")
            }
            TokenizerError::MalformedHexInteger => write!(f, "MalformedHexInteger"),
            TokenizerError::EmptyId => write!(f, "EmptyId"),
        }
    }
}
