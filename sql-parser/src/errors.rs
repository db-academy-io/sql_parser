use crate::{Keyword, Token};

/// The [ParsingError] enum represents the various types of errors that can
/// occur during the parsing of SQL input. Each variant corresponds to a
/// specific parsing error that the parser might encounter.

#[derive(Debug, PartialEq, Clone)]
pub enum ParsingError<'input> {
    /// Token is not recognized by the parser
    UnrecognizedToken,

    /// Unexpected end of input encountered
    UnexpectedEOF,

    /// String literal is not properly closed
    UnterminatedLiteral(&'input str),

    /// Bracket is not properly closed
    UnterminatedBracket,

    /// Block comment is not properly closed
    UnterminatedBlockComment,

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
