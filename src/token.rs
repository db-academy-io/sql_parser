use std::fmt::Display;

use crate::{parser::errors::ParsingError, Keyword};

/// Represents a single lexical token identified by the tokenizer.
///
/// This struct contains the type of the token and its position within
/// the input stream
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token<'a> {
    /// The specific type of the token
    pub token_type: TokenType<'a>,

    /// The position (e.g., byte index) of the token in the input string
    /// This is useful for error reporting and tracking the token's location
    pub position: usize,
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at position {}", self.token_type, self.position)
    }
}

impl<'a> TryInto<Keyword> for Token<'a> {
    type Error = ParsingError;

    fn try_into(self) -> Result<Keyword, Self::Error> {
        match self.token_type {
            TokenType::Keyword(keyword) => Ok(keyword),
            _ => Err(ParsingError::UnexpectedToken(self.to_string())),
        }
    }
}

/// Enumeration of all possible token types that the tokenizer can recognize
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenType<'a> {
    /// Represents a SQL keyword, such as SELECT, FROM, WHERE etc.
    /// See [Keyword] for details
    Keyword(Keyword),

    /// Represents a string literal, enclosed in quotes
    String(&'a str),

    /// Represents an identifier, such as table or column names
    Id(&'a str),

    /// Represents a variable, prefixed with a special character
    /// (e.g., `$variable`).
    Variable(&'a str),

    /// Represents a blob literal, used for binary data
    Blob(&'a str),

    /// Represents an integer literal
    Integer(&'a str),

    /// Represents a floating-point number literal
    Float(&'a str),

    /// Represents a single-line comment starting with `--`
    SingleLineComment(&'a str),

    /// Represents a multi-line comment, enclosed between `/*` and `*/`
    MultiLineComment(&'a str),

    // Operator tokens
    // The following sequences of special characters are recognized as tokens:
    /// - H41415: SQLite shall recognize the 1-character sequenence "+" (u002b) as token PLUS
    Plus,
    /// - H41403: SQLite shall recognize the 1-character sequenence "-" (u002d) as token MINUS
    Minus,
    /// - H41418: SQLite shall recognize the 1-character sequenence "*" (u002a) as token STAR
    Star,
    /// - H41421: SQLite shall recognize the 1-character sequenence "/" (u002f) as token SLASH
    Slash,
    /// - H41424: SQLite shall recognize the 1-character sequenence "%" (u0025) as token REM
    Remainder,
    /// - H41406: SQLite shall recognize the 1-character sequenence "(" (u0028) as token LP
    LeftParen,
    /// - H41409: SQLite shall recognize the 1-character sequenence ")" (u0029) as token RP
    RightParen,
    /// - H41412: SQLite shall recognize the 1-character sequenence ";" (u003b) as token SEMI
    Semi,
    /// - H41442: SQLite shall recognize the 1-character sequenence "<" (u003c) as token LT
    LessThan,
    /// - H41451: SQLite shall recognize the 1-character sequenence ">" (u003e) as token GT
    GreaterThan,
    /// - H41457: SQLite shall recognize the 1-character sequenence "," (u002c) as token COMMA
    Comma,
    /// - H41460: SQLite shall recognize the 1-character sequenence "&" (u0026) as token BITAND
    BitAnd,
    /// - H41463: SQLite shall recognize the 1-character sequenence "~" (u007e) as token BITNOT
    BitNot,
    /// - H41466: SQLite shall recognize the 1-character sequenence "|" (u007c) as token BITOR
    BitOr,
    /// - H41472: SQLite shall recognize the 1-character sequenence "." (u002e) as token DOT
    Dot,

    // The factorial or not symbol !
    // The query parameter marker ?
    /// - H41427: SQLite shall recognize the 1-character sequenence "=" (u003d) as token EQ
    Equals,
    /// - H41430: SQLite shall recognize the 2-character sequenence "==" (u003d u003d) as token EQ
    EqualsEquals,
    /// - H41433: SQLite shall recognize the 2-character sequenence "<=" (u003c u003d) as token LE
    LessEquals,
    /// - H41436: SQLite shall recognize the 2-character sequenence "<>" (u003c u003e) as token NE
    /// - H41454: SQLite shall recognize the 2-character sequenence "!=" (u0021 u003d) as token NE
    NotEquals,
    /// - H41439: SQLite shall recognize the 2-character sequenence "<<" (u003c u003c) as token LSHIFT
    LeftShift,
    /// - H41445: SQLite shall recognize the 2-character sequenence ">=" (u003e u003d) as token GE
    GreaterEquals,
    /// - H41448: SQLite shall recognize the 2-character sequenence ">>" (u003e u003e) as token RSHIFT
    RightShift,
    /// - H41469: SQLite shall recognize the 2-character sequenence "||" (u007c u007c) as token CONCAT
    Concat,

    /// Represents the true literal value
    True,
    /// Represents the false literal value
    False,
}

impl<'a> Display for TokenType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Keyword(keyword) => keyword.fmt(f),
            TokenType::String(string) => write!(f, "{}", string),
            TokenType::Id(id) => write!(f, "{}", id),
            TokenType::Variable(var) => write!(f, "{}", var),
            TokenType::Blob(blob) => write!(f, "{}", blob),
            TokenType::Integer(int) => write!(f, "{}", int),
            TokenType::Float(float) => write!(f, "{}", float),
            TokenType::SingleLineComment(comment) => write!(f, "{}", comment),
            TokenType::MultiLineComment(comment) => write!(f, "{}", comment),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Star => write!(f, "*"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Remainder => write!(f, "%"),
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::Semi => write!(f, ";"),
            TokenType::LessThan => write!(f, "<"),
            TokenType::GreaterThan => write!(f, ">"),
            TokenType::Comma => write!(f, ","),
            TokenType::BitAnd => write!(f, "&"),
            TokenType::BitNot => write!(f, "~"),
            TokenType::BitOr => write!(f, "|"),
            TokenType::Dot => write!(f, "."),
            TokenType::Equals => write!(f, "="),
            TokenType::EqualsEquals => write!(f, "=="),
            TokenType::LessEquals => write!(f, "<="),
            TokenType::NotEquals => write!(f, "!="),
            TokenType::LeftShift => write!(f, "<<"),
            TokenType::GreaterEquals => write!(f, ">="),
            TokenType::RightShift => write!(f, ">>"),
            TokenType::Concat => write!(f, "||"),
            TokenType::True => write!(f, "true"),
            TokenType::False => write!(f, "false"),
        }
    }
}
