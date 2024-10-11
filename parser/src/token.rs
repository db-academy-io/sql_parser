use crate::Keyword;

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Keyword(Keyword),

    String(&'a str),
    Id(&'a str),
    Variable(&'a str),
    Blob(&'a str),

    Integer(&'a str),
    Float(&'a str),

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
    Bitor,
    /// - H41472: SQLite shall recognize the 1-character sequenence "." (u002e) as token DOT
    Dot,

    // The factorial or not symbol !
    // The query parameter marker ?
    /// - H41427: SQLite shall recognize the 1-character sequenence "=" (u003d) as token EQ
    /// - H41430: SQLite shall recognize the 2-character sequenence "==" (u003d u003d) as token EQ
    Equals,
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
}
