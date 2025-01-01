use once_cell::sync::Lazy;
use std::collections::HashMap;

use crate::{Keyword, TokenType};

/// The precedence of the operators.
///
/// The precedence of an operator is the maximum precedence of the expressions
/// that can be formed with the operator.
///
/// The source of the precedence values is the SQLite grammar, see:
/// [SQL Language Expressions](https://www.sqlite.org/lang_expr.html)
static PRECEDENCE: Lazy<HashMap<TokenType, u8>> = Lazy::new(|| {
    let pairs = [
        (TokenType::Concat, 60),
        (TokenType::Star, 50),
        (TokenType::Slash, 50),
        (TokenType::Remainder, 50),
        (TokenType::Plus, 40),
        (TokenType::Minus, 40),
        (TokenType::BitNot, 40),
        (TokenType::BitAnd, 30),
        (TokenType::BitOr, 30),
        (TokenType::LeftShift, 30),
        (TokenType::RightShift, 30),
        (TokenType::GreaterThan, 20),
        (TokenType::LessThan, 20),
        (TokenType::GreaterEquals, 20),
        (TokenType::LessEquals, 20),
        (TokenType::Equals, 10),
        (TokenType::EqualsEquals, 10),
        (TokenType::NotEquals, 10),
        (TokenType::Keyword(Keyword::Not), 5),
        (TokenType::Keyword(Keyword::Isnull), 5),
        (TokenType::Keyword(Keyword::Notnull), 5),
        (TokenType::Keyword(Keyword::Between), 5),
        (TokenType::Keyword(Keyword::Like), 5),
        (TokenType::Keyword(Keyword::Glob), 5),
        (TokenType::Keyword(Keyword::Regexp), 5),
        (TokenType::Keyword(Keyword::Match), 5),
        (TokenType::Keyword(Keyword::In), 5),
        (TokenType::Keyword(Keyword::Is), 5),
        (TokenType::Keyword(Keyword::And), 1),
        (TokenType::Keyword(Keyword::Or), 1),
    ];

    pairs.iter().cloned().collect()
});

pub fn get_precedence(operator: &TokenType) -> u8 {
    *PRECEDENCE.get(operator).unwrap_or(&0)
}
