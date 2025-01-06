use std::fmt::Display;

use crate::{errors::ParsingError, Keyword, TokenType};

/// A unary operation
#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    /// Plus (+)
    Plus,

    /// Minus (-)
    Minus,

    /// Not (!) or NOT
    Not,

    /// Bitwise NOT (~)
    BitNot,
}

/// A binary operation
#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    /// Addition (+)
    Plus,
    /// Subtraction (-)
    Minus,
    /// Multiplication (*)
    Mul,
    /// Division (/)
    Div,
    /// Modulo (%)
    Remainder,
    /// And (AND)
    And,
    /// Or (OR)
    Or,
    /// Greater than (>)
    GreaterThan,
    /// Greater than or equal (>=)
    GreaterThanOrEquals,
    /// Less than (<)
    LessThan,
    /// Less than or equal (<=)
    LessThanOrEquals,
    /// Equals (=)
    Equals,
    /// Equals equals (==)
    EqualsEquals,
    /// Not equals (!=)
    NotEquals,
    /// Concatenation (||)
    Concat,
    /// Bitwise AND (&)
    BitAnd,
    /// Bitwise OR (|)
    BitOr,
    /// Left shift (<<)
    LeftShift,
    /// Right shift (>>)
    RightShift,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::And => write!(f, "AND"),
            BinaryOp::Or => write!(f, "OR"),
            BinaryOp::Remainder => write!(f, "%"),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::GreaterThanOrEquals => write!(f, ">="),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::LessThanOrEquals => write!(f, "<="),
            BinaryOp::Equals => write!(f, "="),
            BinaryOp::EqualsEquals => write!(f, "=="),
            BinaryOp::NotEquals => write!(f, "!="),
            BinaryOp::Concat => write!(f, "||"),
            BinaryOp::BitAnd => write!(f, "&"),
            BinaryOp::BitOr => write!(f, "|"),
            BinaryOp::LeftShift => write!(f, "<<"),
            BinaryOp::RightShift => write!(f, ">>"),
        }
    }
}

impl<'a> TryFrom<&'a TokenType<'a>> for BinaryOp {
    type Error = ParsingError;

    fn try_from(token_type: &'a TokenType<'a>) -> Result<Self, Self::Error> {
        match token_type {
            TokenType::Plus => Ok(BinaryOp::Plus),
            TokenType::Minus => Ok(BinaryOp::Minus),
            TokenType::Star => Ok(BinaryOp::Mul),
            TokenType::Slash => Ok(BinaryOp::Div),
            TokenType::Remainder => Ok(BinaryOp::Remainder),
            TokenType::Keyword(Keyword::And) => Ok(BinaryOp::And),
            TokenType::Keyword(Keyword::Or) => Ok(BinaryOp::Or),
            TokenType::GreaterThan => Ok(BinaryOp::GreaterThan),
            TokenType::GreaterEquals => Ok(BinaryOp::GreaterThanOrEquals),
            TokenType::LessThan => Ok(BinaryOp::LessThan),
            TokenType::LessEquals => Ok(BinaryOp::LessThanOrEquals),
            TokenType::Equals => Ok(BinaryOp::Equals),
            TokenType::EqualsEquals => Ok(BinaryOp::EqualsEquals),
            TokenType::NotEquals => Ok(BinaryOp::NotEquals),
            TokenType::Concat => Ok(BinaryOp::Concat),
            TokenType::BitAnd => Ok(BinaryOp::BitAnd),
            TokenType::BitOr => Ok(BinaryOp::BitOr),
            TokenType::LeftShift => Ok(BinaryOp::LeftShift),
            TokenType::RightShift => Ok(BinaryOp::RightShift),
            _ => Err(ParsingError::UnexpectedTokenType(format!(
                "Unexpected token type: {}",
                token_type
            ))),
        }
    }
}
