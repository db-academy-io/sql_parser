// THIS IS AN AUTOGENERATED TESTS, DO NOT MODIFY!

// The cargo dbcheck will generate the test files for each stage of the course.
// Be careful; these files are not intended to be edited by students. You're free
// to add more tests in separate files, but don't delete or edit tests generated
// by the testing tool

/* --------------- TESTS DERIVED FROM TOKENIZER SPECIFICATION ---------------
 *             (see https://www.sqlite.org/draft/tokenreq.html)
 */
#![allow(non_snake_case)]

mod common;
use common::{run_rainy_day_test, run_sunny_day_test};
use sql_parser::Keyword::*;
use sql_parser::ParsingError;
use sql_parser::TokenType;
// todo!()

/// H40310: SQLite shall recognize as a VARIABLE token the a question-mark
/// (u003f) followed by zero or more NUMERIC characters.
#[test]
fn test_H40310() {
    let valid_test_cases = vec![
        (
            "SELECT * FROM users WHERE id = ?;",
            vec![
                TokenType::Keyword(Select),
                TokenType::Star,
                TokenType::Keyword(From),
                TokenType::Id("users"),
                TokenType::Keyword(Where),
                TokenType::Id("id"),
                TokenType::Equals,
                TokenType::Variable("?"),
                TokenType::Semi,
            ],
        ),
        (
            "SELECT * FROM users WHERE name = ?1;",
            vec![
                TokenType::Keyword(Select),
                TokenType::Star,
                TokenType::Keyword(From),
                TokenType::Id("users"),
                TokenType::Keyword(Where),
                TokenType::Id("name"),
                TokenType::Equals,
                TokenType::Variable("?1"),
                TokenType::Semi,
            ],
        ),
        (
            "INSERT INTO users (id, name) VALUES (?, ?2);",
            vec![
                TokenType::Keyword(Insert),
                TokenType::Keyword(Into),
                TokenType::Id("users"),
                TokenType::LeftParen,
                TokenType::Id("id"),
                TokenType::Comma,
                TokenType::Id("name"),
                TokenType::RightParen,
                TokenType::Keyword(Values),
                TokenType::LeftParen,
                TokenType::Variable("?"),
                TokenType::Comma,
                TokenType::Variable("?2"),
                TokenType::RightParen,
                TokenType::Semi,
            ],
        ),
        (
            "UPDATE users SET age = ?123 WHERE id = ?;",
            vec![
                TokenType::Keyword(Update),
                TokenType::Id("users"),
                TokenType::Keyword(Set),
                TokenType::Id("age"),
                TokenType::Equals,
                TokenType::Variable("?123"),
                TokenType::Keyword(Where),
                TokenType::Id("id"),
                TokenType::Equals,
                TokenType::Variable("?"),
                TokenType::Semi,
            ],
        ),
        (
            "DELETE FROM users WHERE id = ?0;",
            vec![
                TokenType::Keyword(Delete),
                TokenType::Keyword(From),
                TokenType::Id("users"),
                TokenType::Keyword(Where),
                TokenType::Id("id"),
                TokenType::Equals,
                TokenType::Variable("?0"),
                TokenType::Semi,
            ],
        ),
    ];

    for (sql, expected_tokens) in valid_test_cases {
        run_sunny_day_test(sql, expected_tokens);
    }
}

#[test]
fn test_H40310_rainy_day_cases() {
    let invalid_test_cases = vec![
        ("?abc", vec![], ParsingError::BadVariableName),
        ("?1abc", vec![], ParsingError::BadVariableName),
        ("?$", vec![], ParsingError::BadVariableName),
        ("?-1", vec![], ParsingError::BadVariableName),
        ("?1.2", vec![], ParsingError::BadVariableName),
        ("?123abc", vec![], ParsingError::BadVariableName),
    ];

    for (sql, tokens, expected_error) in invalid_test_cases {
        run_rainy_day_test(sql, tokens, expected_error);
    }
}

/// H40320: SQLite shall recognize as a VARIABLE token one of the characters
/// at-sign (u0040), dollar-sign (u0024), or colon (u003a) followed by a parameter name.
#[test]
fn test_H40320() {
    let valid_test_cases = vec![
        (
            "SELECT * FROM users WHERE name = @name;",
            vec![
                TokenType::Keyword(Select),
                TokenType::Star,
                TokenType::Keyword(From),
                TokenType::Id("users"),
                TokenType::Keyword(Where),
                TokenType::Id("name"),
                TokenType::Equals,
                TokenType::Variable("@name"),
                TokenType::Semi,
            ],
        ),
        (
            "INSERT INTO users (id, name) VALUES ($id, :name);",
            vec![
                TokenType::Keyword(Insert),
                TokenType::Keyword(Into),
                TokenType::Id("users"),
                TokenType::LeftParen,
                TokenType::Id("id"),
                TokenType::Comma,
                TokenType::Id("name"),
                TokenType::RightParen,
                TokenType::Keyword(Values),
                TokenType::LeftParen,
                TokenType::Variable("$id"),
                TokenType::Comma,
                TokenType::Variable(":name"),
                TokenType::RightParen,
                TokenType::Semi,
            ],
        ),
        (
            "UPDATE users SET age = @age WHERE id = $id;",
            vec![
                TokenType::Keyword(Update),
                TokenType::Id("users"),
                TokenType::Keyword(Set),
                TokenType::Id("age"),
                TokenType::Equals,
                TokenType::Variable("@age"),
                TokenType::Keyword(Where),
                TokenType::Id("id"),
                TokenType::Equals,
                TokenType::Variable("$id"),
                TokenType::Semi,
            ],
        ),
        (
            "DELETE FROM users WHERE id = :id;",
            vec![
                TokenType::Keyword(Delete),
                TokenType::Keyword(From),
                TokenType::Id("users"),
                TokenType::Keyword(Where),
                TokenType::Id("id"),
                TokenType::Equals,
                TokenType::Variable(":id"),
                TokenType::Semi,
            ],
        ),
    ];

    for (sql, expected_tokens) in valid_test_cases {
        run_sunny_day_test(sql, expected_tokens);
    }
}

#[test]
fn test_H40320_rainy_day() {
    let invalid_test_cases = vec![
        (
            "SELECT * FROM users WHERE name = @;",
            vec![
                TokenType::Keyword(Select),
                TokenType::Star,
                TokenType::Keyword(From),
                TokenType::Id("users"),
                TokenType::Keyword(Where),
                TokenType::Id("name"),
                TokenType::Equals,
            ],
        ),
        (
            "SELECT * FROM users WHERE name = $;",
            vec![
                TokenType::Keyword(Select),
                TokenType::Star,
                TokenType::Keyword(From),
                TokenType::Id("users"),
                TokenType::Keyword(Where),
                TokenType::Id("name"),
                TokenType::Equals,
            ],
        ),
        (
            "SELECT * FROM users WHERE name = : ;",
            vec![
                TokenType::Keyword(Select),
                TokenType::Star,
                TokenType::Keyword(From),
                TokenType::Id("users"),
                TokenType::Keyword(Where),
                TokenType::Id("name"),
                TokenType::Equals,
            ],
        ),
    ];

    for (sql, expected_tokens) in invalid_test_cases {
        run_rainy_day_test(sql, expected_tokens, ParsingError::BadVariableName);
    }
}
