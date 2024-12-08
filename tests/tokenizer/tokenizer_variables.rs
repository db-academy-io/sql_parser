// THIS IS AN AUTOGENERATED TESTS, DO NOT MODIFY!

// The cargo dbcheck will generate the test files for each stage of the course.
// Be careful; these files are not intended to be edited by students. You're free
// to add more tests in separate files, but don't delete or edit tests generated
// by the testing tool

/* --------------- TESTS CREATED FROM TOKENIZER SPECIFICATION ---------------
 *             (see https://www.sqlite.org/draft/tokenreq.html)
 */
#![allow(non_snake_case)]

use super::{run_rainy_day_test, run_sunny_day_test};
use db_academy_sql_parser::{Keyword::*, TokenType, TokenizerError};

/// H40310: SQLite shall recognize as a VARIABLE token the a question-mark
/// (u003f) followed by zero or more NUMERIC characters.
#[test]
fn test_H40310() {
    let valid_test_cases = vec![
        ("?", vec![TokenType::Variable("?"), TokenType::Semi]),
        ("?1;", vec![TokenType::Variable("?1"), TokenType::Semi]),
        (
            "(?0, ?2)",
            vec![
                TokenType::LeftParen,
                TokenType::Variable("?0"),
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
    ];

    for (sql, expected_tokens) in valid_test_cases {
        run_sunny_day_test(sql, expected_tokens);
    }
}

#[test]
fn test_H40310_rainy_day_cases() {
    let invalid_test_cases = vec![
        ("?abc", vec![], TokenizerError::BadVariableName),
        ("?1abc", vec![], TokenizerError::BadVariableName),
        ("?$", vec![], TokenizerError::BadVariableName),
        ("?-1", vec![], TokenizerError::BadVariableName),
        ("?1.2", vec![], TokenizerError::BadVariableName),
        ("?123abc", vec![], TokenizerError::BadVariableName),
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
            "@name;",
            vec![TokenType::Variable("@name"), TokenType::Semi],
        ),
        (
            "($id, :name);",
            vec![
                TokenType::LeftParen,
                TokenType::Variable("$id"),
                TokenType::Comma,
                TokenType::Variable(":name"),
                TokenType::RightParen,
                TokenType::Semi,
            ],
        ),
        (
            "UPDATE users SET age = @age WHERE id = $id and email = :email;",
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
                TokenType::Keyword(And),
                TokenType::Id("email"),
                TokenType::Equals,
                TokenType::Variable(":email"),
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
        ("@;", vec![]),
        ("$;", vec![]),
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
        run_rainy_day_test(sql, expected_tokens, TokenizerError::BadVariableName);
    }
}
