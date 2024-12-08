#![allow(unused)]

mod tokenizer_blobs;
mod tokenizer_general;
mod tokenizer_identifiers;
mod tokenizer_keywords;
mod tokenizer_numbers;
mod tokenizer_operators;
mod tokenizer_strings;
mod tokenizer_variables;
mod tokenizer_whitespaces;

use db_academy_sql_parser::{Token, TokenType, Tokenizer, TokenizerError};

pub fn run_sunny_day_test<'a>(sql: &'a str, expected_tokens: Vec<TokenType<'a>>) {
    let mut tokenizer = Tokenizer::from(sql);

    for expected_token_type in expected_tokens {
        let token = tokenizer.next_token();
        let token = token.expect("Expected a token, but got None");
        let token = token.unwrap_or_else(|_| {
            panic!("Expected {:?}, got Unexpected error: ", expected_token_type)
        });

        // Verify that the token type matches the expected token
        assert_eq!(
            token.token_type, expected_token_type,
            "Expected token {:?}, got {:?}",
            expected_token_type, token.token_type
        );
    }

    // Ensure there are no more tokens
    assert!(
        tokenizer.next_token().is_none(),
        "Tokenizer should have no more tokens"
    );
}

pub fn run_rainy_day_test<'a>(
    sql: &'a str,
    expected_tokens: Vec<TokenType<'a>>,
    expected_error: TokenizerError,
) {
    let mut tokenizer = Tokenizer::from(sql);

    // We expect tokens up until the comment, then an error
    for expected_token_type in expected_tokens {
        let token = tokenizer.next_token();
        let token = token.expect("Expected a token, but got None");
        let token = token.unwrap_or_else(|_| {
            panic!("Expected {:?}, got Unexpected error: ", expected_token_type)
        });

        assert_eq!(
            token.token_type, expected_token_type,
            "Expected token {:?}, got {:?}",
            expected_token_type, token.token_type
        );
    }

    // The next call to next_token() should return an error
    let token: Option<Result<Token<'_>, TokenizerError<'_>>> = tokenizer.next_token();
    let token = token.expect("Expected a token, but got None");
    let token_status = token.expect_err("Expected an error, but got a token");

    if token_status != expected_error {
        assert_eq!(
            token_status, expected_error,
            "Expected {:?}, got {:?}",
            expected_error, token_status,
        )
    }
}
