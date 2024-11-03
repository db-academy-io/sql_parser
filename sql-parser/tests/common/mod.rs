#![allow(unused)]

use sql_parser::{ParsingError, Token, TokenType, Tokenizer};

pub fn run_sunny_day_test<'a>(sql: &'a str, expected_tokens: Vec<TokenType<'a>>) {
    let mut tokenizer = Tokenizer::from(sql);

    for expected_token_type in expected_tokens {
        let token = tokenizer.next_token();
        let token = token.expect("Expected a token, but got None");
        let token = token
            .expect(format!("Expected {:?}, got Unexpected error: ", expected_token_type).as_str());

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
    expected_error: ParsingError,
) {
    let mut tokenizer = Tokenizer::from(sql);

    // We expect tokens up until the comment, then an error
    for expected_token_type in expected_tokens {
        let token = tokenizer.next_token();
        let token = token.expect("Expected a token, but got None");
        let token = token
            .expect(format!("Expected {:?}, got Unexpected error: ", expected_token_type).as_str());

        assert_eq!(
            token.token_type, expected_token_type,
            "Expected token {:?}, got {:?}",
            expected_token_type, token.token_type
        );
    }

    // The next call to next_token() should return an error
    let token: Option<Result<Token<'_>, ParsingError<'_>>> = tokenizer.next_token();
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
