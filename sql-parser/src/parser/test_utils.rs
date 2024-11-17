use crate::{Parser, Statement};

use super::ParsingError;

pub fn run_sunny_day_test(sql: &str, expected_statement: Statement) {
    let mut parser = Parser::from(sql);
    let actual_statement = parser
        .parse_statement()
        .expect("Expected parsed Statement, got Parsing Error");

    assert_eq!(
        expected_statement, actual_statement,
        "Expected statement {:?}, got {:?}",
        expected_statement, actual_statement
    );
}

pub fn run_rainy_day_test(sql: &str, expected_error: ParsingError) {
    let mut parser = Parser::from(sql);
    let actual_error = parser
        .parse_statement()
        .expect_err("Expected Parsing Error, got parsed Statement");

    assert_eq!(
        expected_error, actual_error,
        "Expected error {:?}, got {:?}",
        expected_error, actual_error,
    );
}
