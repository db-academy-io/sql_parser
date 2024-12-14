use std::{fs, path::Path};
mod sqlite_parser;

use db_academy_sql_parser::Parser;
use sqlite_parser::parse_sql_file;

#[test]
fn run_codeschool_sqlite_parser_test() {
    let test_set_path = Path::new("tests/codeschool-sqlite-parser-test-files/sql");

    let mut total_tests = 0;
    let mut passed_test = 0;

    for group in fs::read_dir(test_set_path).expect("Failed to read test_set directory") {
        let group = group.expect("Failed to read directory entry").path();
        if !group.is_dir() {
            continue;
        }

        for entry in fs::read_dir(group).expect("Failed to read test_set directory") {
            let file = entry.expect("Failed to read directory entry").path();

            if file.extension().and_then(|ext| ext.to_str()) == Some("sql") {
                let sql_content = fs::read_to_string(file).expect("Failed to read file");
                let parsing_result = run_test(&sql_content);
                if parsing_result {
                    passed_test += 1;
                }
                total_tests += 1;
            }
        }
    }

    println!("Tested {} SQL files", total_tests);
    println!(
        "Passed {} SQL files, {} failed",
        passed_test,
        total_tests - passed_test
    );
    println!(
        "Passed {:.2}% of the tests",
        passed_test as f64 / total_tests as f64 * 100.0
    );
}

fn run_test(sql_content: &str) -> bool {
    let mut parser = Parser::from(sql_content);
    let statement = parser.parse_statement();
    statement.is_ok()
}

#[test]
fn run_codeschool_sqlite_parser_with_sqlite3_parser() {
    let test_set_path = Path::new("tests/codeschool-sqlite-parser-test-files/sql");

    let mut total_tests = 0;
    let mut passed_test = 0;

    for group in fs::read_dir(test_set_path).expect("Failed to read test_set directory") {
        let group = group.expect("Failed to read directory entry").path();
        if !group.is_dir() {
            continue;
        }

        for entry in fs::read_dir(group).expect("Failed to read test_set directory") {
            let file = entry.expect("Failed to read directory entry").path();

            if file.extension().and_then(|ext| ext.to_str()) == Some("sql") {
                let parsing_result = parse_sql_file(file.to_str().unwrap());
                if parsing_result {
                    passed_test += 1;
                }
                total_tests += 1;
            }
        }
    }

    println!("Tested {} SQL files", total_tests);
    println!(
        "Passed {} SQL files, {} failed",
        passed_test,
        total_tests - passed_test
    );
    println!(
        "Passed {:.2}% of the tests",
        passed_test as f64 / total_tests as f64 * 100.0
    );
}
