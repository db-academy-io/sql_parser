use std::{fs, path::Path};

use db_academy_sql_parser::{errors::ParsingError, Statement};
mod sqlite_parser;

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
                let sql_content = fs::read_to_string(file.clone()).expect("Failed to read file");
                let parsing_result = run_test(&sql_content);
                if parsing_result.is_ok() {
                    passed_test += 1;
                } else {
                    eprintln!("Parsing failed for file: {:?}", file);
                    eprintln!("Error: {:?}", parsing_result.err().unwrap());
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

fn run_test(sql_content: &str) -> Result<Statement, ParsingError> {
    use db_academy_sql_parser::Parser;
    let mut parser = Parser::from(sql_content);
    let statement = parser.parse_statement();
    statement
}

#[test]
pub fn test() {
    let sql_content = fs::read_to_string(
        "tests/codeschool-sqlite-parser-test-files/sql/create-virtual-table/create-virtual-table-alt-syntax.sql",
    )
    .unwrap();
    let result = run_test(&sql_content);
    println!("{:?}", result);
}

#[test]
fn run_codeschool_sqlite_parser_with_sqlite3_parser() {
    use sqlite_parser::parse_sql_file;
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
