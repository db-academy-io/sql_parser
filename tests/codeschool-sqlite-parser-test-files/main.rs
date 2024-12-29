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
    /*
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-20.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/vtab1-2.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/fts4aa-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/rowid-2.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-21.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-23.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-22.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-26.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-9.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-8.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/fts1d-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-25.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-8.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-8.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-9.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-9.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-18.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/triggerD-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-24.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/fts1c-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/pager1-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/triggerC-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-19.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/alter3-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/pager1-3.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-18.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/schema5-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/vacuum4-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/avtrans-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/pagerfault-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/jrnlmode-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-16.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-13.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/fts3ac-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/tkt-b1d3a2e531-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/with3-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/shared9-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-15.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-10.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-11.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/fts2d-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-14.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-10.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-15.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/selectA-2.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/selectA-3.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-14.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-11.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-13.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-16.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/fkey3-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/e_fkey-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-17.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-12.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-15.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-4.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-4.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-6.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-7.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-5.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/fkey4-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-5.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-14.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-16.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-7.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/fts1porter-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-7.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/fts-9fd058691-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-5.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-4.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/trigger5-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-6.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-6.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-17.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-13.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-2.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/without_rowid3-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-2.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/tkt2285-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary3-3.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/fkey2-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-3.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/fts2c-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-12.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-10.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-3.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/orderby7-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/boundary1-2.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/fts3ad-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/official-suite/randexpr1-11.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/datatypes/basic-datatypes.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/parse-errors/parse-error-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/expressions/expression-unary-1.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/expressions/expression-grouping-9.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/expressions/expression-grouping-8.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/expressions/expression-parenthesis-2.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/expressions/expression-grouping-6.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/expressions/expression-grouping-7.sql"
       "tests/codeschool-sqlite-parser-test-files/sql/create-virtual-table/create-virtual-table-alt-syntax.sql"
    */

    let sql_content = fs::read_to_string(
        "tests/codeschool-sqlite-parser-test-files/sql/datatypes/basic-datatypes.sql",
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
