use db_academy_sql_parser::Parser;
use std::fs;
use std::path::{Path, PathBuf};

#[test]
fn run_bart_riers_sqlite_dataset_test() {
    let test_set_path = Path::new("tests/bart-riers-sqlite-dataset/test_set");

    assert!(
        test_set_path.exists() && test_set_path.is_dir(),
        "test_set directory not found"
    );

    let mut total_tests = 0;
    let mut passed_test = 0;

    for entry in fs::read_dir(test_set_path).expect("Failed to read test_set directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.extension().and_then(|ext| ext.to_str()) == Some("sql") {
            let parsing_result = std::panic::catch_unwind(|| run_test(&path));

            if parsing_result.is_ok() && parsing_result.unwrap() {
                passed_test += 1;
            }
        }
        total_tests += 1;
    }

    println!("Tested {} SQL files", total_tests);
    println!(
        "Passed {} SQL files, {} failed",
        passed_test,
        total_tests - passed_test
    );
    println!(
        "Passed {:.2}% of the tests",
        (passed_test as f64 / total_tests as f64) * 100.0
    );

    // assert_eq!(passed_test, total_tests);
}

/// Run a single test and return true if the test passes, false otherwise
fn run_test(path: &PathBuf) -> bool {
    let sql_content =
        fs::read_to_string(path).unwrap_or_else(|_| panic!("Failed to read SQL file: {:?}", path));
    let mut parser = Parser::from(sql_content.as_str());
    let statement = parser.parse_statement();

    // use sql_parser::{Token, Tokenizer, TokenizerError};
    // let tokenizer = Tokenizer::from(sql_content.as_str());
    // let statement: Result<Vec<Token>, TokenizerError> = tokenizer.tokens();

    // if statement.is_ok() {
    //     let debug_output = format!("{:#?}", statement);
    //     let debug_file_path = path.with_extension("result.txt");
    //     std::fs::write(debug_file_path, debug_output).expect("Failed to write debug output");
    //     println!("======\n{:?}\n======\n", &statement);
    //     std::process::exit(1);
    // }

    statement.is_ok()
}
