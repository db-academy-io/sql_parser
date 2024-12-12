use fallible_iterator::FallibleIterator;
use std::fs::read;

use sqlite3_parser::lexer::sql::Parser;

/// Parse specified files and check all commands.
pub fn parse_sql_file(file_path: &str) -> bool {
    let input = read(file_path).unwrap();
    let mut parser = Parser::new(&input);
    loop {
        match parser.next() {
            Ok(None) => return true,
            Err(err) => {
                eprintln!("Err: {err} in {file_path}");
                return false;
            }
            Ok(Some(cmd)) => {
                let input = cmd.to_string();
                let mut checker = Parser::new(input.as_bytes());
                match checker.next() {
                    Err(err) => {
                        eprintln!(
                            "Check Err in {}:{}, {} in\n{}\n{:?}",
                            file_path,
                            parser.line(),
                            err,
                            input,
                            cmd
                        );
                    }
                    Ok(None) => {
                        eprintln!("Check Err in {}:{}, {:?}", file_path, parser.line(), cmd);
                    }
                    Ok(Some(check)) => {
                        if cmd != check {
                            eprintln!("{cmd:?}\n<>\n{check:?}");
                        }
                    }
                }
            }
        }
    }
}
