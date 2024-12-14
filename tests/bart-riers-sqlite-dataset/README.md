## Test dataset

The test dataset is taken from the [Bart Kiers' SQLite ANTLR parser](https://github.com/bkiers/sqlite-parser/tree/master).

In the current implementation, there is 67.73% of the tests that are passing.
By passing tests, I mean that the parser successfully parses the SQL query and returns a valid `Expression` AST.
The structure of the parsed AST is not checked, as there are no expected ASTs list

