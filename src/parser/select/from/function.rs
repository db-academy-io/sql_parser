use crate::{
    parser::{ExpressionParser, ParsingError},
    FromClause, Identifier, Parser, SelectFromFunction, TokenType,
};

pub trait SelectFromFunctionParser {
    fn parse_from_function(
        &mut self,
        function_name: Identifier,
    ) -> Result<FromClause, ParsingError>;
}

impl SelectFromFunctionParser for Parser<'_> {
    fn parse_from_function(
        &mut self,
        function_name: Identifier,
    ) -> Result<FromClause, ParsingError> {
        self.consume_as(TokenType::LeftParen)?;

        let arguments = if self.peek_as(TokenType::RightParen).is_ok() {
            vec![]
        } else {
            let expressions = self.parse_comma_separated_expressions()?;
            self.consume_as(TokenType::RightParen)?;
            expressions
        };

        let alias = self.parse_alias_after_as_keyword()?;

        Ok(FromClause::Function(SelectFromFunction {
            function_name,
            arguments,
            alias,
        }))
    }
}

#[cfg(test)]
mod select_from_function_tests {
    use crate::expression::test_utils::{binary_op, identifier_expr, numeric_expr};
    use crate::parser::select::from::test_utils::select_from_function;
    use crate::parser::test_utils::*;
    use crate::{BinaryOp, Identifier, SelectFromFunction, Statement};

    #[test]
    fn select_from_function_without_arguments() {
        let expected_statement = select_from_function(SelectFromFunction {
            function_name: Identifier::Single("function_1".to_string()),
            arguments: vec![],
            alias: None,
        });

        run_sunny_day_test(
            "SELECT * FROM function_1()",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_function_with_one_argument() {
        let expected_statement = select_from_function(SelectFromFunction {
            function_name: Identifier::Single("function_1".to_string()),
            arguments: vec![numeric_expr("1")],
            alias: None,
        });

        run_sunny_day_test(
            "SELECT * FROM function_1(1)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_function_with_schema() {
        let expected_statement = select_from_function(SelectFromFunction {
            function_name: Identifier::Compound(vec![
                "schema_1".to_string(),
                "function_1".to_string(),
            ]),
            arguments: vec![],
            alias: None,
        });

        run_sunny_day_test(
            "SELECT * FROM schema_1.function_1()",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_function_with_expression() {
        let expected_statement = select_from_function(SelectFromFunction {
            function_name: Identifier::Single("function_1".to_string()),
            arguments: vec![binary_op(
                BinaryOp::Plus,
                numeric_expr("1"),
                numeric_expr("2"),
            )],
            alias: None,
        });

        run_sunny_day_test(
            "SELECT * FROM function_1(1+2)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_function_with_multiple_arguments() {
        let expected_statement = select_from_function(SelectFromFunction {
            function_name: Identifier::Single("function_1".to_string()),
            arguments: vec![
                numeric_expr("1"),
                identifier_expr(&["col1"]),
                numeric_expr("3"),
            ],
            alias: None,
        });

        run_sunny_day_test(
            "SELECT * FROM function_1(1, col1, 3)",
            Statement::Select(expected_statement),
        );
    }

    #[test]
    fn select_from_function_with_alias() {
        let expected_statement = select_from_function(SelectFromFunction {
            function_name: Identifier::Single("function_1".to_string()),
            arguments: vec![numeric_expr("1"), numeric_expr("2"), numeric_expr("3")],
            alias: Some("alias".to_string()),
        });

        run_sunny_day_test(
            "SELECT * FROM function_1(1, 2, 3) AS alias",
            Statement::Select(expected_statement.clone()),
        );

        run_sunny_day_test(
            "SELECT * FROM function_1(1, 2, 3) alias",
            Statement::Select(expected_statement.clone()),
        );
    }
}
