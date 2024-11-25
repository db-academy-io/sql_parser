use crate::{
    BinaryOp, Expression, Function, FunctionArg, Identifier, Keyword, LiteralValue, OrderingTerm,
    OverClause, Parser, ParsingError, TokenType, UnaryOp,
};

use once_cell::sync::Lazy;
use std::collections::HashMap;

/// The precedence of the operators.
///
/// The precedence of an operator is the maximum precedence of the expressions
/// that can be formed with the operator.
///
/// The source of the precedence values is the SQLite grammar, see:
/// [SQL Language Expressions](https://www.sqlite.org/lang_expr.html)
static PRECEDENCE: Lazy<HashMap<TokenType, u8>> = Lazy::new(|| {
    let pairs = [
        (TokenType::Concat, 60),
        (TokenType::Star, 50),
        (TokenType::Slash, 50),
        (TokenType::Remainder, 50),
        (TokenType::Plus, 40),
        (TokenType::Minus, 40),
        (TokenType::BitAnd, 30),
        (TokenType::BitOr, 30),
        (TokenType::LeftShift, 30),
        (TokenType::RightShift, 30),
        (TokenType::GreaterThan, 20),
        (TokenType::LessThan, 20),
        (TokenType::GreaterEquals, 20),
        (TokenType::LessEquals, 20),
        (TokenType::Equals, 10),
        (TokenType::NotEquals, 10),
    ];

    pairs.iter().cloned().collect()
});

/// Trait for parsing expressions
/// The expression documentation can be found here:
/// https://www.sqlite.org/lang_expr.html
pub trait ExpressionParser {
    /// Parse an expression
    fn parse_expression(&mut self) -> Result<Expression, ParsingError>;

    /// Parse a function
    fn parse_function(&mut self, name: Identifier) -> Result<Expression, ParsingError>;

    /// Parse a function arguments
    fn parse_function_args(&mut self) -> Result<Vec<FunctionArg>, ParsingError>;

    /// Parse a function argument
    fn parse_function_arg(&mut self) -> Result<FunctionArg, ParsingError>;

    /// Parse a function ordering terms
    fn parse_function_ordering_terms(&mut self) -> Result<Vec<OrderingTerm>, ParsingError>;

    /// Parse a function filter clause
    fn parse_function_filter_clause(&mut self) -> Result<Expression, ParsingError>;

    /// Parse a function over clause
    fn parse_function_over_clause(&mut self) -> Result<OverClause, ParsingError>;

    /// Parse an identifier
    fn parse_identifier(&mut self) -> Result<Identifier, ParsingError>;

    /// Parse an expression using Pratt's parsing algorithm
    /// `rbp` is the right binding power of the current operator
    fn parse_expression_pratt(&mut self, rbp: u8) -> Result<Expression, ParsingError>;

    /// Parse a prefix expression
    /// A prefix expression is an expression that does not have a left operand.
    fn parse_prefix(&mut self) -> Result<Expression, ParsingError>;

    /// Parse an infix expression
    /// An infix expression is an expression that has a left operand.
    fn parse_infix(&mut self, left: Expression, precedence: u8)
        -> Result<Expression, ParsingError>;

    /// Get the precedence of the given operator
    fn get_precedence(&mut self, operator: &TokenType) -> u8;
}

impl<'a> ExpressionParser for Parser<'a> {
    /// Parse an expression
    /// See details [sqlite-expression]
    ///
    /// [sqlite-expression]: https://www.sqlite.org/lang_expr.html#the_expr_list
    fn parse_expression(&mut self) -> Result<Expression, ParsingError> {
        // Check if it's a compound identifier
        if let Ok(identifier) = self.parse_identifier() {
            // Check if it's a function call
            match self.peek_as(TokenType::LeftParen) {
                Ok(_) => {
                    // It's a function call
                    // consume the left parenthesis
                    self.consume_token()?;

                    // Parse the function arguments
                    return self.parse_function(identifier);
                }
                _ => return Ok(Expression::Identifier(identifier)),
            }
        }

        let expression = self.parse_expression_pratt(0);
        dbg!("parse_expression: {:?}", &expression);
        if let Ok(expression) = expression {
            return Ok(expression);
        }

        todo!()
    }

    /// Parse an identifier
    fn parse_identifier(&mut self) -> Result<Identifier, ParsingError> {
        let mut components = Vec::new();

        while let Ok(identifier) = self.peek_as_id() {
            components.push(identifier.to_string());
            // Consume the identifier token
            self.consume_token()?;

            if self.peek_as(TokenType::Dot).is_ok() {
                // Consume the dot token
                self.consume_token()?;
            }
        }

        match components.len() {
            0 => Err(ParsingError::UnexpectedToken(
                "Expected identifier".to_string(),
            )),
            1 => Ok(Identifier::Single(components[0].to_string())),
            _ => Ok(Identifier::Compound(components)),
        }
    }

    /// Parse an expression using Pratt's parsing algorithm
    fn parse_expression_pratt(&mut self, precedence: u8) -> Result<Expression, ParsingError> {
        let mut expression = self.parse_prefix()?;

        loop {
            let current_token = self.peek_token()?;
            let next_precedence = self.get_precedence(&current_token.token_type);

            if precedence >= next_precedence {
                break;
            }
            expression = self.parse_infix(expression, next_precedence)?;
        }
        Ok(expression)
    }

    /// Parse an infix expression
    /// An infix expression is an expression that has a left operand.
    fn parse_infix(
        &mut self,
        left: Expression,
        precedence: u8,
    ) -> Result<Expression, ParsingError> {
        let token = self.peek_token()?;
        let operator = BinaryOp::try_from(&token.token_type)?;
        // Consume the operator token
        self.consume_token()?;
        let right = self.parse_expression_pratt(precedence)?;
        Ok(Expression::BinaryOp(
            Box::new(left),
            operator,
            Box::new(right),
        ))
    }

    /// Parse a prefix expression
    /// A prefix expression is an expression that does not have a left operand.
    fn parse_prefix(&mut self) -> Result<Expression, ParsingError> {
        if let Ok(keyword) = self.peek_as_keyword() {
            if keyword == Keyword::Null
                || keyword == Keyword::CurrentTime
                || keyword == Keyword::CurrentDate
                || keyword == Keyword::CurrentTimestamp
            {
                self.consume_token()?;

                match keyword {
                    Keyword::Null => return Ok(Expression::LiteralValue(LiteralValue::Null)),
                    Keyword::CurrentTime => {
                        return Ok(Expression::LiteralValue(LiteralValue::CurrentTime))
                    }
                    Keyword::CurrentDate => {
                        return Ok(Expression::LiteralValue(LiteralValue::CurrentDate))
                    }
                    Keyword::CurrentTimestamp => {
                        return Ok(Expression::LiteralValue(LiteralValue::CurrentTimestamp))
                    }
                    _ => {}
                }
            }
        }

        let token = self.peek_token()?;

        match token.token_type {
            TokenType::Id(value) => {
                self.consume_token()?;
                Ok(Expression::Identifier(Identifier::Single(
                    value.to_string(),
                )))
            }
            TokenType::Integer(value) => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::Number(
                    value.to_string(),
                )))
            }
            TokenType::Float(value) => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::Number(
                    value.to_string(),
                )))
            }
            TokenType::String(value) => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::String(
                    value.to_string(),
                )))
            }
            TokenType::Variable(value) => {
                self.consume_token()?;
                Ok(Expression::BindParameter(value.to_string()))
            }
            TokenType::LeftParen => {
                // Consume the left parenthesis
                self.consume_token()?;
                let expression = self.parse_expression_pratt(0)?;
                let token = self.peek_token()?;
                if token.token_type != TokenType::RightParen {
                    return Err(ParsingError::UnexpectedToken(format!(
                        "Expected right parenthesis, got: {}",
                        token.token_type
                    )));
                }
                // Consume the right parenthesis
                self.consume_token()?;
                Ok(expression)
            }
            TokenType::Minus => {
                // Consume the minus token
                self.consume_token()?;
                let pr = self.get_precedence(&TokenType::Minus);
                let expression = self.parse_expression_pratt(pr)?;
                Ok(Expression::UnaryOp(UnaryOp::Minus, Box::new(expression)))
            }
            TokenType::Plus => {
                // Consume the plus token
                self.consume_token()?;
                let pr = self.get_precedence(&TokenType::Plus);
                let expression = self.parse_expression_pratt(pr)?;
                Ok(Expression::UnaryOp(UnaryOp::Plus, Box::new(expression)))
            }
            TokenType::Blob(value) => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::Blob(
                    value.to_string(),
                )))
            }
            TokenType::True => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::Boolean(true)))
            }
            TokenType::False => {
                self.consume_token()?;
                Ok(Expression::LiteralValue(LiteralValue::Boolean(false)))
            }

            // TokenType:: => {
            //     self.consume_token()?;
            //     Ok(Expression::LiteralValue(LiteralValue::Null))
            // }
            _ => Err(ParsingError::UnexpectedToken(format!(
                "Unexpected token: {}",
                token.token_type
            ))),
        }
    }

    /// Get the precedence of the given operator
    fn get_precedence(&mut self, operator: &TokenType) -> u8 {
        *PRECEDENCE.get(operator).unwrap_or(&0)
    }

    /// Parse a function
    fn parse_function(&mut self, name: Identifier) -> Result<Expression, ParsingError> {
        let mut function = Function {
            name,
            args: Vec::new(),
            filter_clause: None,
            over_clause: None,
        };
        function.args = self.parse_function_args()?;
        dbg!("AAAA");
        dbg!("parse_function: function.args: {:?}", &function.args);
        if let Ok(token) = self.peek_as_keyword() {
            if token == Keyword::Filter {
                self.consume_token()?;
                function.filter_clause = Some(Box::new(self.parse_function_filter_clause()?));
            }
        }

        if let Ok(token) = self.peek_as_keyword() {
            if token == Keyword::Over {
                self.consume_token()?;
                function.over_clause = Some(self.parse_function_over_clause()?);
            }
        }

        Ok(Expression::Function(function))
    }

    /// Parse a function arguments
    fn parse_function_args(&mut self) -> Result<Vec<FunctionArg>, ParsingError> {
        dbg!("parse_function_args");
        let mut args = Vec::new();

        while let Ok(arg) = self.parse_function_arg() {
            args.push(arg);
            dbg!("parse_function_args: args: {:?}", &args);
        }
        dbg!("!!!!parse_function_args: args: {:?}", &args);
        Ok(args)
    }

    /// Parse a function argument
    fn parse_function_arg(&mut self) -> Result<FunctionArg, ParsingError> {
        match self.peek_token()?.token_type {
            TokenType::Keyword(Keyword::Distinct) => {
                // Consume the distinct keyword
                self.consume_token()?;
                let expression = self.parse_expression()?;
                Ok(FunctionArg::Distinct(expression))
            }
            TokenType::Star => {
                // Consume the star token
                self.consume_token()?;
                Ok(FunctionArg::Wildcard)
            }
            TokenType::RightParen => {
                // Consume the right parenthesis
                self.consume_token()?;
                Ok(FunctionArg::None)
            }
            _ => {
                let expression = self.parse_expression()?;

                let token = self.peek_token()?;
                if token.token_type == TokenType::Keyword(Keyword::Order) {
                    // Consume the order keyword
                    self.consume_token()?;

                    let by_token_expected = self.peek_token()?;
                    if by_token_expected.token_type != TokenType::Keyword(Keyword::By) {
                        return Err(ParsingError::UnexpectedToken(format!(
                            "Expected by keyword, got: {}",
                            by_token_expected.token_type
                        )));
                    }

                    // Consume the by keyword
                    self.consume_token()?;

                    let ordering_terms = self.parse_function_ordering_terms()?;
                    return Ok(FunctionArg::OrderedByExpression(expression, ordering_terms));
                }
                Ok(FunctionArg::Expression(expression))
            }
        }
    }

    /// Parse a function ordering terms
    fn parse_function_ordering_terms(&mut self) -> Result<Vec<OrderingTerm>, ParsingError> {
        todo!()
    }

    /// Parse a function filter clause
    fn parse_function_filter_clause(&mut self) -> Result<Expression, ParsingError> {
        todo!()
    }

    /// Parse a function over clause
    fn parse_function_over_clause(&mut self) -> Result<OverClause, ParsingError> {
        todo!()
    }
}

#[cfg(test)]
pub(crate) mod test_utils {
    use crate::ast::{Expression, SelectItem};
    use crate::{
        BinaryOp, Function, FunctionArg, Identifier, LiteralValue, OverClause, Parser, Statement,
        UnaryOp,
    };

    // TODO: Make this generic, and move to test_utils module
    pub fn run_sunny_day_test(sql: &str, expected_expression: &Expression) {
        let mut parser = Parser::from(sql);
        let actual_expression = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        match actual_expression {
            Statement::Select(select_statement) => {
                assert_eq!(
                    1,
                    select_statement.columns.len(),
                    "Expected 1 column, got {:?}",
                    select_statement.columns.len()
                );
                let select_item = &select_statement.columns[0];

                match select_item {
                    SelectItem::Expression(actual_expression) => {
                        assert_eq!(
                            expected_expression, actual_expression,
                            "Expected expression {:?}, got {:?}",
                            expected_expression, actual_expression
                        );
                    }
                    _ => panic!("Expected Expression, got {:?}", select_item),
                }
            }
            _ => panic!("Expected Select statement, got {:?}", actual_expression),
        }
    }

    pub fn numeric_literal_expression(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::Number(value.to_string()))
    }

    pub fn string_literal_expression(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::String(value.to_string()))
    }

    pub fn blob_literal_expression(value: &str) -> Expression {
        Expression::LiteralValue(LiteralValue::Blob(value.to_string()))
    }

    pub fn boolean_literal_expression(value: bool) -> Expression {
        Expression::LiteralValue(LiteralValue::Boolean(value))
    }

    pub fn null_literal_expression() -> Expression {
        Expression::LiteralValue(LiteralValue::Null)
    }

    pub fn bind_parameter_expression(value: &str) -> Expression {
        Expression::BindParameter(value.to_string())
    }

    pub fn identifier_expression(value: &str) -> Expression {
        Expression::Identifier(Identifier::Single(value.to_string()))
    }

    pub fn compound_identifier_expression(values: &[&str]) -> Expression {
        Expression::Identifier(Identifier::Compound(
            values.iter().map(|s| s.to_string()).collect(),
        ))
    }

    pub fn unary_op_expression(op: UnaryOp, value: Expression) -> Expression {
        Expression::UnaryOp(op, Box::new(value))
    }

    pub fn binary_op_expression(op: BinaryOp, left: Expression, right: Expression) -> Expression {
        Expression::BinaryOp(Box::new(left), op, Box::new(right))
    }

    pub fn function_expression(
        name: &str,
        args: &[Expression],
        filter: Option<Box<Expression>>,
        over: Option<OverClause>,
    ) -> Expression {
        let args = args
            .iter()
            .map(|arg| FunctionArg::Expression(arg.clone()))
            .collect();

        let function = Function {
            name: Identifier::Single(name.to_string()),
            args,
            filter_clause: filter,
            over_clause: over,
        };

        Expression::Function(function)
    }
}

#[cfg(test)]
mod literal_value_expression_tests {
    use super::test_utils::*;
    use crate::ast::{Expression, LiteralValue};

    #[test]
    fn test_expression_literal_value_valid() {
        run_sunny_day_test("SELECT 1;", &numeric_literal_expression("1"));

        run_sunny_day_test("SELECT 1.2;", &numeric_literal_expression("1.2"));

        run_sunny_day_test(
            "SELECT 1.234567890;",
            &numeric_literal_expression("1.234567890"),
        );

        run_sunny_day_test(
            "SELECT 'Hello, world!';",
            &string_literal_expression("'Hello, world!'"),
        );

        run_sunny_day_test(
            "SELECT X'DEADBEEF';",
            &blob_literal_expression("X'DEADBEEF'"),
        );

        run_sunny_day_test("SELECT TRUE;", &boolean_literal_expression(true));

        run_sunny_day_test("SELECT FALSE;", &boolean_literal_expression(false));

        run_sunny_day_test("SELECT NULL;", &null_literal_expression());

        run_sunny_day_test(
            "SELECT CURRENT_TIME;",
            &Expression::LiteralValue(LiteralValue::CurrentTime),
        );

        run_sunny_day_test(
            "SELECT CURRENT_DATE;",
            &Expression::LiteralValue(LiteralValue::CurrentDate),
        );

        run_sunny_day_test(
            "SELECT CURRENT_TIMESTAMP;",
            &Expression::LiteralValue(LiteralValue::CurrentTimestamp),
        );
    }
}

#[cfg(test)]
mod bind_parameter_expression_tests {
    use super::test_utils::*;

    #[test]
    fn test_expression_bind_parameter_valid() {
        run_sunny_day_test("SELECT ?;", &bind_parameter_expression("?"));
        run_sunny_day_test("SELECT ?1;", &bind_parameter_expression("?1"));
        run_sunny_day_test("SELECT :name;", &bind_parameter_expression(":name"));
        run_sunny_day_test("SELECT @var;", &bind_parameter_expression("@var"));
        run_sunny_day_test("SELECT $value;", &bind_parameter_expression("$value"));
        run_sunny_day_test("SELECT #param;", &bind_parameter_expression("#param"));

        // TODO: Add tests for invalid bind parameters
    }
}

#[cfg(test)]
mod identifier_expression_tests {
    use super::test_utils::*;

    #[test]
    fn test_expression_identifier_valid() {
        run_sunny_day_test("SELECT id;", &identifier_expression("id"));
        run_sunny_day_test(
            "SELECT table1.column1;",
            &compound_identifier_expression(&["table1", "column1"]),
        );
        run_sunny_day_test(
            "SELECT schema1.table1.column1;",
            &compound_identifier_expression(&["schema1", "table1", "column1"]),
        );
    }
}

#[cfg(test)]
mod unary_op_expression_tests {
    use crate::UnaryOp;

    use super::test_utils::*;

    #[test]
    fn test_expression_unary_op_valid() {
        run_sunny_day_test(
            "SELECT +1;",
            &unary_op_expression(UnaryOp::Plus, numeric_literal_expression("1")),
        );
        run_sunny_day_test(
            "SELECT -1;",
            &unary_op_expression(UnaryOp::Minus, numeric_literal_expression("1")),
        );
        run_sunny_day_test(
            "SELECT -abc;",
            &unary_op_expression(UnaryOp::Minus, identifier_expression("abc")),
        );
        run_sunny_day_test(
            "SELECT +abc;",
            &unary_op_expression(UnaryOp::Plus, identifier_expression("abc")),
        );
    }
}

#[cfg(test)]
mod binary_op_expression_tests {
    use super::test_utils::*;
    use crate::{BinaryOp, UnaryOp};

    #[test]
    fn test_unary_op_expression() {
        run_sunny_day_test(
            "SELECT -1;",
            &unary_op_expression(UnaryOp::Minus, numeric_literal_expression("1")),
        );
        run_sunny_day_test(
            "SELECT -+1;",
            &unary_op_expression(
                UnaryOp::Minus,
                unary_op_expression(UnaryOp::Plus, numeric_literal_expression("1")),
            ),
        );
        run_sunny_day_test(
            "SELECT ++1;",
            &unary_op_expression(
                UnaryOp::Plus,
                unary_op_expression(UnaryOp::Plus, numeric_literal_expression("1")),
            ),
        );
    }

    #[test]
    fn test_expression_binary_op_valid() {
        run_sunny_day_test(
            "SELECT 1 + 2;",
            &binary_op_expression(
                BinaryOp::Add,
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
            ),
        );
        run_sunny_day_test(
            "SELECT 1 - 2;",
            &binary_op_expression(
                BinaryOp::Sub,
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
            ),
        );
        run_sunny_day_test(
            "SELECT 1 * 2;",
            &binary_op_expression(
                BinaryOp::Mul,
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
            ),
        );
        run_sunny_day_test(
            "SELECT 1 / 2;",
            &binary_op_expression(
                BinaryOp::Div,
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
            ),
        );
        run_sunny_day_test(
            "SELECT 1 ++ 2;",
            &binary_op_expression(
                BinaryOp::Add,
                numeric_literal_expression("1"),
                unary_op_expression(UnaryOp::Plus, numeric_literal_expression("2")),
            ),
        );

        run_sunny_day_test(
            "SELECT 1 + 2 * 3;",
            &binary_op_expression(
                BinaryOp::Add,
                numeric_literal_expression("1"),
                binary_op_expression(
                    BinaryOp::Mul,
                    numeric_literal_expression("2"),
                    numeric_literal_expression("3"),
                ),
            ),
        );

        run_sunny_day_test(
            "SELECT 1 * 2 / 3;",
            &binary_op_expression(
                BinaryOp::Div,
                binary_op_expression(
                    BinaryOp::Mul,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                numeric_literal_expression("3"),
            ),
        );

        run_sunny_day_test(
            "SELECT 1 + 2 + 3;",
            &binary_op_expression(
                BinaryOp::Add,
                binary_op_expression(
                    BinaryOp::Add,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                numeric_literal_expression("3"),
            ),
        );

        run_sunny_day_test(
            "SELECT 1 * 2 * 3;",
            &binary_op_expression(
                BinaryOp::Mul,
                binary_op_expression(
                    BinaryOp::Mul,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                numeric_literal_expression("3"),
            ),
        );

        run_sunny_day_test(
            "SELECT 1 + 2 * 3 - 4",
            &binary_op_expression(
                BinaryOp::Sub,
                binary_op_expression(
                    BinaryOp::Add,
                    numeric_literal_expression("1"),
                    binary_op_expression(
                        BinaryOp::Mul,
                        numeric_literal_expression("2"),
                        numeric_literal_expression("3"),
                    ),
                ),
                numeric_literal_expression("4"),
            ),
        );
    }
}

#[cfg(test)]
mod function_expression_tests {
    // use super::test_utils::*;

    #[test]
    fn test_expression_function_valid() {
        // run_sunny_day_test(
        //     "SELECT abc();",
        //     &function_expression("abc", &[], None, None),
        // );

        // run_sunny_day_test(
        //     "SELECT abc(*);",
        //     &function_expression("abc", &[], None, None),
        // );

        // run_sunny_day_test(
        //     "SELECT abc(1);",
        //     &function_expression("abc", &[numeric_literal_expression("1")], None, None),
        // );

        // run_sunny_day_test(
        //     "SELECT abc(distinct 1);",
        //     &function_expression("abc", &[numeric_literal_expression("1")], None, None),
        // );

        // run_sunny_day_test(
        //     "SELECT abc(distinct 1 + 2);",
        //     &function_expression("abc", &[numeric_literal_expression("1")], None, None),
        // );

        // run_sunny_day_test(
        //     "SELECT abc(1, 2, 3);",
        //     &function_expression(
        //         "abc",
        //         &[
        //             numeric_literal_expression("1"),
        //             numeric_literal_expression("2"),
        //             numeric_literal_expression("3"),
        //     ],
        //     None,
        //     None,
        // ),
        // );

        // run_sunny_day_test(
        //     "SELECT abc(distinct 1);",
        //     &function_expression("abc", &[numeric_literal_expression("1")], None, None),
        // );

        // run_sunny_day_test(
        //     "SELECT abc(-1);",
        //     &function_expression("abc", &[numeric_literal_expression("-1")], None, None),
        // );
    }
}
