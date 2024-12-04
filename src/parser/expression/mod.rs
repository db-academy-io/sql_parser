mod function;

use crate::{
    parser::select::SelectStatementParser, BetweenExpression, BinaryMatchingExpression, BinaryOp,
    CaseExpression, DataType, ExistsStatement, Expression, Identifier, Keyword, LiteralValue,
    Parser, ParsingError, RaiseFunction, TokenType, UnaryMatchingExpression, UnaryOp,
    WhenExpression,
};

use function::FunctionParser;
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
        (TokenType::EqualsEquals, 10),
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

    /// Parse a CASE expression
    fn parse_case_expression(&mut self) -> Result<Expression, ParsingError>;

    /// Parse a CAST expression
    fn parse_cast_expression(&mut self) -> Result<Expression, ParsingError>;

    /// Parse an EXISTS expression
    fn parse_exist_expression(&mut self, is_not: bool) -> Result<Expression, ParsingError>;

    /// Parse a RAISE expression
    fn parse_raise_expression(&mut self) -> Result<Expression, ParsingError>;

    /// Parse a BETWEEN expression
    fn parse_between_expression(
        &mut self,
        expression: Expression,
        inverted: bool,
    ) -> Result<Expression, ParsingError>;

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
        // Check if it's one of the special expressions
        if let Ok(keyword) = self.peek_as_keyword() {
            match keyword {
                Keyword::Case => return self.parse_case_expression(),
                Keyword::Cast => return self.parse_cast_expression(),
                Keyword::Not => {
                    self.consume_token()?;
                    return self.parse_exist_expression(true);
                }
                Keyword::Exists => return self.parse_exist_expression(false),
                Keyword::Raise => return self.parse_raise_expression(),
                Keyword::Null
                | Keyword::CurrentTime
                | Keyword::CurrentDate
                | Keyword::CurrentTimestamp => {
                    // These are literals, so we don't need to do anything here
                    // as the pratt parser will handle them
                }
                _ => return Err(ParsingError::UnexpectedKeyword(keyword)),
            }
        }

        // Check if it's a compound identifier
        if let Ok(identifier) = self.parse_identifier() {
            // Check if it's a function call
            if self.peek_as(TokenType::LeftParen).is_ok() {
                // Parse the function call
                return self.parse_function(identifier);
            } else {
                return Ok(Expression::Identifier(identifier));
            }
        }

        let expression = self.parse_expression_pratt(0)?;
        dbg!("parse_expression: {:?}", &expression);

        if let Ok(keyword) = self.peek_as_keyword() {
            match keyword {
                Keyword::Collate => {
                    self.consume_keyword(Keyword::Collate)?;

                    let name = self.peek_as_string()?;
                    self.consume_token()?;

                    return Ok(Expression::CollateExpression(
                        Box::new(expression),
                        name.to_string(),
                    ));
                }
                Keyword::Isnull => {
                    self.consume_keyword(Keyword::Isnull)?;
                    return Ok(Expression::UnaryMatchingExpression(
                        Box::new(expression),
                        UnaryMatchingExpression::IsNull,
                    ));
                }
                Keyword::Notnull => {
                    self.consume_keyword(Keyword::Notnull)?;
                    return Ok(Expression::UnaryMatchingExpression(
                        Box::new(expression),
                        UnaryMatchingExpression::IsNotNull,
                    ));
                }
                Keyword::Not => {
                    self.consume_keyword(Keyword::Not)?;

                    if let Ok(Keyword::Null) = self.peek_as_keyword() {
                        self.consume_keyword(Keyword::Null)?;
                        return Ok(Expression::UnaryMatchingExpression(
                            Box::new(expression),
                            UnaryMatchingExpression::IsNotNull,
                        ));
                    }
                    if let Ok(Keyword::Between) = self.peek_as_keyword() {
                        return self.parse_between_expression(expression, true);
                    }
                }
                Keyword::Between => {
                    // No need to consume the BETWEEN keyword, as the between parser will handle it
                    return self.parse_between_expression(expression, false);
                }
                _ => {}
            }
        }

        Ok(expression)
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

    fn parse_case_expression(&mut self) -> Result<Expression, ParsingError> {
        self.consume_keyword(Keyword::Case)?;

        // the main expression is optional, so we have to try to parse it first
        let expression = self
            .parse_expression()
            .map(|expression| Some(Box::new(expression)))
            .unwrap_or(None);

        let mut when_expressions = Vec::new();

        while let Ok(Keyword::When) = self.peek_as_keyword() {
            // the when expression must start with the WHEN keyword
            self.consume_keyword(Keyword::When)?;

            let expression = self.parse_expression()?;

            // The THEN keyword is required after the WHEN expression
            self.consume_keyword(Keyword::Then)?;

            let then_expression = self.parse_expression()?;

            when_expressions.push(WhenExpression {
                condition: Box::new(expression),
                then_expression: Box::new(then_expression),
            });
        }

        let mut else_expression = None;
        if let Ok(Keyword::Else) = self.peek_as_keyword() {
            self.consume_keyword(Keyword::Else)?;
            else_expression = Some(Box::new(self.parse_expression()?));
        }

        self.consume_keyword(Keyword::End)?;
        Ok(Expression::CaseExpression(CaseExpression {
            expression,
            when_expressions,
            else_expression,
        }))
    }

    fn parse_cast_expression(&mut self) -> Result<Expression, ParsingError> {
        self.consume_keyword(Keyword::Cast)?;

        self.peek_as(TokenType::LeftParen)?;
        self.consume_token()?;

        let expression = self.parse_expression()?;
        self.consume_keyword(Keyword::As)?;

        let data_type = {
            let name = self.peek_as_id()?;
            self.consume_token()?;

            if self.peek_as(TokenType::LeftParen).is_ok() {
                self.consume_token()?;
                let lower_bound = self.peek_as_number()?;
                self.consume_token()?;

                if self.peek_as(TokenType::Comma).is_ok() {
                    self.consume_token()?;
                    let upper_bound = self.peek_as_number()?;
                    self.consume_token()?;

                    self.peek_as(TokenType::RightParen)?;
                    self.consume_token()?;

                    DataType::BoundedDataType(name.to_string(), lower_bound, upper_bound)
                } else {
                    self.peek_as(TokenType::RightParen)?;
                    self.consume_token()?;
                    DataType::SizedDataType(name.to_string(), lower_bound)
                }
            } else {
                DataType::PlainDataType(name.to_string())
            }
        };

        Ok(Expression::Cast(Box::new(expression), data_type))
    }

    fn parse_exist_expression(&mut self, is_inverted: bool) -> Result<Expression, ParsingError> {
        // Consume the EXISTS keyword, if it's present. The result is ignored, because
        // the EXISTS keyword is an optional keyword
        let _ = self.consume_keyword(Keyword::Exists);

        // Check if it's a left parenthesis, which indicates the start of a subquery
        self.peek_as(TokenType::LeftParen)?;
        self.consume_token()?;

        let select_statement = self.parse_select_statement()?;

        self.peek_as(TokenType::RightParen)?;
        self.consume_token()?;

        if is_inverted {
            Ok(Expression::ExistsStatement(ExistsStatement::NotExists(
                select_statement,
            )))
        } else {
            Ok(Expression::ExistsStatement(ExistsStatement::Exists(
                select_statement,
            )))
        }
    }

    fn parse_raise_expression(&mut self) -> Result<Expression, ParsingError> {
        // Consume the RAISE keyword
        self.consume_keyword(Keyword::Raise)?;

        self.peek_as(TokenType::LeftParen)?;
        self.consume_token()?;

        let raise = match self.peek_as_keyword()? {
            Keyword::Ignore => {
                self.consume_keyword(Keyword::Ignore)?;
                RaiseFunction::Ignore
            }
            Keyword::Rollback => {
                self.consume_keyword(Keyword::Rollback)?;
                self.peek_as(TokenType::Comma)?;
                // Consume the comma token
                self.consume_token()?;

                let message = self.peek_as_string()?;
                // Consume the message string
                self.consume_token()?;

                RaiseFunction::Rollback(message)
            }
            Keyword::Abort => {
                self.consume_keyword(Keyword::Abort)?;
                self.peek_as(TokenType::Comma)?;
                // Consume the comma token
                self.consume_token()?;

                let message = self.peek_as_string()?;
                // Consume the message string
                self.consume_token()?;

                RaiseFunction::Abort(message)
            }
            Keyword::Fail => {
                self.consume_keyword(Keyword::Fail)?;
                self.peek_as(TokenType::Comma)?;
                // Consume the comma token
                self.consume_token()?;

                let message = self.peek_as_string()?;
                // Consume the message string
                self.consume_token()?;

                RaiseFunction::Fail(message)
            }
            keyword => return Err(ParsingError::UnexpectedKeyword(keyword)),
        };

        self.peek_as(TokenType::RightParen)?;
        self.consume_token()?;

        Ok(Expression::RaiseFunction(raise))
    }

    fn parse_between_expression(
        &mut self,
        expression: Expression,
        inverted: bool,
    ) -> Result<Expression, ParsingError> {
        // Consume the BETWEEN keyword
        self.consume_keyword(Keyword::Between)?;

        let lower_bound = self.parse_expression()?;

        self.consume_keyword(Keyword::And)?;

        let upper_bound = self.parse_expression()?;

        if inverted {
            Ok(Expression::BinaryMatchingExpression(
                Box::new(expression),
                BinaryMatchingExpression::Not(Box::new(BinaryMatchingExpression::Between(
                    BetweenExpression {
                        lower_bound: Box::new(lower_bound),
                        upper_bound: Box::new(upper_bound),
                    },
                ))),
            ))
        } else {
            Ok(Expression::BinaryMatchingExpression(
                Box::new(expression),
                BinaryMatchingExpression::Between(BetweenExpression {
                    lower_bound: Box::new(lower_bound),
                    upper_bound: Box::new(upper_bound),
                }),
            ))
        }
    }
}

#[cfg(test)]
pub(crate) mod test_utils {
    use crate::ast::{Expression, SelectItem};
    use crate::{
        BinaryOp, DataType, ExistsStatement, Function, FunctionArg, Identifier, LiteralValue,
        OverClause, Parser, RaiseFunction, SelectStatement, Statement, UnaryOp,
    };

    // TODO: Make this generic, and move to test_utils module
    pub fn run_sunny_day_test(sql: &str, expected_expression: &Expression) {
        let mut parser = Parser::from(sql);
        let actual_expression = parser
            .parse_statement()
            .expect("Expected parsed Statement, got Parsing Error");

        dbg!("actual_expression: {:?}", &actual_expression);

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
        arg: FunctionArg,
        filter: Option<Box<Expression>>,
        over: Option<OverClause>,
    ) -> Expression {
        let function = Function {
            name: Identifier::Single(name.to_string()),
            arg,
            filter_clause: filter,
            over_clause: over,
        };

        Expression::Function(function)
    }

    pub fn exist_expression(is_inverted: bool, statement: SelectStatement) -> Expression {
        Expression::ExistsStatement(if is_inverted {
            ExistsStatement::NotExists(statement)
        } else {
            ExistsStatement::Exists(statement)
        })
    }

    pub fn raise_expression(function: RaiseFunction) -> Expression {
        Expression::RaiseFunction(function)
    }

    pub fn cast_expression(expression: Expression, data_type: DataType) -> Expression {
        Expression::Cast(Box::new(expression), data_type)
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
}

#[cfg(test)]
mod binary_op_expression_tests {
    use super::test_utils::*;
    use crate::{BinaryOp, UnaryOp};

    #[test]
    fn test_expression_binary_ops() {
        use BinaryOp::*;
        let operators = [
            Plus,
            Minus,
            Mul,
            Div,
            Remainder,
            GreaterThan,
            GreaterThanOrEquals,
            LessThan,
            LessThanOrEquals,
            Equals,
            EqualsEquals,
            NotEquals,
            Concat,
            BitAnd,
            BitOr,
            LeftShift,
            RightShift,
        ];

        for op in operators {
            run_sunny_day_test(
                &format!("SELECT 1 {} 2;", op),
                &binary_op_expression(
                    op,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
            );
        }
    }

    #[test]
    fn test_expression_binary_op_valid() {
        run_sunny_day_test(
            "SELECT 1 ++ 2;",
            &binary_op_expression(
                BinaryOp::Plus,
                numeric_literal_expression("1"),
                unary_op_expression(UnaryOp::Plus, numeric_literal_expression("2")),
            ),
        );

        run_sunny_day_test(
            "SELECT 1 + 2 * 3;",
            &binary_op_expression(
                BinaryOp::Plus,
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
                BinaryOp::Plus,
                binary_op_expression(
                    BinaryOp::Plus,
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
                BinaryOp::Minus,
                binary_op_expression(
                    BinaryOp::Plus,
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
mod exist_expression_tests {
    use crate::{SelectItem, SelectStatement};

    use super::test_utils::*;

    #[test]
    fn test_expression_exists() {
        run_sunny_day_test(
            "SELECT EXISTS (SELECT 1);",
            &exist_expression(
                false,
                SelectStatement {
                    distinct: false,
                    all: false,
                    columns: vec![SelectItem::Expression(numeric_literal_expression("1"))],
                    from: None,
                },
            ),
        );
    }

    #[test]
    fn test_expression_not_exists() {
        run_sunny_day_test(
            "SELECT NOT EXISTS (SELECT 1);",
            &exist_expression(
                true,
                SelectStatement {
                    distinct: false,
                    all: false,
                    columns: vec![SelectItem::Expression(numeric_literal_expression("1"))],
                    from: None,
                },
            ),
        );
    }

    #[test]
    fn test_expression_not_exists_without_exists_keyword() {
        run_sunny_day_test(
            "SELECT NOT (SELECT 21);",
            &exist_expression(
                true,
                SelectStatement {
                    distinct: false,
                    all: false,
                    columns: vec![SelectItem::Expression(numeric_literal_expression("21"))],
                    from: None,
                },
            ),
        );
    }
}

#[cfg(test)]
mod raise_expression_tests {
    use crate::{parser::test_utils::run_rainy_day_test, ParsingError, RaiseFunction};

    use super::test_utils::*;

    #[test]
    fn test_expression_raise_ignore() {
        run_sunny_day_test(
            "SELECT RAISE(IGNORE);",
            &raise_expression(RaiseFunction::Ignore),
        );
    }

    #[test]
    fn test_expression_raise_rollback() {
        run_sunny_day_test(
            "SELECT RAISE(ROLLBACK, 'Error');",
            &raise_expression(RaiseFunction::Rollback("'Error'".to_string())),
        );
    }

    #[test]
    fn test_expression_raise_abort() {
        run_sunny_day_test(
            "SELECT RAISE(ABORT, 'Error');",
            &raise_expression(RaiseFunction::Abort("'Error'".to_string())),
        );
    }

    #[test]
    fn test_expression_raise_fail() {
        run_sunny_day_test(
            "SELECT RAISE(FAIL, 'Error');",
            &raise_expression(RaiseFunction::Fail("'Error'".to_string())),
        );
    }

    #[test]
    fn test_expression_raise_fail_with_empty_string() {
        run_rainy_day_test(
            "SELECT RAISE(FAIL);",
            ParsingError::UnexpectedToken(")".to_string()),
        );
    }
}

#[cfg(test)]
mod cast_expression_tests {
    use crate::{BinaryOp, DataType};

    use super::test_utils::*;

    #[test]
    fn test_expression_cast_basic() {
        run_sunny_day_test(
            "SELECT CAST(1 AS INTEGER);",
            &cast_expression(
                numeric_literal_expression("1"),
                DataType::PlainDataType("INTEGER".to_string()),
            ),
        );
    }

    #[test]
    fn test_expression_cast_expression() {
        run_sunny_day_test(
            "SELECT CAST(1 + 2 AS INTEGER);",
            &cast_expression(
                binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                DataType::PlainDataType("INTEGER".to_string()),
            ),
        );
    }

    #[test]
    fn test_expression_cast_with_null() {
        run_sunny_day_test(
            "SELECT CAST(NULL AS INTEGER);",
            &cast_expression(
                null_literal_expression(),
                DataType::PlainDataType("INTEGER".to_string()),
            ),
        );
    }

    #[test]
    fn test_expression_cast_with_complex_type() {
        run_sunny_day_test(
            "SELECT CAST(1 AS VARCHAR(10));",
            &cast_expression(
                numeric_literal_expression("1"),
                DataType::SizedDataType("VARCHAR".to_string(), "10".to_string()),
            ),
        );
    }

    #[test]
    fn test_expression_cast_with_complex_type2() {
        run_sunny_day_test(
            "SELECT CAST(1 AS VARCHAR(1, 10));",
            &cast_expression(
                numeric_literal_expression("1"),
                DataType::BoundedDataType("VARCHAR".to_string(), "1".to_string(), "10".to_string()),
            ),
        );
    }
}

#[cfg(test)]
mod case_expression_tests {
    use crate::{BinaryOp, CaseExpression, Expression, WhenExpression};

    use super::test_utils::*;

    fn case_expression(
        expression: Option<Box<Expression>>,
        when_expressions: Vec<WhenExpression>,
        else_expression: Option<Box<Expression>>,
    ) -> Expression {
        Expression::CaseExpression(CaseExpression {
            expression,
            when_expressions,
            else_expression,
        })
    }

    #[test]
    fn test_expression_case_basic() {
        let expression = None;
        let when_expressions = vec![WhenExpression {
            condition: Box::new(numeric_literal_expression("1")),
            then_expression: Box::new(numeric_literal_expression("2")),
        }];
        let else_expression = Some(Box::new(numeric_literal_expression("3")));

        run_sunny_day_test(
            "SELECT CASE WHEN 1 THEN 2 ELSE 3 END;",
            &case_expression(expression, when_expressions, else_expression),
        );
    }

    #[test]
    fn test_expression_case_with_multiple_when_expressions() {
        let expression = None;
        let when_expressions = vec![
            WhenExpression {
                condition: Box::new(numeric_literal_expression("1")),
                then_expression: Box::new(numeric_literal_expression("2")),
            },
            WhenExpression {
                condition: Box::new(numeric_literal_expression("3")),
                then_expression: Box::new(numeric_literal_expression("4")),
            },
        ];
        let else_expression = Some(Box::new(numeric_literal_expression("5")));

        run_sunny_day_test(
            "SELECT CASE WHEN 1 THEN 2 WHEN 3 THEN 4 ELSE 5 END;",
            &case_expression(expression, when_expressions, else_expression),
        );
    }

    #[test]
    fn test_expression_case_with_main_expression() {
        let expression = Some(Box::new(binary_op_expression(
            BinaryOp::EqualsEquals,
            numeric_literal_expression("1"),
            numeric_literal_expression("1"),
        )));
        let when_expressions = vec![WhenExpression {
            condition: Box::new(boolean_literal_expression(true)),
            then_expression: Box::new(numeric_literal_expression("1")),
        }];
        let else_expression = Some(Box::new(numeric_literal_expression("2")));

        run_sunny_day_test(
            "SELECT CASE 1 == 1 WHEN TRUE THEN 1 ELSE 2 END;",
            &case_expression(expression, when_expressions, else_expression),
        );
    }
}

#[cfg(test)]
mod collate_expression_tests {
    use crate::{BinaryOp, Expression};

    use super::test_utils::*;

    fn collate_expression(expression: Expression, name: String) -> Expression {
        Expression::CollateExpression(Box::new(expression), name)
    }

    #[test]
    fn test_expression_collate_basic() {
        run_sunny_day_test(
            "SELECT 1 COLLATE 'utf8';",
            &collate_expression(numeric_literal_expression("1"), "'utf8'".to_string()),
        );
    }

    #[test]
    fn test_expression_collate_with_expression() {
        run_sunny_day_test(
            "SELECT 1 + 2 COLLATE 'utf8';",
            &collate_expression(
                binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                "'utf8'".to_string(),
            ),
        );
    }
}

#[cfg(test)]
mod unary_matching_expression_tests {
    use crate::{Expression, UnaryMatchingExpression};

    use super::test_utils::*;

    fn unary_matching_expression(
        expression: Expression,
        unary_matching_expression: UnaryMatchingExpression,
    ) -> Expression {
        Expression::UnaryMatchingExpression(Box::new(expression), unary_matching_expression)
    }

    #[test]
    fn test_expression_matching_isnull() {
        run_sunny_day_test(
            "SELECT 1 ISNULL;",
            &unary_matching_expression(
                numeric_literal_expression("1"),
                UnaryMatchingExpression::IsNull,
            ),
        );
    }

    #[test]
    fn test_expression_matching_notnull() {
        run_sunny_day_test(
            "SELECT 1 NOT NULL;",
            &unary_matching_expression(
                numeric_literal_expression("1"),
                UnaryMatchingExpression::IsNotNull,
            ),
        );
        run_sunny_day_test(
            "SELECT 1 NOTNULL;",
            &unary_matching_expression(
                numeric_literal_expression("1"),
                UnaryMatchingExpression::IsNotNull,
            ),
        );
    }
}

#[cfg(test)]
mod between_expression_tests {
    use crate::{BetweenExpression, BinaryMatchingExpression, BinaryOp, Expression};

    use super::test_utils::*;

    fn between_expression(
        expression: Expression,
        lower_bound: Expression,
        upper_bound: Expression,
        inverted: bool,
    ) -> Expression {
        let between_expression = BinaryMatchingExpression::Between(BetweenExpression {
            lower_bound: Box::new(lower_bound),
            upper_bound: Box::new(upper_bound),
        });

        let binary_matching_expression = if inverted {
            BinaryMatchingExpression::Not(Box::new(between_expression))
        } else {
            between_expression
        };

        Expression::BinaryMatchingExpression(Box::new(expression), binary_matching_expression)
    }

    #[test]
    fn test_expression_between_basic() {
        run_sunny_day_test(
            "SELECT 1 BETWEEN 2 AND 3;",
            &between_expression(
                numeric_literal_expression("1"),
                numeric_literal_expression("2"),
                numeric_literal_expression("3"),
                false,
            ),
        );
    }

    #[test]
    fn test_expression_between_with_expression() {
        run_sunny_day_test(
            "SELECT 1 + 2 BETWEEN 3 AND 4;",
            &between_expression(
                binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                numeric_literal_expression("3"),
                numeric_literal_expression("4"),
                false,
            ),
        );
    }

    #[test]
    fn test_expression_not_between_with_expression() {
        run_sunny_day_test(
            "SELECT 1 + 2 NOT BETWEEN 3 AND 4;",
            &between_expression(
                binary_op_expression(
                    BinaryOp::Plus,
                    numeric_literal_expression("1"),
                    numeric_literal_expression("2"),
                ),
                numeric_literal_expression("3"),
                numeric_literal_expression("4"),
                true,
            ),
        );
    }
}
