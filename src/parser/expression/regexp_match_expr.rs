use crate::parser::errors::ParsingError;
use crate::{Expression, GlobExpression, Keyword, MatchExpression, Parser, RegexpExpression};

use super::ExpressionParser;

pub trait RegexpMatchExpressionParser {
    /// Parse a GLOB expression
    /// The `is_not` parameter is used to determine if the GLOB expression is a NOT GLOB expression
    fn parse_regexp_match_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError>;
}

impl RegexpMatchExpressionParser for Parser<'_> {
    fn parse_regexp_match_expression(
        &mut self,
        expression: Expression,
        is_not: bool,
    ) -> Result<Expression, ParsingError> {
        let match_type = self.peek_as_keyword()?;

        if !matches!(match_type, Keyword::Glob | Keyword::Regexp | Keyword::Match) {
            return Err(ParsingError::UnexpectedKeyword(match_type));
        }

        self.consume_as_keyword(match_type)?;

        let pattern = self.parse_expression()?;

        let matching_expression = match match_type {
            Keyword::Glob => Expression::GlobExpression(GlobExpression {
                expression: Box::new(expression),
                not: is_not,
                pattern: Box::new(pattern),
            }),
            Keyword::Regexp => Expression::RegexpExpression(RegexpExpression {
                expression: Box::new(expression),
                not: is_not,
                pattern: Box::new(pattern),
            }),
            Keyword::Match => Expression::MatchExpression(MatchExpression {
                expression: Box::new(expression),
                not: is_not,
                pattern: Box::new(pattern),
            }),
            _ => unreachable!(),
        };

        Ok(matching_expression)
    }
}

#[cfg(test)]
mod regexp_match_expression_tests {
    use crate::parser::test_utils::run_sunny_day_test;
    use crate::select::test_utils::select_expr;
    use crate::{Expression, GlobExpression, MatchExpression, RegexpExpression};

    use crate::parser::expression::test_utils::*;

    fn glob_expr(expression: Expression, pattern: Expression) -> GlobExpression {
        GlobExpression {
            expression: Box::new(expression),
            not: false,
            pattern: Box::new(pattern),
        }
    }

    fn regexp_expr(expression: Expression, pattern: Expression) -> RegexpExpression {
        RegexpExpression {
            expression: Box::new(expression),
            not: false,
            pattern: Box::new(pattern),
        }
    }

    fn match_expr(expression: Expression, pattern: Expression) -> MatchExpression {
        MatchExpression {
            expression: Box::new(expression),
            not: false,
            pattern: Box::new(pattern),
        }
    }

    #[test]
    fn glob_expr_test() {
        let expected = glob_expr(numeric_expr("1"), string_expr("'a*'"));
        run_sunny_day_test("SELECT 1 GLOB 'a*';", select_expr(expected.into()).into());

        let mut expected = glob_expr(numeric_expr("1"), string_expr("'a*'"));
        expected.not = true;
        run_sunny_day_test(
            "SELECT 1 NOT GLOB 'a*';",
            select_expr(expected.into()).into(),
        );
    }

    #[test]
    fn regexp_expr_test() {
        let expected = regexp_expr(numeric_expr("1"), string_expr("'a*'"));
        run_sunny_day_test("SELECT 1 REGEXP 'a*';", select_expr(expected.into()).into());

        let mut expected = regexp_expr(numeric_expr("1"), string_expr("'a*'"));
        expected.not = true;
        run_sunny_day_test(
            "SELECT 1 NOT REGEXP 'a*';",
            select_expr(expected.into()).into(),
        );
    }

    #[test]
    fn match_expr_test() {
        let expected = match_expr(numeric_expr("1"), string_expr("'a*'"));
        run_sunny_day_test("SELECT 1 MATCH 'a*';", select_expr(expected.into()).into());

        let mut expected = match_expr(numeric_expr("1"), string_expr("'a*'"));
        expected.not = true;
        run_sunny_day_test(
            "SELECT 1 NOT MATCH 'a*';",
            select_expr(expected.into()).into(),
        );
    }
}
