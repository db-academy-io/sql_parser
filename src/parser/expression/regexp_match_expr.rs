use crate::parser::errors::ParsingError;
use crate::{BinaryMatchingExpression, Expression, Keyword, Parser};

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

impl<'a> RegexpMatchExpressionParser for Parser<'a> {
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
            Keyword::Glob => BinaryMatchingExpression::Glob(Box::new(pattern)),
            Keyword::Regexp => BinaryMatchingExpression::Regexp(Box::new(pattern)),
            Keyword::Match => BinaryMatchingExpression::Match(Box::new(pattern)),
            _ => unreachable!(),
        };

        if is_not {
            Ok(Expression::BinaryMatchingExpression(
                Box::new(expression),
                BinaryMatchingExpression::Not(Box::new(matching_expression)),
            ))
        } else {
            Ok(Expression::BinaryMatchingExpression(
                Box::new(expression),
                matching_expression,
            ))
        }
    }
}

#[cfg(test)]
mod regexp_match_expression_tests {
    use crate::{BinaryMatchingExpression, Expression, Keyword};

    use crate::parser::expression::test_utils::*;

    fn binary_matching_expression(
        pattern: Expression,
        keyword: Keyword,
    ) -> BinaryMatchingExpression {
        match keyword {
            Keyword::Glob => BinaryMatchingExpression::Glob(Box::new(pattern)),
            Keyword::Regexp => BinaryMatchingExpression::Regexp(Box::new(pattern)),
            Keyword::Match => BinaryMatchingExpression::Match(Box::new(pattern)),
            _ => panic!("Invalid keyword: {}", keyword),
        }
    }

    fn regexp_match_expression(
        expression: Expression,
        pattern: Expression,
        keyword: Keyword,
        is_not: bool,
    ) -> Expression {
        let binary_matching_expression = if is_not {
            BinaryMatchingExpression::Not(Box::new(binary_matching_expression(pattern, keyword)))
        } else {
            binary_matching_expression(pattern, keyword)
        };

        Expression::BinaryMatchingExpression(Box::new(expression), binary_matching_expression)
    }

    #[test]
    fn test_expression_regexp_match_basic() {
        let keywords = vec![Keyword::Glob, Keyword::Regexp, Keyword::Match];

        for keyword in keywords {
            run_sunny_day_expression_test(
                &format!("SELECT 1 {} 'a*';", keyword),
                &regexp_match_expression(
                    numeric_literal_expression("1"),
                    string_literal_expression("'a*'"),
                    keyword,
                    false,
                ),
            );
        }
    }

    #[test]
    fn test_expression_not_regexp_match_basic() {
        let keywords = vec![Keyword::Glob, Keyword::Regexp, Keyword::Match];

        for keyword in keywords {
            run_sunny_day_expression_test(
                &format!("SELECT 1 NOT {} 'a*';", keyword),
                &regexp_match_expression(
                    numeric_literal_expression("1"),
                    string_literal_expression("'a*'"),
                    keyword,
                    true,
                ),
            );
        }
    }
}
