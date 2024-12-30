use super::CreateStatementParser;
use crate::parser::errors::ParsingError;
use crate::{
    CreateTriggerStatement, Expression, ExpressionParser, IdentifierParser, Keyword, Parser,
    TokenType, TriggerEvent, TriggerEventType, TriggerPreCondition, TriggerStatement,
};

pub trait CreateTriggerStatementParser {
    fn parse_create_trigger_statement(
        &mut self,
        is_temporary: bool,
    ) -> Result<CreateTriggerStatement, ParsingError>;

    fn parse_trigger_pre_condition(&mut self) -> Result<Option<TriggerPreCondition>, ParsingError>;

    fn parse_trigger_event(&mut self) -> Result<TriggerEvent, ParsingError>;

    fn parse_trigger_event_type(&mut self) -> Result<TriggerEventType, ParsingError>;

    fn parse_trigger_statements(&mut self) -> Result<Vec<TriggerStatement>, ParsingError>;

    fn parse_for_each_row(&mut self) -> Result<bool, ParsingError>;

    fn parse_when_clause(&mut self) -> Result<Option<Expression>, ParsingError>;
}

impl<'a> CreateTriggerStatementParser for Parser<'a> {
    fn parse_create_trigger_statement(
        &mut self,
        is_temporary: bool,
    ) -> Result<CreateTriggerStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Trigger)?;

        let if_not_exists = self.parse_if_not_exists()?;
        let trigger_name = self.parse_identifier()?;

        let trigger_pre_condition = self.parse_trigger_pre_condition()?;

        let trigger_event = self.parse_trigger_event()?;

        let for_each_row = self.parse_for_each_row()?;

        let when_clause = self.parse_when_clause()?;

        let trigger_statements = self.parse_trigger_statements()?;

        Ok(CreateTriggerStatement {
            temporary: is_temporary,
            if_not_exists,
            trigger_name,
            trigger_pre_condition,
            trigger_event,
            for_each_row,
            when_clause,
            trigger_statements,
        })
    }

    fn parse_trigger_pre_condition(&mut self) -> Result<Option<TriggerPreCondition>, ParsingError> {
        if self.consume_as_keyword(Keyword::Before).is_ok() {
            return Ok(Some(TriggerPreCondition::Before));
        }

        if self.consume_as_keyword(Keyword::After).is_ok() {
            return Ok(Some(TriggerPreCondition::After));
        }

        if self.consume_as_keyword(Keyword::Instead).is_ok() {
            self.consume_as_keyword(Keyword::Of)?;
            return Ok(Some(TriggerPreCondition::InsteadOf));
        }

        Ok(None)
    }

    fn parse_trigger_event(&mut self) -> Result<TriggerEvent, ParsingError> {
        let event_type = self.parse_trigger_event_type()?;
        self.consume_as_keyword(Keyword::On)?;
        let table_name = self.parse_identifier()?;

        Ok(TriggerEvent {
            event_type,
            table_name,
        })
    }

    fn parse_trigger_event_type(&mut self) -> Result<TriggerEventType, ParsingError> {
        if self.consume_as_keyword(Keyword::Delete).is_ok() {
            return Ok(TriggerEventType::Delete);
        }

        if self.consume_as_keyword(Keyword::Insert).is_ok() {
            return Ok(TriggerEventType::Insert);
        }

        if self.consume_as_keyword(Keyword::Update).is_ok() {
            if self.consume_as_keyword(Keyword::Of).is_ok() {
                let mut columns = vec![];
                loop {
                    let column = self.parse_identifier()?;
                    columns.push(column);
                    if self.consume_as(TokenType::Comma).is_err() {
                        break;
                    }
                }
                return Ok(TriggerEventType::Update(Some(columns)));
            }
            return Ok(TriggerEventType::Update(None));
        }

        Err(ParsingError::UnexpectedParsingState(format!(
            "Expected DELETE, INSERT or UPDATE keyword, got: {}",
            self.peek_token()?
        )))
    }

    fn parse_for_each_row(&mut self) -> Result<bool, ParsingError> {
        if self.consume_as_keyword(Keyword::For).is_ok() {
            self.consume_as_keyword(Keyword::Each)?;
            self.consume_as_keyword(Keyword::Row)?;
            return Ok(true);
        }
        Ok(false)
    }

    fn parse_when_clause(&mut self) -> Result<Option<Expression>, ParsingError> {
        if self.consume_as_keyword(Keyword::When).is_ok() {
            let expression = self.parse_expression()?;
            return Ok(Some(expression));
        }
        Ok(None)
    }

    fn parse_trigger_statements(&mut self) -> Result<Vec<TriggerStatement>, ParsingError> {
        self.consume_as_keyword(Keyword::Begin)?;
        let mut statements = vec![];

        loop {
            if self.consume_as_keyword(Keyword::End).is_ok() {
                break;
            }

            let statement = self.parse_statement()?;
            statements.push(TriggerStatement::try_from(statement)?);

            if self.consume_as(TokenType::Semi).is_err() {
                break;
            }
        }

        // self.consume_as_keyword(Keyword::End)?;
        Ok(statements)
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{
        parser::select::test_utils::select_from, CreateTriggerStatement, FromClause, Identifier,
        QualifiedTableName, TriggerEvent, TriggerEventType, TriggerStatement,
    };

    pub fn create_trigger_statement() -> CreateTriggerStatement {
        CreateTriggerStatement {
            temporary: false,
            if_not_exists: false,
            trigger_name: Identifier::from("trigger_name"),
            trigger_pre_condition: None,
            trigger_event: TriggerEvent {
                event_type: TriggerEventType::Delete,
                table_name: Identifier::from("table_name"),
            },
            for_each_row: false,
            when_clause: None,
            trigger_statements: vec![TriggerStatement::Select(select_from(FromClause::Table(
                QualifiedTableName::from(Identifier::from("table_name")),
            )))],
        }
    }
}

#[cfg(test)]
mod create_trigger_tests {
    use std::fmt::Display;

    use crate::{
        expression::test_utils::{
            binary_op_expression, identifier_expression, numeric_literal_expression,
        },
        parser::{
            create::create_trigger::test_utils::create_trigger_statement,
            delete::test_utils::delete_statement, insert::test_utils::insert_statement,
            select::test_utils::select_statement, test_utils::run_sunny_day_test,
            update::test_utils::update_statement,
        },
        BinaryOp, Identifier, Keyword, Statement, TriggerEvent, TriggerEventType,
        TriggerPreCondition, TriggerStatement,
    };

    #[test]
    fn create_trigger_test() {
        let expected = create_trigger_statement();
        run_sunny_day_test(
            "CREATE TRIGGER trigger_name DELETE ON table_name BEGIN SELECT * FROM table_name; END;",
            Statement::CreateTrigger(expected),
        );
    }

    #[test]
    fn create_temporary_trigger_test() {
        let temp_keywords = vec![Keyword::Temporary, Keyword::Temp];
        let mut expected = create_trigger_statement();
        expected.temporary = true;

        for temp_keyword in temp_keywords {
            run_sunny_day_test(
                &format!("CREATE {} TRIGGER trigger_name DELETE ON table_name BEGIN SELECT * FROM table_name; END;", temp_keyword.to_string()),
                Statement::CreateTrigger(expected.clone()),
            );
        }
    }

    #[test]
    fn create_trigger_with_if_not_exists() {
        let mut expected = create_trigger_statement();
        expected.if_not_exists = true;

        run_sunny_day_test(
            "CREATE TRIGGER IF NOT EXISTS trigger_name DELETE ON table_name BEGIN SELECT * FROM table_name; END;",
            Statement::CreateTrigger(expected),
        );
    }

    #[test]
    fn create_trigger_with_trigger_schema() {
        let mut expected = create_trigger_statement();
        expected.trigger_name =
            Identifier::Compound(vec!["schema_name".to_string(), "trigger_name".to_string()]);

        run_sunny_day_test(
            "CREATE TRIGGER schema_name.trigger_name DELETE ON table_name BEGIN SELECT * FROM table_name; END;",
            Statement::CreateTrigger(expected),
        );
    }

    #[test]
    fn create_trigger_with_trigger_pre_conditions() {
        let trigger_pre_conditions = vec![
            TriggerPreCondition::Before,
            TriggerPreCondition::After,
            TriggerPreCondition::InsteadOf,
        ];

        for trigger_pre_condition in trigger_pre_conditions {
            let mut expected = create_trigger_statement();
            expected.trigger_pre_condition = Some(trigger_pre_condition.clone());

            run_sunny_day_test(
                &format!(
                    "CREATE TRIGGER trigger_name {} DELETE ON table_name BEGIN SELECT * FROM table_name; END;",
                    trigger_pre_condition.to_string()
                ),
                Statement::CreateTrigger(expected),
            );
        }
    }

    #[test]
    fn create_trigger_with_trigger_event_types() {
        let trigger_event_types = vec![
            TriggerEventType::Delete,
            TriggerEventType::Insert,
            TriggerEventType::Update(None),
            TriggerEventType::Update(Some(vec![
                Identifier::from("column_name1"),
                Identifier::from("column_name2"),
            ])),
        ];

        impl Display for Identifier {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Identifier::Single(s) => write!(f, "{}", s),
                    Identifier::Compound(s) => write!(f, "{}", s.join(".")),
                    Identifier::Wildcard => write!(f, "*"),
                    Identifier::NameWithWildcard(s) => write!(f, "{}*", s),
                }
            }
        }

        impl Display for TriggerEventType {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    TriggerEventType::Delete => write!(f, "DELETE"),
                    TriggerEventType::Insert => write!(f, "INSERT"),
                    TriggerEventType::Update(columns) => match columns {
                        Some(columns) => write!(
                            f,
                            "UPDATE OF {}",
                            columns
                                .iter()
                                .map(|c| c.to_string())
                                .collect::<Vec<String>>()
                                .join(", ")
                        ),
                        None => write!(f, "UPDATE"),
                    },
                }
            }
        }

        for trigger_event_type in trigger_event_types {
            let mut expected = create_trigger_statement();
            expected.trigger_event = TriggerEvent {
                event_type: trigger_event_type.clone(),
                table_name: Identifier::from("table_name"),
            };

            let query = format!(
                "CREATE TRIGGER trigger_name {} ON table_name BEGIN SELECT * FROM table_name; END;",
                trigger_event_type.to_string()
            );
            run_sunny_day_test(&query, Statement::CreateTrigger(expected));
        }
    }

    #[test]
    fn create_trigger_with_for_each_row() {
        let mut expected = create_trigger_statement();
        expected.for_each_row = true;

        run_sunny_day_test(
            "CREATE TRIGGER trigger_name DELETE ON table_name FOR EACH ROW BEGIN SELECT * FROM table_name; END;",
            Statement::CreateTrigger(expected),
        );
    }

    #[test]
    fn create_trigger_with_when_clause() {
        let mut expected = create_trigger_statement();
        expected.when_clause = Some(binary_op_expression(
            BinaryOp::Equals,
            identifier_expression(&["col1"]),
            numeric_literal_expression("1"),
        ));

        run_sunny_day_test(
            "CREATE TRIGGER trigger_name DELETE ON table_name WHEN col1 = 1 BEGIN SELECT * FROM table_name; END;",
            Statement::CreateTrigger(expected),
        );
    }

    #[test]
    fn create_trigger_with_multiple_statements() {
        let mut expected = create_trigger_statement();
        expected.trigger_statements = vec![
            TriggerStatement::Update(update_statement()),
            TriggerStatement::Insert(insert_statement()),
            TriggerStatement::Delete(delete_statement()),
            TriggerStatement::Select(select_statement()),
        ];

        run_sunny_day_test(
            r#"
            CREATE TRIGGER trigger_name 
            DELETE ON table_name 
            BEGIN 
                UPDATE table_name1 SET col1 = 1;
                INSERT INTO table_name1 DEFAULT VALUES; 
                DELETE FROM table_name1;
                SELECT * FROM table_name1; 
            END;"#,
            Statement::CreateTrigger(expected),
        );
    }
}
