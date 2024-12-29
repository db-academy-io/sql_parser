use crate::{ExplainStatement, Keyword};

use super::{Parser, ParsingError};

pub trait ExplainStatementParser {
    fn parse_explain_statement(&mut self) -> Result<ExplainStatement, ParsingError>;
}

impl<'a> ExplainStatementParser for Parser<'a> {
    fn parse_explain_statement(&mut self) -> Result<ExplainStatement, ParsingError> {
        self.consume_as_keyword(Keyword::Explain)?;

        let query_plan = if self.consume_as_keyword(Keyword::Query).is_ok() {
            self.consume_as_keyword(Keyword::Plan)?;
            true
        } else {
            false
        };

        let statement = self.parse_statement()?;

        Ok(ExplainStatement {
            statement: Box::new(statement),
            query_plan,
        })
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{parser::select::test_utils::select_statement, ExplainStatement, Statement};

    pub fn explain_statement() -> ExplainStatement {
        ExplainStatement {
            statement: Box::new(Statement::Select(select_statement())),
            query_plan: false,
        }
    }
}

#[cfg(test)]
mod explain_statement_tests {
    use super::test_utils::explain_statement;
    use crate::parser::alter::test_utils::alter_table_statement;
    use crate::parser::create::create_index::test_utils::create_index_statement;
    use crate::parser::create::create_table::test_utils::create_table_statement;
    use crate::parser::create::create_trigger::test_utils::create_trigger_statement;
    use crate::parser::create::create_view::test_utils::create_view_statement;
    use crate::parser::create::create_virtual_table::test_utils::create_virtual_table_statement;
    use crate::parser::delete::test_utils::delete_statement;
    use crate::parser::drop::drop_index::test_utils::drop_index_statement;
    use crate::parser::drop::drop_table::test_utils::drop_table_statement;
    use crate::parser::drop::drop_trigger::test_utils::drop_trigger_statement;
    use crate::parser::drop::drop_view::test_utils::drop_view_statement;
    use crate::parser::insert::test_utils::insert_statement;
    use crate::parser::select::test_utils::select_statement;
    use crate::parser::sqlite::analyze::test_utils::analyze_statement;
    use crate::parser::sqlite::attach::test_utils::{attach_statement, detach_statement};
    use crate::parser::sqlite::pragma::test_utils::pragma_statement;
    use crate::parser::sqlite::reindex::test_utils::reindex_statement;
    use crate::parser::sqlite::vacuum::test_utils::vacuum_statement;
    use crate::parser::trx::begin::test_utils::begin_statement;
    use crate::parser::trx::commit::test_utils::commit_statement;
    use crate::parser::trx::release::test_utils::release_statement;
    use crate::parser::trx::rollback::test_utils::rollback_statement;
    use crate::parser::trx::savepoint::test_utils::savepoint_statement;
    use crate::parser::update::test_utils::update_statement;
    use crate::ExplainStatement;
    use crate::{parser::test_utils::run_sunny_day_test, Statement};

    #[test]
    fn explain_statement_test() {
        let explain_statement = explain_statement();

        run_sunny_day_test(
            "EXPLAIN SELECT * FROM table_name1;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_with_query_plan() {
        let mut explain_statement = explain_statement();
        explain_statement.query_plan = true;

        run_sunny_day_test(
            "EXPLAIN QUERY PLAN SELECT * FROM table_name1;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_alter_table() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::AlterTable(alter_table_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN ALTER TABLE table_name ADD COLUMN column_name integer;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_analize() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::Analyze(analyze_statement())),
            query_plan: false,
        };

        run_sunny_day_test("EXPLAIN ANALYZE;", Statement::Explain(explain_statement));
    }

    #[test]
    fn explain_attach() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::Attach(attach_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN ATTACH DATABASE 'test.db' AS test_db;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_begin() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::BeginTransaction(begin_statement())),
            query_plan: false,
        };

        run_sunny_day_test("EXPLAIN BEGIN;", Statement::Explain(explain_statement));
    }

    #[test]
    fn explain_commit() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::CommitTransaction(commit_statement())),
            query_plan: false,
        };

        run_sunny_day_test("EXPLAIN COMMIT;", Statement::Explain(explain_statement));
    }

    #[test]
    fn explain_create_index() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::CreateIndex(create_index_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN CREATE INDEX index_name ON table_name (column1);",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_create_table() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::CreateTable(create_table_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN CREATE TABLE table_name AS SELECT * FROM table_name;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_create_trigger() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::CreateTrigger(create_trigger_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN CREATE TRIGGER trigger_name DELETE ON table_name BEGIN SELECT * FROM table_name; END;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_create_view() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::CreateView(create_view_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN CREATE VIEW view_name AS SELECT * FROM table_name;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_create_virtual_table() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::CreateVirtualTable(
                create_virtual_table_statement(),
            )),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN CREATE VIRTUAL TABLE test_table USING test_module;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_delete() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::Delete(delete_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN DELETE FROM table_name1",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_detach() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::Detach(detach_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN DETACH DATABASE test_db;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_drop_index() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::DropIndex(drop_index_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN DROP INDEX my_index;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_drop_table() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::DropTable(drop_table_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN DROP TABLE my_table;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_drop_trigger() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::DropTrigger(drop_trigger_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN DROP TRIGGER my_trigger;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_drop_view() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::DropView(drop_view_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN DROP VIEW my_view;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_insert() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::Insert(insert_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN INSERT INTO table_name1 DEFAULT VALUES;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_pragma() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::Pragma(pragma_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN PRAGMA cache_size;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_reindex() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::Reindex(reindex_statement())),
            query_plan: false,
        };

        run_sunny_day_test("EXPLAIN REINDEX;", Statement::Explain(explain_statement));
    }

    #[test]
    fn explain_release() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::Release(release_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN RELEASE my_savepoint;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_rollback() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::RollbackTransaction(rollback_statement())),
            query_plan: false,
        };

        run_sunny_day_test("EXPLAIN ROLLBACK;", Statement::Explain(explain_statement));
    }

    #[test]
    fn explain_savepoint() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::Savepoint(savepoint_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN SAVEPOINT my_savepoint;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_select() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::Select(select_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN SELECT * FROM table_name1;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_update() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::Update(update_statement())),
            query_plan: false,
        };

        run_sunny_day_test(
            "EXPLAIN UPDATE table_name1 SET col1 = 1;",
            Statement::Explain(explain_statement),
        );
    }

    #[test]
    fn explain_vacuum() {
        let explain_statement = ExplainStatement {
            statement: Box::new(Statement::Vacuum(vacuum_statement())),
            query_plan: false,
        };

        run_sunny_day_test("EXPLAIN VACUUM;", Statement::Explain(explain_statement));
    }
}
