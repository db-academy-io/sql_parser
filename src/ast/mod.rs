mod alter;
mod create;
mod cte;
mod delete;
mod drop;
mod explain;
mod expression;
mod insert;
mod select;
mod sqlite;
mod trx;
mod update;

pub use alter::*;
pub use create::*;
pub use cte::*;
pub use delete::*;
pub use drop::*;
pub use explain::*;
pub use expression::*;
pub use insert::*;
pub use select::*;
pub use sqlite::*;
pub use trx::*;
pub use update::*;

/// Top-level AST node representing any SQLite statement
#[derive(Debug, PartialEq)]
pub enum Statement {
    /// Data Query Language (DQL), see [SelectStatement]
    Select(SelectStatement),

    /// Data Manipulation Language (DML), see [InsertStatement]
    Insert(InsertStatement),
    /// Data Manipulation Language (DML), see [UpdateStatement]
    Update(UpdateStatement),
    /// Data Manipulation Language (DML), see [DeleteStatement]
    Delete(DeleteStatement),

    /// Data Definition Language (DDL), see [CreateTableStatement]
    CreateTable(CreateTableStatement),
    /// Data Definition Language (DDL), see [AlterTableStatement]
    AlterTable(AlterTableStatement),
    /// Data Definition Language (DDL), see [DropTableStatement]
    DropTable(DropTableStatement),
    /// Data Definition Language (DDL), see [CreateIndexStatement]
    CreateIndex(CreateIndexStatement),
    /// Data Definition Language (DDL), see [DropIndexStatement]
    DropIndex(DropIndexStatement),
    /// Data Definition Language (DDL), see [CreateViewStatement]
    CreateView(CreateViewStatement),
    /// Data Definition Language (DDL), see [DropViewStatement]
    DropView(DropViewStatement),
    /// Data Definition Language (DDL), see [CreateTriggerStatement]
    CreateTrigger(CreateTriggerStatement),
    /// Data Definition Language (DDL), see [DropTriggerStatement]
    DropTrigger(DropTriggerStatement),
    /// Data Definition Language (DDL), see [CreateVirtualTableStatement]
    CreateVirtualTable(CreateVirtualTableStatement),

    /// Transaction Control Language (TCL), see [BeginTransactionStatement]
    BeginTransaction(BeginTransactionStatement),
    /// Transaction Control Language (TCL), see [CommitTransactionStatement]
    CommitTransaction(CommitTransactionStatement),
    /// Transaction Control Language (TCL), see [RollbackTransactionStatement]
    RollbackTransaction(RollbackTransactionStatement),
    /// Transaction Control Language (TCL), see [SavepointStatement]
    Savepoint(SavepointStatement),
    /// Transaction Control Language (TCL), see [ReleaseStatement]
    Release(ReleaseStatement),

    /// SQLite Administrative statements, see [PragmaStatement]
    Pragma(PragmaStatement),
    /// SQLite Administrative statements, see [AttachStatement]
    Attach(AttachStatement),
    /// SQLite Administrative statements, see [DetachStatement]
    Detach(DetachStatement),
    /// SQLite Administrative statements, see [VacuumStatement]
    Vacuum(VacuumStatement),
    /// SQLite Administrative statements, see [AnalyzeStatement]
    Analyze(AnalyzeStatement),
    /// SQLite Administrative statements, see [ReindexStatement]
    Reindex(ReindexStatement),

    /// WITH statement, see [WithCteStatement]
    WithCte(WithCteStatement),

    /// EXPLAIN statement (can wrap another statement), see [ExplainStatement]
    Explain(ExplainStatement),
}

/// An ordering term, used in the ORDER BY clause
#[derive(Debug, PartialEq, Clone)]
pub struct OrderingTerm {
    /// The expression to order by
    pub expression: Box<Expression>,
    /// The ordering
    pub ordering: Option<Ordering>,
    /// The nulls ordering
    pub nulls_ordering: Option<NullsOrdering>,
}

/// An ordering
#[derive(Debug, PartialEq, Clone)]
pub enum Ordering {
    /// Ascending
    Asc,
    /// Descending
    Desc,
}
