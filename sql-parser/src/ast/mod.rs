mod alter;
mod create;
mod delete;
mod drop;
mod explain;
mod insert;
mod select;
mod sqlite;
mod trx;
mod update;

use alter::AlterTableStatement;
use create::{
    CreateIndexStatement, CreateTableStatement, CreateTriggerStatement, CreateViewStatement,
    CreateVirtualTableStatement,
};
use delete::DeleteStatement;
use drop::{DropIndexStatement, DropTableStatement, DropTriggerStatement, DropViewStatement};
use explain::ExplainStatement;
use insert::InsertStatement;
use select::SelectStatement;
use sqlite::{
    AnalyzeStatement, AttachStatement, DetachStatement, PragmaStatement, ReindexStatement,
    VacuumStatement,
};
use trx::{
    BeginTransactionStatement, CommitTransactionStatement, RollbackTransactionStatement,
    SavepointReleaseStatement, SavepointStatement,
};
use update::UpdateStatement;

/// Top-level AST node representing any SQLite statement
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
    /// Transaction Control Language (TCL), see [SavepointReleaseStatement]
    Release(SavepointReleaseStatement),

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

    /// EXPLAIN statement (can wrap another statement), see [ExplainStatement]
    Explain(ExplainStatement),
}
