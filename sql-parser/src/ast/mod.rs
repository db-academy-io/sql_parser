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

use alter::{AlterTableStatement, RenameTableStatement};
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

// Top-level AST node representing any SQLite statement
pub enum Statement {
    // Data Query Language (DQL)
    Select(SelectStatement),

    // Data Manipulation Language (DML)
    Insert(InsertStatement),
    Update(UpdateStatement),
    Delete(DeleteStatement),

    // Data Definition Language (DDL)
    CreateTable(CreateTableStatement),
    AlterTable(AlterTableStatement),
    DropTable(DropTableStatement),
    CreateIndex(CreateIndexStatement),
    DropIndex(DropIndexStatement),
    CreateView(CreateViewStatement),
    DropView(DropViewStatement),
    CreateTrigger(CreateTriggerStatement),
    DropTrigger(DropTriggerStatement),
    CreateVirtualTable(CreateVirtualTableStatement),
    RenameTable(RenameTableStatement),

    // Transaction Control Language (TCL)
    BeginTransaction(BeginTransactionStatement),
    CommitTransaction(CommitTransactionStatement),
    RollbackTransaction(RollbackTransactionStatement),
    Savepoint(SavepointStatement),
    Release(SavepointReleaseStatement),

    // Administrative statements
    Pragma(PragmaStatement),
    Attach(AttachStatement),
    Detach(DetachStatement),
    Vacuum(VacuumStatement),
    Analyze(AnalyzeStatement),
    Reindex(ReindexStatement),

    // EXPLAIN statement (can wrap another statement)
    Explain(ExplainStatement),
}
