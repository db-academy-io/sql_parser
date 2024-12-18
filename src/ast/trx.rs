/// An AST for [BEGIN TRANSACTION](https://www.sqlite.org/lang_transaction.html) SQL statement.
#[derive(Debug, PartialEq, Default)]
pub struct BeginTransactionStatement {
    pub transaction_type: Option<TransactionType>,
}

/// The [type](https://www.sqlite.org/lang_transaction.html) of transaction,
/// i.e. DEFERRED, IMMEDIATE, or EXCLUSIVE.
#[derive(Debug, PartialEq)]
pub enum TransactionType {
    /// DEFERRED means that the transaction does not actually start until the
    /// database is first accessed. Internally, the BEGIN DEFERRED statement
    /// merely sets a flag on the database connection that turns off the
    /// automatic commit that would normally occur when the last statement
    /// finishes. This causes the transaction that is automatically started to
    /// persist until an explicit COMMIT or ROLLBACK or until a rollback is
    /// provoked by an error or an ON CONFLICT ROLLBACK clause. If the first
    /// statement after BEGIN DEFERRED is a SELECT, then a read transaction is
    /// started.
    ///
    /// Subsequent write statements will upgrade the transaction to a write
    /// transaction if possible, or return SQLITE_BUSY. If the first statement
    /// after BEGIN DEFERRED is a write statement, then a write transaction
    /// is started.
    ///
    /// This is the default database transaction behavior
    Deferred,

    /// IMMEDIATE causes the database connection to start a new write
    /// immediately, without waiting for a write statement. The BEGIN IMMEDIATE
    /// might fail with SQLITE_BUSY if another write transaction is already
    /// active on another database connection.
    Immediate,

    /// EXCLUSIVE is similar to IMMEDIATE in that a write transaction is
    /// started immediately. EXCLUSIVE and IMMEDIATE are the same in WAL mode,
    /// but in other journaling modes, EXCLUSIVE prevents other database
    /// connections from reading the database while the transaction is
    /// underway.
    Exclusive,
}

impl Default for TransactionType {
    fn default() -> Self {
        Self::Deferred
    }
}

/// An AST for [COMMIT TRANSACTION](https://www.sqlite.org/lang_transaction.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct CommitTransactionStatement;

/// An AST for [ROLLBACK TRANSACTION](https://www.sqlite.org/lang_transaction.html) SQL statement.
#[derive(Debug, PartialEq, Default)]
pub struct RollbackTransactionStatement {
    pub savepoint_name: Option<String>,
}

/// An AST for [SAVEPOINT](https://www.sqlite.org/lang_savepoint.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct SavepointStatement {
    pub savepoint_name: String,
}

/// An AST for [RELEASE](https://www.sqlite.org/lang_savepoint.html) SQL statement.
#[derive(Debug, PartialEq)]
pub struct ReleaseStatement {
    pub savepoint_name: String,
}
