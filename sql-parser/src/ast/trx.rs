#[derive(Debug, PartialEq)]
pub struct BeginTransactionStatement {}

#[derive(Debug, PartialEq)]
pub struct CommitTransactionStatement {}

#[derive(Debug, PartialEq)]
pub struct RollbackTransactionStatement {}

#[derive(Debug, PartialEq)]
pub struct SavepointStatement {}

#[derive(Debug, PartialEq)]
pub struct SavepointReleaseStatement {}
