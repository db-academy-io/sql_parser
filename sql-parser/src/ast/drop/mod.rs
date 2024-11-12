#[derive(Debug, PartialEq)]
pub struct DropIndexStatement {}

#[derive(Debug, PartialEq)]
pub struct DropViewStatement {}

#[derive(Debug, PartialEq)]
pub struct DropTriggerStatement {}

mod drop_table;
pub use drop_table::DropTableStatement;
