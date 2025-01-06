use crate::OrderingTerm;

use super::{frame_spec::FrameSpec, Expression};

/// A window definition
#[derive(Debug, PartialEq, Clone, Default)]
pub struct WindowDefinition {
    /// The window name
    pub window_name: Option<String>,

    /// The partition by clause
    pub partition_by: Option<Vec<Expression>>,

    /// The order by clause
    pub order_by: Option<Vec<OrderingTerm>>,

    /// The frame spec
    pub frame_spec: Option<FrameSpec>,
}
