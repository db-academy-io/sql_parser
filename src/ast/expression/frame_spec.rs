use super::Expression;

/// A frame spec
#[derive(Debug, PartialEq, Clone)]
pub struct FrameSpec {
    /// The frame type
    pub frame_type: FrameType,

    /// The frame spec type
    pub frame_spec_type: FrameSpecType,

    /// The exclude clause
    pub exclude: Option<FrameSpecExclude>,
}

/// A frame type
#[derive(Debug, PartialEq, Clone)]
pub enum FrameType {
    /// Rows frame type
    Rows,

    /// Range frame type
    Range,

    /// Groups frame type
    Groups,
}

/// A frame spec type
#[derive(Debug, PartialEq, Clone)]
pub enum FrameSpecType {
    /// A between frame spec
    Between(BetweenFrameSpec),

    /// Unbounded preceding
    UnboundedPreceding,

    /// Preceding frame spec
    Preceding(Box<Expression>),

    /// Current row
    CurrentRow,
}

/// A between frame spec
#[derive(Debug, PartialEq, Clone)]
pub struct BetweenFrameSpec {
    /// The start frame spec type
    pub start: BetweenFrameSpecType,

    /// The end frame spec type
    pub end: BetweenFrameSpecType,
}

/// A between frame spec type
#[derive(Debug, PartialEq, Clone)]
pub enum BetweenFrameSpecType {
    /// Unbounded preceding
    UnboundedPreceding,

    /// Preceding frame spec
    Preceding(Box<Expression>),

    /// Current row
    CurrentRow,

    /// Following frame spec
    Following(Box<Expression>),

    /// Unbounded following
    UnboundedFollowing,
}

/// A frame spec exclude
#[derive(Debug, PartialEq, Clone)]
pub enum FrameSpecExclude {
    /// No others
    NoOthers,

    /// Current row
    CurrentRow,

    /// Group
    Group,

    /// Ties
    Ties,
}
