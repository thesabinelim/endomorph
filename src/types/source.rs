use std::path::PathBuf;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum SourceId {
    File(PathBuf),
    Stdin,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Span {
    pub start: Offset,
    pub end: Offset,
}

pub type Offset = usize;
