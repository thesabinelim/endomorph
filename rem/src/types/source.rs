use std::path::PathBuf;

#[derive(Clone)]
pub enum Source {
    File(PathBuf),
    Stdin,
}

pub struct Span {
    pub source: Source,
    pub start: Offset,
    pub end: Offset,
}

pub type Offset = usize;
