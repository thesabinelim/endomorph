use self::problems::Code;
use crate::types::source::{Offset, SourceId, Span};

pub mod problems;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Problem {
    pub source: SourceId,
    pub location: Offset,
    pub level: Level,
    pub code: Code,
    pub name: String,
    pub content: Vec<Content>,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Level {
    Error,
    Warn,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Content {
    Paragraph(Vec<MessagePart>),
    Annotations {
        source: SourceId,
        annotations: Vec<Annotation>,
    },
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Annotation {
    pub span: Span,
    pub message: Vec<MessagePart>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct MessagePart {
    pub highlight: bool,
    pub text: String,
}

pub fn join_message_parts(parts: Vec<MessagePart>, separator: MessagePart) -> Vec<MessagePart> {
    let len = parts.len();
    parts
        .into_iter()
        .flat_map(|part| [part, separator.clone()])
        .take(len * 2 - 1)
        .collect::<Vec<MessagePart>>()
}
