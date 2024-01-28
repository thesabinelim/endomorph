use super::Code;
use crate::{
    diagnostics::{join_message_parts, Annotation, Content, Level, MessagePart, Problem},
    types::source::{SourceId, Span},
};

pub fn syntax_error(source: SourceId, location: Span, expected: Vec<String>) -> Problem {
    Problem {
        source: source.clone(),
        location: location.start,
        level: Level::Error,
        code: Code::SyntaxError,
        name: "Syntax error".to_string(),
        content: vec![Content::Annotations {
            source,
            annotations: vec![Annotation {
                span: location,
                message: vec![
                    vec![MessagePart {
                        highlight: false,
                        text: "expected ".to_string(),
                    }],
                    join_message_parts(
                        expected
                            .into_iter()
                            .map(|token| MessagePart {
                                highlight: true,
                                text: token,
                            })
                            .collect(),
                        MessagePart {
                            highlight: false,
                            text: ", ".to_string(),
                        },
                    ),
                ]
                .concat(),
            }],
        }],
    }
}
