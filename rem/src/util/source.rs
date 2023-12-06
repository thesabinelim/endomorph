use crate::types::source::{Source, SourcePosition};

pub fn offset_to_position(source: &Source, offset: usize) -> SourcePosition {
    let chars = source.text.chars().collect::<Vec<char>>();
    let mut line = 0;
    let mut col = 0;
    for i in 0..offset {
        match chars[i] {
            '\n' => {
                line += 1;
                col = 0
            }
            '\r' => {}
            _ => col += 1,
        }
    }
    SourcePosition { line, col }
}
