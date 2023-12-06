pub struct Source {
    pub file: String,
    pub text: String,
}

pub struct SourceError {
    pub offset: usize,
    pub message: String,
}

pub struct SourcePosition {
    pub line: usize,
    pub col: usize,
}
