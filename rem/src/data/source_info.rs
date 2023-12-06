pub struct SourceError {
    pub location: SourceLocation,
    pub message: String,
}

pub struct SourceLocation {
    pub file: String,
    pub line_offset: usize,
    pub col_offset: usize,
}
