pub struct SourceError {
    location: SourceLocation,
    message: String,
}

pub struct SourceLocation {
    file: String,
    line_offset: usize,
    col_offset: usize,
}
