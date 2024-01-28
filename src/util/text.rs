pub fn remove_line_break(line: String) -> String {
    let Some(line) = line.strip_suffix('\n') else {
        return line;
    };
    let Some(line) = line.strip_suffix('\r') else {
        return line.to_string();
    };
    line.to_string()
}
