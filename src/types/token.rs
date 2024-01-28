use super::source::Span;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Span,
    pub text: String,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TokenKind {
    Comment,
    Dedent(usize),
    Error,
    Identifier,
    Indent(usize),
    Keyword(Keyword),
    LineBreak,
    Literal(Literal),
    Punctuation(Punctuation),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Keyword {
    Else,
    For,
    From,
    If,
    In,
    Let,
    Mut,
    Pub,
    Use,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Literal {
    Boolean,
    Char,
    Integer,
    Float,
    String,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Punctuation {
    And,
    AndAnd,
    BSlash,
    Caret,
    Colon,
    Comma,
    Dash,
    Dot,
    Ellipsis,
    Eq,
    EqEq,
    Exclaim,
    ExclaimEq,
    FSlash,
    Gt,
    GtEq,
    GtGt,
    GtGtGt,
    LBrace,
    LBracket,
    LParens,
    Lt,
    LtEq,
    LtLt,
    Modulo,
    NotEq,
    Pipe,
    PipePipe,
    Plus,
    Question,
    RArrow,
    RBrace,
    RBracket,
    RParens,
    Star,
    StarStar,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Whitespace {
    pub text: String,
}
