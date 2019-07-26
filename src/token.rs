use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Eof,
    Newline,
    Indent,
    Dedent,
    Name(String),
    Integer(num_bigint::BigUint),
    Float(f64),
    Imaginary(f64),
    String {
        value: String,
        formatted: bool,
    },
    Bytes(Vec<u8>),

    // Symbols
    //
    // `(`
    ParenOpen,
    // `)`
    ParenClose,
    // `[`
    BracketOpen,
    // `]`
    BracketClose,
    // `{`
    BraceOpen,
    // `}`
    BraceClose,
    // `:`
    Colon,
    // `,`
    Comma,
    // `;`
    Semi,
    // `+`
    Plus,
    // `-`
    Minus,
    // `*`
    Star,
    // `/`
    Slash,
    // `|`
    Pipe,
    // `&`
    Amper,
    // `<`
    Less,
    // `>`
    Greater,
    // `=`
    Equal,
    // `.`
    Dot,
    // `%`
    Percent,
    // `==`
    EqEqual,
    // `!=`
    NotEqual,
    // `<=`
    LessEqual,
    // `>=`
    GreaterEqual,
    // `~`
    Tilde,
    // `^`
    Caret,
    // `<<`
    LeftShift,
    // `>>`
    RightShift,
    // `**`
    DoubleStar,
    // `+=`
    PlusEqual,
    // `-=`
    MinEqual,
    // `*=`
    StarEqual,
    // `/=`
    SlashEqual,
    // `%=`
    PercentEqual,
    // `&=`
    AmperEqual,
    // `|=`
    PipeEqual,
    // `^=`
    CaretEqual,
    // `<<=`
    LeftShiftEqual,
    // `>>=`
    RightShiftEqual,
    // `**=`
    DoubleStarEqual,
    // `//`
    DoubleSlash,
    // `//=`
    DoubleSlashEqual,
    // `@`
    At,
    // `@=`
    AtEqual,
    // `->`
    Rarrow,
    // `...`
    Ellipsis,

    // Keywords
    //
    // `False`
    False,
    // `None`
    None,
    // `True`
    True,
    // `and`
    And,
    // `as`
    As,
    // `assert`
    Assert,
    // `break`
    Break,
    // `class`
    Class,
    // `continue`
    Continue,
    // `def`
    Def,
    // `del`
    Del,
    // `elif`
    Elif,
    // `else`
    Else,
    // `except`
    Except,
    // `finally`
    Finally,
    // `for`
    For,
    // `from`
    From,
    // `global`
    Global,
    // `if`
    If,
    // `import`
    Import,
    // `in`
    In,
    // `is`
    Is,
    // `lambda`
    Lambda,
    // `nonlocal`
    Nonlocal,
    // `not`
    Not,
    // `or`
    Or,
    // `pass`
    Pass,
    // `raise`
    Raise,
    // `return`
    Return,
    // `try`
    Try,
    // `while`
    While,
    // `with`
    With,
    // `yield`
    Yield,
    // `await`
    Await,
    // `async`
    Async,
}

lazy_static::lazy_static! {
    pub(crate) static ref SYMBOLS: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.insert("(", Token::ParenOpen);
        m.insert(")", Token::ParenClose);
        m.insert("[", Token::BracketOpen);
        m.insert("]", Token::BracketClose);
        m.insert("{", Token::BraceOpen);
        m.insert("}", Token::BraceClose);
        m.insert(":", Token::Colon);
        m.insert(",", Token::Comma);
        m.insert(";", Token::Semi);
        m.insert("+", Token::Plus);
        m.insert("-", Token::Minus);
        m.insert("*", Token::Star);
        m.insert("/", Token::Slash);
        m.insert("|", Token::Pipe);
        m.insert("&", Token::Amper);
        m.insert("<", Token::Less);
        m.insert(">", Token::Greater);
        m.insert("=", Token::Equal);
        m.insert(".", Token::Dot);
        m.insert("%", Token::Percent);
        m.insert("==", Token::EqEqual);
        m.insert("!=", Token::NotEqual);
        m.insert("<=", Token::LessEqual);
        m.insert(">=", Token::GreaterEqual);
        m.insert("~", Token::Tilde);
        m.insert("^", Token::Caret);
        m.insert("<<", Token::LeftShift);
        m.insert(">>", Token::RightShift);
        m.insert("**", Token::DoubleStar);
        m.insert("+=", Token::PlusEqual);
        m.insert("-=", Token::MinEqual);
        m.insert("*=", Token::StarEqual);
        m.insert("/=", Token::SlashEqual);
        m.insert("%=", Token::PercentEqual);
        m.insert("&=", Token::AmperEqual);
        m.insert("|=", Token::PipeEqual);
        m.insert("^=", Token::CaretEqual);
        m.insert("<<=", Token::LeftShiftEqual);
        m.insert(">>=", Token::RightShiftEqual);
        m.insert("**=", Token::DoubleStarEqual);
        m.insert("//", Token::DoubleSlash);
        m.insert("//=", Token::DoubleSlashEqual);
        m.insert("@", Token::At);
        m.insert("@=", Token::AtEqual);
        m.insert("->", Token::Rarrow);
        m.insert("...", Token::Ellipsis);
        m
    };
    pub(crate) static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.insert("False", Token::False);
        m.insert("None", Token::None);
        m.insert("True", Token::True);
        m.insert("and", Token::And);
        m.insert("as", Token::As);
        m.insert("assert", Token::Assert);
        m.insert("break", Token::Break);
        m.insert("class", Token::Class);
        m.insert("continue", Token::Continue);
        m.insert("def", Token::Def);
        m.insert("del", Token::Del);
        m.insert("elif", Token::Elif);
        m.insert("else", Token::Else);
        m.insert("except", Token::Except);
        m.insert("finally", Token::Finally);
        m.insert("for", Token::For);
        m.insert("from", Token::From);
        m.insert("global", Token::Global);
        m.insert("if", Token::If);
        m.insert("import", Token::Import);
        m.insert("in", Token::In);
        m.insert("is", Token::Is);
        m.insert("lambda", Token::Lambda);
        m.insert("nonlocal", Token::Nonlocal);
        m.insert("not", Token::Not);
        m.insert("or", Token::Or);
        m.insert("pass", Token::Pass);
        m.insert("raise", Token::Raise);
        m.insert("return", Token::Return);
        m.insert("try", Token::Try);
        m.insert("while", Token::While);
        m.insert("with", Token::With);
        m.insert("yield", Token::Yield);
        m.insert("await", Token::Await);
        m.insert("async", Token::Async);
        m
    };
}
