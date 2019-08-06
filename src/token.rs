use std::{
    collections::HashMap,
    fmt::{self, Display},
};

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
    String(String),
    FormattedString(String),
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

    // Used by LALRPOP
    #[doc(hidden)]
    Module,
    #[doc(hidden)]
    Interactive,
    #[doc(hidden)]
    Eval,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            Eof => write!(f, "EOF"),
            Newline => write!(f, "newline"),
            Indent => write!(f, "indent"),
            Dedent => write!(f, "dedent"),
            Name(name) => write!(f, "name {}", name),
            Integer(n) => write!(f, "integer {}", n),
            Float(n) => write!(f, "float {}", n),
            Imaginary(n) => write!(f, "complex {}", n),
            String(_) => write!(f, "string"),
            FormattedString(_) => write!(f, "formatted string"),
            Bytes(_) => write!(f, "bytes"),
            ParenOpen => write!(f, r#"symbol "(""#),
            ParenClose => write!(f, r#"symbol ")"#),
            BracketOpen => write!(f, r#"symbol "[""#),
            BracketClose => write!(f, r#"symbol "]""#),
            BraceOpen => write!(f, r#"symbol "{{""#),
            BraceClose => write!(f, r#"symbol \"}}"#),
            Colon => write!(f, r#"symbol ":""#),
            Comma => write!(f, r#"symbol ",""#),
            Semi => write!(f, r#"symbol ";""#),
            Plus => write!(f, r#"symbol "+""#),
            Minus => write!(f, r#"symbol "-""#),
            Star => write!(f, r#"symbol "*""#),
            Slash => write!(f, r#"symbol "/""#),
            Pipe => write!(f, r#"symbol "|""#),
            Amper => write!(f, r#"symbol "&""#),
            Less => write!(f, r#"symbol "<""#),
            Greater => write!(f, r#"symbol ">""#),
            Equal => write!(f, r#"symbol "=""#),
            Dot => write!(f, r#"symbol ".""#),
            Percent => write!(f, r#"symbol "%""#),
            EqEqual => write!(f, r#"symbol "==""#),
            NotEqual => write!(f, r#"symbol "!=""#),
            LessEqual => write!(f, r#"symbol "<=""#),
            GreaterEqual => write!(f, r#"symbol ">=""#),
            Tilde => write!(f, r#"symbol "~""#),
            Caret => write!(f, r#"symbol "^""#),
            LeftShift => write!(f, r#"symbol "<<""#),
            RightShift => write!(f, r#"symbol ">>""#),
            DoubleStar => write!(f, r#"symbol "**""#),
            PlusEqual => write!(f, r#"symbol "+=""#),
            MinEqual => write!(f, r#"symbol "-=""#),
            StarEqual => write!(f, r#"symbol "*=""#),
            SlashEqual => write!(f, r#"symbol "/=""#),
            PercentEqual => write!(f, r#"symbol "%=""#),
            AmperEqual => write!(f, r#"symbol "&=""#),
            PipeEqual => write!(f, r#"symbol "|=""#),
            CaretEqual => write!(f, r#"symbol "^=""#),
            LeftShiftEqual => write!(f, r#"symbol """#),
            RightShiftEqual => write!(f, r#"symbol ">>=""#),
            DoubleStarEqual => write!(f, r#"symbol "**=""#),
            DoubleSlash => write!(f, r#"symbol "//""#),
            DoubleSlashEqual => write!(f, r#"symbol "//=""#),
            At => write!(f, r#"symbol "@""#),
            AtEqual => write!(f, r#"symbol "@=""#),
            Rarrow => write!(f, r#"symbol "->""#),
            Ellipsis => write!(f, r#"symbol "...""#),
            False => write!(f, r#"keyword "False""#),
            None => write!(f, r#"keyword "None""#),
            True => write!(f, r#"keyword "True""#),
            And => write!(f, r#"keyword "and""#),
            As => write!(f, r#"keyword "as""#),
            Assert => write!(f, r#"keyword "assert""#),
            Break => write!(f, r#"keyword "break""#),
            Class => write!(f, r#"keyword "class""#),
            Continue => write!(f, r#"keyword "continue""#),
            Def => write!(f, r#"keyword "def""#),
            Del => write!(f, r#"keyword "del""#),
            Elif => write!(f, r#"keyword "elif""#),
            Else => write!(f, r#"keyword "else""#),
            Except => write!(f, r#"keyword "except""#),
            Finally => write!(f, r#"keyword "finally""#),
            For => write!(f, r#"keyword "for""#),
            From => write!(f, r#"keyword "from""#),
            Global => write!(f, r#"keyword "global""#),
            If => write!(f, r#"keyword "if""#),
            Import => write!(f, r#"keyword "import""#),
            In => write!(f, r#"keyword "in""#),
            Is => write!(f, r#"keyword "is""#),
            Lambda => write!(f, r#"keyword "lambda""#),
            Nonlocal => write!(f, r#"keyword "nonlocal""#),
            Not => write!(f, r#"keyword "not""#),
            Or => write!(f, r#"keyword "or""#),
            Pass => write!(f, r#"keyword "pass""#),
            Raise => write!(f, r#"keyword "raise""#),
            Return => write!(f, r#"keyword "return""#),
            Try => write!(f, r#"keyword "try""#),
            While => write!(f, r#"keyword "while""#),
            With => write!(f, r#"keyword "with""#),
            Yield => write!(f, r#"keyword "yield""#),
            Await => write!(f, r#"keyword "await""#),
            Async => write!(f, r#"keyword "async""#),

            Module => write!(f, "module"),
            Interactive => write!(f, "interactive"),
            Eval => write!(f, "eval"),
        }
    }
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
