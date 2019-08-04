use std::{
    fmt::{self, Display},
    ops::Deref,
};

use crate::source::{Location, Span, Spanned};

#[derive(Debug, Clone)]
pub enum Program {
    Module(Module),
    Interactive(Interactive),
    Eval(Eval),
}

#[derive(Debug, Clone)]
pub struct Module {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Interactive {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Eval {
    pub body: Box<Expression>,
}

macro_rules! impl_spanned {
    ($t:ty) => {
        impl $t {
            #[allow(dead_code)]
            pub(crate) fn spanned(self, start: Location, end: Location) -> Spanned<$t> {
                Spanned {
                    span: Span::new(start, end),
                    kind: self,
                }
            }
        }
    };
}

pub type Statement = Spanned<StatementKind>;

impl_spanned!(StatementKind);

#[derive(Debug, Clone)]
pub enum StatementKind {
    FunctionDef {
        name: String,
        args: Box<Arguments>,
        body: Vec<Statement>,
        decorator_list: Vec<Expression>,
        returns: Option<Box<Expression>>,
    },
    AsyncFunctionDef {
        name: String,
        args: Box<Arguments>,
        body: Vec<Statement>,
        decorator_list: Vec<Expression>,
        returns: Option<Box<Expression>>,
    },
    ClassDef {
        name: String,
        bases: Vec<Expression>,
        keywords: Vec<Keyword>,
        body: Vec<Statement>,
        decorator_list: Vec<Expression>,
    },
    Return {
        value: Option<Box<Expression>>,
    },
    Delete {
        targets: Vec<Expression>,
    },
    Assign {
        targets: Vec<Expression>,
        value: Box<Expression>,
    },
    AugAssign {
        target: Box<Expression>,
        op: Operator,
        value: Box<Expression>,
    },
    AnnAssign {
        target: Box<Expression>,
        annotation: Box<Expression>,
        value: Option<Box<Expression>>,
        simple: bool,
    },
    For {
        target: Box<Expression>,
        iter: Box<Expression>,
        body: Vec<Statement>,
        orelse: Vec<Statement>,
    },
    AsyncFor {
        target: Box<Expression>,
        iter: Box<Expression>,
        body: Vec<Statement>,
        orelse: Vec<Statement>,
    },
    While {
        test: Box<Expression>,
        body: Vec<Statement>,
        orelse: Vec<Statement>,
    },
    If {
        test: Box<Expression>,
        body: Vec<Statement>,
        orelse: Vec<Statement>,
    },
    With {
        items: Vec<WithItem>,
        body: Vec<Statement>,
    },
    AsyncWith {
        items: Vec<WithItem>,
        body: Vec<Statement>,
    },
    Raise {
        exc: Option<Box<Expression>>,
        cause: Option<Box<Expression>>,
    },
    Try {
        body: Vec<Statement>,
        handlers: Vec<ExceptHandler>,
        orelse: Vec<Statement>,
        finalbody: Vec<Statement>,
    },
    Assert {
        test: Box<Expression>,
        msg: Option<Box<Expression>>,
    },
    Import {
        names: Vec<Alias>,
    },
    ImportFrom {
        module: Option<String>,
        names: Vec<Alias>,
        level: usize,
    },
    Global {
        names: Vec<String>,
    },
    Nonlocal {
        names: Vec<String>,
    },
    Expr {
        value: Box<Expression>,
    },
    Pass,
    Break,
    Continue,
}

pub type Expression = Spanned<ExpressionKind>;

impl_spanned!(ExpressionKind);

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    BoolOp {
        left: Box<Expression>,
        op: BooleanOperator,
        right: Box<Expression>,
    },
    BinOp {
        left: Box<Expression>,
        op: Operator,
        right: Box<Expression>,
    },
    UnaryOp {
        op: UnaryOperator,
        operand: Box<Expression>,
    },
    Lambda {
        args: Box<Arguments>,
        body: Box<Expression>,
    },
    IfExp {
        test: Box<Expression>,
        body: Box<Expression>,
        orelse: Box<Expression>,
    },
    Dict {
        keys: Vec<Option<Expression>>,
        values: Vec<Expression>,
    },
    Set {
        elts: Vec<Expression>,
    },
    ListComp {
        elt: Box<Expression>,
        generators: Vec<Comprehension>,
    },
    SetComp {
        elt: Box<Expression>,
        generators: Vec<Comprehension>,
    },
    DictComp {
        key: Box<Expression>,
        value: Box<Expression>,
        generators: Vec<Comprehension>,
    },
    GeneratorExp {
        elt: Box<Expression>,
        generators: Vec<Comprehension>,
    },
    Await {
        value: Box<Expression>,
    },
    Yield {
        value: Option<Box<Expression>>,
    },
    YieldFrom {
        value: Box<Expression>,
    },
    Compare {
        left: Box<Expression>,
        ops: Vec<ComparisonOperator>,
        comparators: Vec<Expression>,
    },
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
        keywords: Vec<Keyword>,
    },
    Num {
        n: Number,
    },
    Str {
        s: String,
    },
    FormattedValue {
        value: Box<Expression>,
        conversion: Option<Conversion>,
        format_spec: Box<Expression>,
    },
    JoinedStr {
        values: Vec<Expression>,
    },
    Bytes {
        s: Vec<u8>,
    },
    NameConstant {
        value: Singleton,
    },
    Ellipsis,
    Attribute {
        value: Box<Expression>,
        attr: String,
    },
    Subscript {
        value: Box<Expression>,
        slice: Slice,
    },
    Starred {
        value: Box<Expression>,
    },
    Name {
        id: String,
    },
    List {
        elts: Vec<Expression>,
    },
    Tuple {
        elts: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum Slice {
    Slice {
        lower: Option<Box<Expression>>,
        upper: Option<Box<Expression>>,
        step: Option<Box<Expression>>,
    },
    ExtSlice {
        dims: Vec<Slice>,
    },
    Index {
        value: Box<Expression>,
    },
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BooleanOperator {
    And,
    Or,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    MatrixMultiplication,
    Division,
    Modulo,
    Power,
    LeftShift,
    RightShift,
    BitOr,
    BitXor,
    BitAnd,
    FloorDivision,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum UnaryOperator {
    Invert,
    Not,
    Plus,
    Minus,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ComparisonOperator {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Is,
    IsNot,
    In,
    NotIn,
}

#[derive(Debug, Clone)]
pub struct Comprehension {
    pub target: Box<Expression>,
    pub iter: Box<Expression>,
    pub ifs: Vec<Expression>,
    pub is_async: bool,
}

#[derive(Debug, Clone)]
pub struct ExceptHandler {
    pub span: Span,
    pub typ: Option<Box<Expression>>,
    pub name: Option<String>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, Default)]
pub struct Arguments {
    pub args: Vec<Arg>,
    pub vararg: Option<Arg>,
    pub kwonlyargs: Vec<Arg>,
    pub kwarg: Option<Arg>,
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub span: Span,
    pub arg: String,
    pub annotation: Option<Box<Expression>>,
    pub kind: ArgKind,
}

#[derive(Debug, Clone)]
pub enum ArgKind {
    Required,
    Optional(Box<Expression>),
    Vararg,
    Kwarg,
}

#[derive(Debug, Clone)]
pub struct Keyword {
    pub arg: Option<String>,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Alias {
    pub identifier: String,
    pub asname: Option<String>,
}

#[derive(Debug, Clone)]
pub struct WithItem {
    pub context_expr: Box<Expression>,
    pub optional_vars: Option<Box<Expression>>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Singleton {
    None,
    True,
    False,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Integer(num_bigint::BigUint),
    Float(f64),
    Complex(f64),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Conversion {
    Str,
    Repr,
    Ascii,
}

fn write_joined<I, T, S>(f: &mut fmt::Formatter<'_>, i: I, sep: S) -> fmt::Result
where
    I: IntoIterator<Item = T>,
    T: Display,
    S: Display,
{
    let mut iter = i.into_iter();
    if let Some(e) = iter.next() {
        write!(f, "{}", e)?;
    }
    for e in iter {
        write!(f, "{}{}", sep, e)?;
    }
    Ok(())
}

fn write_escaped(f: &mut fmt::Formatter<'_>, b: u8) -> fmt::Result {
    match b {
        b' ' => f.write_str(" "),
        b'"' => f.write_str("\\\""),
        b'\\' => f.write_str("\\"),
        b'\n' => f.write_str("\\n"),
        b'\r' => f.write_str("\\r"),
        b'\t' => f.write_str("\\t"),
        b'\x07' => f.write_str("\\a"),
        b'\x08' => f.write_str("\\b"),
        b'\x0c' => f.write_str("\\f"),
        b'\x0b' => f.write_str("\\v"),
        b if b.is_ascii_graphic() => f.write_str(&char::from(b).to_string()),
        b => write!(f, "\\x{:x}", b),
    }
}

fn write_indent(f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
    for _ in 0..level * 4 {
        f.write_str(" ")?;
    }
    Ok(())
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Program::Module(p) => write!(f, "{}", p),
            Program::Interactive(p) => write!(f, "{}", p),
            Program::Eval(p) => write!(f, "{}", p),
        }
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_joined(f, &self.body, "\n")
    }
}

impl Display for Interactive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_joined(f, &self.body, "\n")
    }
}

impl Display for Eval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.body)
    }
}

fn write_body(f: &mut fmt::Formatter<'_>, indent: usize, body: &[Statement]) -> fmt::Result {
    for e in body {
        e.fmt_indented(f, indent)?;
        f.write_str("\n")?;
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn write_funcdef(
    f: &mut fmt::Formatter<'_>,
    indent: usize,
    name: &str,
    args: &Arguments,
    body: &[Statement],
    decorator_list: &[Expression],
    returns: Option<&Expression>,
    is_async: bool,
) -> fmt::Result {
    for dec in decorator_list {
        writeln!(f, "@{}", dec)?;
        write_indent(f, indent)?;
    }
    if is_async {
        f.write_str("async ")?;
    }
    write!(f, "def {}({})", name, args)?;
    if let Some(returns) = returns {
        write!(f, " -> {}", returns)?;
    }
    f.write_str(":\n")?;
    write_body(f, indent + 1, body)?;
    Ok(())
}

fn write_for(
    f: &mut fmt::Formatter<'_>,
    indent: usize,
    target: &Expression,
    iter: &Expression,
    body: &[Statement],
    orelse: &[Statement],
    is_async: bool,
) -> fmt::Result {
    if is_async {
        f.write_str("async ")?;
    }
    writeln!(f, "for {} in {}:", target, iter)?;
    write_body(f, indent + 1, body)?;
    if !orelse.is_empty() {
        write_indent(f, indent)?;
        f.write_str("else:\n")?;
        write_body(f, indent + 1, orelse)?;
    }
    Ok(())
}

fn write_if(
    f: &mut fmt::Formatter<'_>,
    indent: usize,
    test: &Expression,
    body: &[Statement],
    orelse: &[Statement],
) -> fmt::Result {
    writeln!(f, "if {}:", test)?;
    write_body(f, indent + 1, body)?;
    match orelse {
        [Statement {
            kind: StatementKind::If { test, body, orelse },
            ..
        }] => {
            write_indent(f, indent)?;
            f.write_str("el")?;
            write_if(f, indent, test, body, orelse)?;
        }
        [] => (),
        s => {
            write_indent(f, indent)?;
            f.write_str("else:\n")?;
            write_body(f, indent + 1, s)?;
        }
    };
    Ok(())
}

fn write_with(
    f: &mut fmt::Formatter<'_>,
    indent: usize,
    items: &[WithItem],
    body: &[Statement],
    is_async: bool,
) -> fmt::Result {
    if is_async {
        f.write_str("async ")?;
    }
    f.write_str("with ")?;
    write_joined(f, items, ", ")?;
    f.write_str(":\n")?;
    write_body(f, indent + 1, body)
}

impl Statement {
    fn fmt_indented(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        match &self.kind {
            StatementKind::FunctionDef {
                name,
                args,
                body,
                decorator_list,
                returns,
            } => write_funcdef(
                f,
                indent,
                name,
                args,
                body,
                decorator_list,
                returns.as_ref().map(Deref::deref),
                false,
            ),
            StatementKind::AsyncFunctionDef {
                name,
                args,
                body,
                decorator_list,
                returns,
            } => write_funcdef(
                f,
                indent,
                name,
                args,
                body,
                decorator_list,
                returns.as_ref().map(Deref::deref),
                true,
            ),
            StatementKind::ClassDef {
                name,
                bases,
                keywords,
                body,
                decorator_list,
            } => {
                for dec in decorator_list {
                    writeln!(f, "@{}", dec)?;
                    write_indent(f, indent)?;
                }
                write!(f, "class {}", name)?;
                if !bases.is_empty() || !keywords.is_empty() {
                    f.write_str("(")?;
                    write_joined(f, bases, ", ")?;
                    if !bases.is_empty() && !keywords.is_empty() {
                        f.write_str(", ")?;
                    }
                    write_joined(f, keywords, ", ")?;
                    f.write_str(")")?;
                }
                f.write_str(":\n")?;
                write_body(f, indent + 1, body)
            }
            StatementKind::Return { value } => match value {
                Some(v) => write!(f, "return {}", v),
                _ => write!(f, "return"),
            },
            StatementKind::Delete { targets } => {
                write!(f, "del ")?;
                write_joined(f, targets, ", ")
            }
            StatementKind::Assign { targets, value } => {
                write_joined(f, targets, " = ")?;
                write!(f, " = {}", value)
            }
            StatementKind::AugAssign { target, op, value } => {
                write!(f, "{} {}= {}", target, op, value)
            }
            StatementKind::AnnAssign {
                target,
                annotation,
                value,
                ..
            } => {
                write!(f, "{}: {}", target, annotation)?;
                if let Some(value) = value {
                    write!(f, " = {}", value)?;
                }
                Ok(())
            }
            StatementKind::For {
                target,
                iter,
                body,
                orelse,
            } => write_for(f, indent, target, iter, body, orelse, false),
            StatementKind::AsyncFor {
                target,
                iter,
                body,
                orelse,
            } => write_for(f, indent, target, iter, body, orelse, true),
            StatementKind::While { test, body, orelse } => {
                writeln!(f, "while {}:", test)?;
                write_body(f, indent + 1, body)?;
                if !orelse.is_empty() {
                    write_indent(f, indent)?;
                    f.write_str("else:\n")?;
                    write_body(f, indent + 1, orelse)?;
                }
                Ok(())
            }
            StatementKind::If { test, body, orelse } => write_if(f, indent, test, body, orelse),
            StatementKind::With { items, body } => write_with(f, indent, items, body, false),
            StatementKind::AsyncWith { items, body } => write_with(f, indent, items, body, true),
            StatementKind::Raise { exc, cause } => {
                write!(f, "raise")?;
                if let Some(exc) = exc {
                    write!(f, " {}", exc)?;
                }
                if let Some(cause) = cause {
                    write!(f, " from {}", cause)?;
                }
                Ok(())
            }
            StatementKind::Try {
                body,
                handlers,
                orelse,
                finalbody,
            } => {
                f.write_str("try:\n")?;
                write_body(f, indent + 1, body)?;
                for hdl in handlers {
                    hdl.fmt_indented(f, indent)?;
                }
                if !orelse.is_empty() {
                    write_indent(f, indent)?;
                    f.write_str("else:\n")?;
                    write_body(f, indent + 1, orelse)?;
                }
                if !finalbody.is_empty() {
                    write_indent(f, indent)?;
                    f.write_str("finally:\n")?;
                    write_body(f, indent + 1, finalbody)?;
                }
                Ok(())
            }
            StatementKind::Assert { test, msg } => {
                write!(f, "assert {}", test)?;
                if let Some(msg) = msg {
                    write!(f, ", {}", msg)?;
                }
                Ok(())
            }
            StatementKind::Import { names } => {
                f.write_str("import ")?;
                write_joined(f, names, ", ")
            }
            StatementKind::ImportFrom {
                module,
                names,
                level,
            } => {
                f.write_str("from ")?;
                for _ in 0..*level {
                    f.write_str(".")?;
                }
                if let Some(module) = module {
                    write!(f, "{}", module)?;
                }
                f.write_str(" import ")?;
                write_joined(f, names, ", ")
            }
            StatementKind::Global { names } => {
                f.write_str("global ")?;
                write_joined(f, names, ", ")
            }
            StatementKind::Nonlocal { names } => {
                f.write_str("nonlocal ")?;
                write_joined(f, names, ", ")
            }
            StatementKind::Expr { value } => write!(f, "{}", value),
            StatementKind::Pass => f.write_str("pass"),
            StatementKind::Break => f.write_str("break"),
            StatementKind::Continue => f.write_str("continue"),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_indented(f, 0)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for ExpressionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExpressionKind::BoolOp { left, op, right } => write!(f, "{} {} {}", left, op, right),
            ExpressionKind::BinOp { left, op, right } => write!(f, "{} {} {}", left, op, right),
            ExpressionKind::UnaryOp { op, operand } => write!(f, "{} {}", op, operand),
            ExpressionKind::Lambda { args, body, .. } => write!(f, "lambda {}: {}", args, body),
            ExpressionKind::IfExp { test, body, orelse } => {
                write!(f, "{} if {} else {}", body, test, orelse)
            }
            ExpressionKind::Dict { keys, values } => {
                fn write_kv(
                    f: &mut fmt::Formatter<'_>,
                    key: &Option<Expression>,
                    value: &Expression,
                ) -> fmt::Result {
                    match key {
                        Some(k) => write!(f, "{}: ", k)?,
                        _ => write!(f, "**")?,
                    };
                    write!(f, "{}", value)
                }
                f.write_str("{")?;
                let mut iter = keys.iter().zip(values);
                if let Some((k, v)) = iter.next() {
                    write_kv(f, k, v)?;
                }
                for (k, v) in iter {
                    write!(f, ", ")?;
                    write_kv(f, k, v)?;
                }
                f.write_str("}")
            }
            ExpressionKind::Set { elts, .. } => {
                f.write_str("{")?;
                write_joined(f, elts, ", ")?;
                f.write_str("}")
            }
            ExpressionKind::ListComp { elt, generators } => {
                write!(f, "[{} ", elt)?;
                write_joined(f, generators, " ")?;
                f.write_str("]")
            }
            ExpressionKind::SetComp { elt, generators } => {
                write!(f, "{{{} ", elt)?;
                write_joined(f, generators, " ")?;
                f.write_str("}")
            }
            ExpressionKind::DictComp {
                key,
                value,
                generators,
            } => {
                write!(f, "{{{}: {} ", key, value)?;
                write_joined(f, generators, " ")?;
                f.write_str("}")
            }
            ExpressionKind::GeneratorExp { elt, generators } => {
                write!(f, "({} ", elt)?;
                write_joined(f, generators, " ")?;
                f.write_str(")")
            }
            ExpressionKind::Await { value } => write!(f, "await {}", value),
            ExpressionKind::Yield { value } => match value {
                Some(value) => write!(f, "yield {}", value),
                _ => write!(f, "yield"),
            },
            ExpressionKind::YieldFrom { value } => write!(f, "yield from {}", value),
            ExpressionKind::Compare {
                left,
                ops,
                comparators,
            } => {
                write!(f, "{}", left)?;
                for (op, e) in ops.iter().zip(comparators) {
                    write!(f, " {} {}", op, e)?
                }
                Ok(())
            }
            ExpressionKind::Call {
                func,
                args,
                keywords,
            } => {
                write!(f, "{}(", func)?;
                write_joined(f, args, ", ")?;
                if !args.is_empty() && !keywords.is_empty() {
                    f.write_str(", ")?;
                }
                write_joined(f, keywords, ", ")?;
                f.write_str(")")
            }
            ExpressionKind::Num { n } => write!(f, "{}", n),
            ExpressionKind::Str { s } => {
                f.write_str("\"")?;
                for b in s.bytes() {
                    write_escaped(f, b)?;
                }
                f.write_str("\"")
            }
            ExpressionKind::FormattedValue { .. } => unimplemented!(),
            ExpressionKind::JoinedStr { .. } => unimplemented!(),
            ExpressionKind::Bytes { s } => {
                f.write_str("b\"")?;
                for b in s {
                    write_escaped(f, *b)?;
                }
                f.write_str("\"")
            }
            ExpressionKind::NameConstant { value } => write!(f, "{}", value),
            ExpressionKind::Ellipsis { .. } => write!(f, "..."),
            ExpressionKind::Attribute { value, attr, .. } => write!(f, "{}.{}", value, attr),
            ExpressionKind::Subscript { value, slice, .. } => write!(f, "{}[{}]", value, slice),
            ExpressionKind::Starred { value, .. } => write!(f, "*{}", value),
            ExpressionKind::Name { id, .. } => write!(f, "{}", id),
            ExpressionKind::List { elts, .. } => {
                f.write_str("[")?;
                write_joined(f, elts, ", ")?;
                f.write_str("]")
            }
            ExpressionKind::Tuple { elts, .. } => {
                f.write_str("(")?;
                write_joined(f, elts, ", ")?;
                f.write_str(")")
            }
        }
    }
}

impl Display for Slice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Slice::Slice { lower, upper, step } => {
                if let Some(lower) = lower {
                    write!(f, "{}", lower)?;
                }
                f.write_str(":")?;
                if let Some(upper) = upper {
                    write!(f, "{}", upper)?;
                }
                f.write_str(":")?;
                if let Some(step) = step {
                    write!(f, "{}", step)?;
                }
                Ok(())
            }
            Slice::ExtSlice { dims } => write_joined(f, dims, ","),
            Slice::Index { value } => write!(f, "{}", value),
        }
    }
}

impl Display for BooleanOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BooleanOperator::And => "and",
            BooleanOperator::Or => "or",
        };
        f.write_str(s)
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Operator::Addition => "+",
            Operator::Subtraction => "-",
            Operator::Multiplication => "*",
            Operator::MatrixMultiplication => "@",
            Operator::Division => "/",
            Operator::Modulo => "%",
            Operator::Power => "**",
            Operator::LeftShift => "<<",
            Operator::RightShift => ">>",
            Operator::BitOr => "|",
            Operator::BitXor => "^",
            Operator::BitAnd => "&",
            Operator::FloorDivision => "//",
        };
        f.write_str(s)
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            UnaryOperator::Invert => "~",
            UnaryOperator::Not => "not",
            UnaryOperator::Plus => "+",
            UnaryOperator::Minus => "-",
        };
        f.write_str(s)
    }
}

impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            ComparisonOperator::Equal => "==",
            ComparisonOperator::NotEqual => "!=",
            ComparisonOperator::Less => "<",
            ComparisonOperator::LessEqual => "<=",
            ComparisonOperator::Greater => ">",
            ComparisonOperator::GreaterEqual => ">=",
            ComparisonOperator::Is => "is",
            ComparisonOperator::IsNot => "is not",
            ComparisonOperator::In => "in",
            ComparisonOperator::NotIn => "not in",
        };
        f.write_str(s)
    }
}

impl Display for Comprehension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_async {
            f.write_str("async ")?;
        }
        write!(f, "for {} in {}", self.target, self.iter)?;
        for e in &self.ifs {
            write!(f, " if {}", e)?;
        }
        Ok(())
    }
}

impl ExceptHandler {
    fn fmt_indented(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        f.write_str("except")?;
        if let Some(typ) = &self.typ {
            write!(f, " {}", typ)?;
        }
        if let Some(name) = &self.name {
            write!(f, " as {}", name)?;
        }
        f.write_str(":\n")?;
        write_body(f, indent + 1, &self.body)
    }
}

impl Display for Arguments {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_joined(
            f,
            self.args
                .iter()
                .chain(&self.vararg)
                .chain(&self.kwonlyargs)
                .chain(&self.kwarg),
            ", ",
        )
    }
}

impl Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ArgKind::Vararg => f.write_str("*")?,
            ArgKind::Kwarg => f.write_str("**")?,
            _ => (),
        }
        f.write_str(&self.arg)?;
        if let Some(a) = &self.annotation {
            write!(f, ": {}", &a)?;
        }
        if let ArgKind::Optional(default) = &self.kind {
            write!(f, " = {}", &default)?;
        }
        Ok(())
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.arg {
            Some(arg) => write!(f, "{}={}", arg, self.value),
            _ => write!(f, "**{}", self.value),
        }
    }
}

impl Display for Alias {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.identifier)?;
        if let Some(asname) = &self.asname {
            write!(f, " as {}", asname)?;
        }
        Ok(())
    }
}

impl Display for WithItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.context_expr)?;
        if let Some(vars) = &self.optional_vars {
            write!(f, " as {}", vars)?;
        }
        Ok(())
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number::Integer(n) => write!(f, "{}", n),
            Number::Float(n) | Number::Complex(n) => write!(f, "{}", n),
        }
    }
}

impl Display for Singleton {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Singleton::None => "None",
            Singleton::True => "True",
            Singleton::False => "False",
        };
        f.write_str(s)
    }
}
