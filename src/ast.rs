use std::fmt::{self, Display};

use crate::source::Span;

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

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub span: Span,
    pub name: String,
    pub args: Box<Arguments>,
    pub body: Vec<Statement>,
    pub decorator_list: Vec<Expression>,
    pub returns: Option<Box<Expression>>,
}

pub type AsyncFunctionDef = FunctionDef;

#[derive(Debug, Clone)]
pub struct ClassDef {
    pub span: Span,
    pub name: String,
    pub bases: Vec<Expression>,
    pub keywords: Vec<Keyword>,
    pub body: Vec<Statement>,
    pub decorator_list: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub span: Span,
    pub value: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct Delete {
    pub span: Span,
    pub targets: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub span: Span,
    pub targets: Vec<Expression>,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct AugAssign {
    pub span: Span,
    pub target: Box<Expression>,
    pub op: OpKind,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct AnnAssign {
    pub span: Span,
    pub target: Box<Expression>,
    pub annotation: Box<Expression>,
    pub value: Option<Box<Expression>>,
    pub simple: bool,
}

#[derive(Debug, Clone)]
pub struct For {
    pub span: Span,
    pub target: Box<Expression>,
    pub iter: Box<Expression>,
    pub body: Vec<Statement>,
    pub orelse: Vec<Statement>,
}

pub type AsyncFor = For;

#[derive(Debug, Clone)]
pub struct While {
    pub span: Span,
    pub test: Box<Expression>,
    pub body: Vec<Statement>,
    pub orelse: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub span: Span,
    pub test: Box<Expression>,
    pub body: Vec<Statement>,
    pub orelse: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct With {
    pub span: Span,
    pub items: Vec<WithItem>,
    pub body: Vec<Statement>,
}

pub type AsyncWith = With;

#[derive(Debug, Clone)]
pub struct Raise {
    pub span: Span,
    pub exc: Option<Box<Expression>>,
    pub cause: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct Try {
    pub span: Span,
    pub body: Vec<Statement>,
    pub handlers: Vec<ExceptHandler>,
    pub orelse: Vec<Statement>,
    pub finalbody: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Assert {
    pub span: Span,
    pub test: Box<Expression>,
    pub msg: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub span: Span,
    pub names: Vec<Alias>,
}

#[derive(Debug, Clone)]
pub struct ImportFrom {
    pub span: Span,
    pub module: Option<String>,
    pub names: Vec<Alias>,
    pub level: usize,
}

#[derive(Debug, Clone)]
pub struct Global {
    pub span: Span,
    pub names: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Nonlocal {
    pub span: Span,
    pub names: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Pass {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Break {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Continue {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionDef(FunctionDef),
    AsyncFunctionDef(AsyncFunctionDef),
    ClassDef(ClassDef),
    Return(Return),
    Delete(Delete),
    Assign(Assign),
    AugAssign(AugAssign),
    AnnAssign(AnnAssign),
    For(For),
    AsyncFor(AsyncFor),
    While(While),
    If(If),
    With(With),
    AsyncWith(AsyncWith),
    Raise(Raise),
    Try(Try),
    Assert(Assert),
    Import(Import),
    ImportFrom(ImportFrom),
    Global(Global),
    Nonlocal(Nonlocal),
    Expr(Expr),
    Pass(Pass),
    Break(Break),
    Continue(Continue),
}

impl Statement {
    pub fn span(&self) -> &Span {
        match self {
            Statement::FunctionDef(x) => &x.span,
            Statement::AsyncFunctionDef(x) => &x.span,
            Statement::ClassDef(x) => &x.span,
            Statement::Return(x) => &x.span,
            Statement::Delete(x) => &x.span,
            Statement::Assign(x) => &x.span,
            Statement::AugAssign(x) => &x.span,
            Statement::AnnAssign(x) => &x.span,
            Statement::For(x) => &x.span,
            Statement::AsyncFor(x) => &x.span,
            Statement::While(x) => &x.span,
            Statement::If(x) => &x.span,
            Statement::With(x) => &x.span,
            Statement::AsyncWith(x) => &x.span,
            Statement::Raise(x) => &x.span,
            Statement::Try(x) => &x.span,
            Statement::Assert(x) => &x.span,
            Statement::Import(x) => &x.span,
            Statement::ImportFrom(x) => &x.span,
            Statement::Global(x) => &x.span,
            Statement::Nonlocal(x) => &x.span,
            Statement::Expr(x) => &x.span,
            Statement::Pass(x) => &x.span,
            Statement::Break(x) => &x.span,
            Statement::Continue(x) => &x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoolOp {
    pub span: Span,
    pub left: Box<Expression>,
    pub op: BoolOpKind,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub span: Span,
    pub left: Box<Expression>,
    pub op: OpKind,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub span: Span,
    pub op: UnaryOpKind,
    pub operand: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub span: Span,
    pub args: Box<Arguments>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct IfExp {
    pub span: Span,
    pub test: Box<Expression>,
    pub body: Box<Expression>,
    pub orelse: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Dict {
    pub span: Span,
    pub keys: Vec<Option<Expression>>,
    pub values: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub span: Span,
    pub elts: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct ListComp {
    pub span: Span,
    pub elt: Box<Expression>,
    pub generators: Vec<Comprehension>,
}

#[derive(Debug, Clone)]
pub struct SetComp {
    pub span: Span,
    pub elt: Box<Expression>,
    pub generators: Vec<Comprehension>,
}

#[derive(Debug, Clone)]
pub struct DictComp {
    pub span: Span,
    pub key: Box<Expression>,
    pub value: Box<Expression>,
    pub generators: Vec<Comprehension>,
}

#[derive(Debug, Clone)]
pub struct GeneratorExp {
    pub span: Span,
    pub elt: Box<Expression>,
    pub generators: Vec<Comprehension>,
}

#[derive(Debug, Clone)]
pub struct Await {
    pub span: Span,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Yield {
    pub span: Span,
    pub value: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct YieldFrom {
    pub span: Span,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Compare {
    pub span: Span,
    pub left: Box<Expression>,
    pub ops: Vec<ComparisonOpKind>,
    pub comparators: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub span: Span,
    pub func: Box<Expression>,
    pub args: Vec<Expression>,
    pub keywords: Vec<Keyword>,
}

#[derive(Debug, Clone)]
pub struct Num {
    pub span: Span,
    pub value: NumKind,
}

#[derive(Debug, Clone)]
pub struct Str {
    pub span: Span,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct FormattedValue {
    pub span: Span,
    pub value: Box<Expression>,
    pub conversion: Option<Conversion>,
    pub format_spec: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct JoinedStr {
    pub span: Span,
    pub values: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Bytes {
    pub span: Span,
    pub value: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct NameConstant {
    pub span: Span,
    pub value: Singleton,
}

#[derive(Debug, Clone)]
pub struct Ellipsis {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub span: Span,
    pub value: Box<Expression>,
    pub attr: String,
}

#[derive(Debug, Clone)]
pub struct Subscript {
    pub span: Span,
    pub value: Box<Expression>,
    pub slice: Slice,
}

#[derive(Debug, Clone)]
pub struct Starred {
    pub span: Span,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub span: Span,
    pub id: String,
}

#[derive(Debug, Clone)]
pub struct List {
    pub span: Span,
    pub elts: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Tuple {
    pub span: Span,
    pub elts: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    BoolOp(BoolOp),
    BinOp(BinOp),
    UnaryOp(UnaryOp),
    Lambda(Lambda),
    IfExp(IfExp),
    Dict(Dict),
    Set(Set),
    ListComp(ListComp),
    SetComp(SetComp),
    DictComp(DictComp),
    GeneratorExp(GeneratorExp),
    Await(Await),
    Yield(Yield),
    YieldFrom(YieldFrom),
    Compare(Compare),
    Call(Call),
    Num(Num),
    Str(Str),
    FormattedValue(FormattedValue),
    JoinedStr(JoinedStr),
    Bytes(Bytes),
    NameConstant(NameConstant),
    Ellipsis(Ellipsis),
    Attribute(Attribute),
    Subscript(Subscript),
    Starred(Starred),
    Name(Name),
    List(List),
    Tuple(Tuple),
}

impl Expression {
    pub fn span(&self) -> &Span {
        match self {
            Expression::BoolOp(x) => &x.span,
            Expression::BinOp(x) => &x.span,
            Expression::UnaryOp(x) => &x.span,
            Expression::Lambda(x) => &x.span,
            Expression::IfExp(x) => &x.span,
            Expression::Dict(x) => &x.span,
            Expression::Set(x) => &x.span,
            Expression::ListComp(x) => &x.span,
            Expression::SetComp(x) => &x.span,
            Expression::DictComp(x) => &x.span,
            Expression::GeneratorExp(x) => &x.span,
            Expression::Await(x) => &x.span,
            Expression::Yield(x) => &x.span,
            Expression::YieldFrom(x) => &x.span,
            Expression::Compare(x) => &x.span,
            Expression::Call(x) => &x.span,
            Expression::Num(x) => &x.span,
            Expression::Str(x) => &x.span,
            Expression::FormattedValue(x) => &x.span,
            Expression::JoinedStr(x) => &x.span,
            Expression::Bytes(x) => &x.span,
            Expression::NameConstant(x) => &x.span,
            Expression::Ellipsis(x) => &x.span,
            Expression::Attribute(x) => &x.span,
            Expression::Subscript(x) => &x.span,
            Expression::Starred(x) => &x.span,
            Expression::Name(x) => &x.span,
            Expression::List(x) => &x.span,
            Expression::Tuple(x) => &x.span,
        }
    }
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
pub enum BoolOpKind {
    And,
    Or,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum OpKind {
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
pub enum UnaryOpKind {
    Invert,
    Not,
    Plus,
    Minus,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ComparisonOpKind {
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
pub enum NumKind {
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

fn write_funcdef(
    f: &mut fmt::Formatter<'_>,
    indent: usize,
    node: &FunctionDef,
    is_async: bool,
) -> fmt::Result {
    for dec in &node.decorator_list {
        writeln!(f, "@{}", dec)?;
        write_indent(f, indent)?;
    }
    if is_async {
        f.write_str("async ")?;
    }
    write!(f, "def {}({})", node.name, node.args)?;
    if let Some(returns) = &node.returns {
        write!(f, " -> {}", returns)?;
    }
    f.write_str(":\n")?;
    write_body(f, indent + 1, &node.body)?;
    Ok(())
}

fn write_for(f: &mut fmt::Formatter<'_>, indent: usize, node: &For, is_async: bool) -> fmt::Result {
    if is_async {
        f.write_str("async ")?;
    }
    writeln!(f, "for {} in {}:", node.target, node.iter)?;
    write_body(f, indent + 1, &node.body)?;
    if !node.orelse.is_empty() {
        write_indent(f, indent)?;
        f.write_str("else:\n")?;
        write_body(f, indent + 1, &node.orelse)?;
    }
    Ok(())
}

fn write_if(f: &mut fmt::Formatter<'_>, indent: usize, node: &If) -> fmt::Result {
    writeln!(f, "if {}:", node.test)?;
    write_body(f, indent + 1, &node.body)?;
    match node.orelse.as_slice() {
        [Statement::If(s)] => {
            write_indent(f, indent)?;
            f.write_str("el")?;
            write_if(f, indent, s)?;
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
    node: &With,
    is_async: bool,
) -> fmt::Result {
    if is_async {
        f.write_str("async ")?;
    }
    f.write_str("with ")?;
    write_joined(f, &node.items, ", ")?;
    f.write_str(":\n")?;
    write_body(f, indent + 1, &node.body)
}

impl Statement {
    fn fmt_indented(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        match self {
            Statement::FunctionDef(node) => write_funcdef(f, indent, node, false),
            Statement::AsyncFunctionDef(node) => write_funcdef(f, indent, node, true),
            Statement::ClassDef(node) => {
                for dec in &node.decorator_list {
                    writeln!(f, "@{}", dec)?;
                    write_indent(f, indent)?;
                }
                write!(f, "class {}", node.name)?;
                if !node.bases.is_empty() || !node.keywords.is_empty() {
                    f.write_str("(")?;
                    write_joined(f, &node.bases, ", ")?;
                    if !node.bases.is_empty() && !node.keywords.is_empty() {
                        f.write_str(", ")?;
                    }
                    write_joined(f, &node.keywords, ", ")?;
                    f.write_str(")")?;
                }
                f.write_str(":\n")?;
                write_body(f, indent + 1, &node.body)
            }
            Statement::Return(Return { value, .. }) => match value {
                Some(v) => write!(f, "return {}", v),
                _ => write!(f, "return"),
            },
            Statement::Delete(Delete { targets, .. }) => {
                write!(f, "del ")?;
                write_joined(f, targets, ", ")
            }
            Statement::Assign(Assign { targets, value, .. }) => {
                write_joined(f, targets, " = ")?;
                write!(f, " = {}", value)
            }
            Statement::AugAssign(AugAssign {
                target, op, value, ..
            }) => write!(f, "{} {}= {}", target, op, value),
            Statement::AnnAssign(node) => {
                write!(f, "{}: {}", node.target, node.annotation)?;
                if let Some(value) = &node.value {
                    write!(f, " = {}", value)?;
                }
                Ok(())
            }
            Statement::For(node) => write_for(f, indent, node, false),
            Statement::AsyncFor(node) => write_for(f, indent, node, true),
            Statement::While(node) => {
                writeln!(f, "while {}:", node.test)?;
                write_body(f, indent + 1, &node.body)?;
                if !node.orelse.is_empty() {
                    write_indent(f, indent)?;
                    f.write_str("else:\n")?;
                    write_body(f, indent + 1, &node.orelse)?;
                }
                Ok(())
            }
            Statement::If(node) => write_if(f, indent, node),
            Statement::With(node) => write_with(f, indent, node, false),
            Statement::AsyncWith(node) => write_with(f, indent, node, true),
            Statement::Raise(Raise { exc, cause, .. }) => {
                write!(f, "raise")?;
                if let Some(exc) = exc {
                    write!(f, " {}", exc)?;
                }
                if let Some(cause) = cause {
                    write!(f, " from {}", cause)?;
                }
                Ok(())
            }
            Statement::Try(node) => {
                f.write_str("try:\n")?;
                write_body(f, indent + 1, &node.body)?;
                for hdl in &node.handlers {
                    hdl.fmt_indented(f, indent)?;
                }
                if !node.orelse.is_empty() {
                    write_indent(f, indent)?;
                    f.write_str("else:\n")?;
                    write_body(f, indent + 1, &node.orelse)?;
                }
                if !node.finalbody.is_empty() {
                    write_indent(f, indent)?;
                    f.write_str("finally:\n")?;
                    write_body(f, indent + 1, &node.finalbody)?;
                }
                Ok(())
            }
            Statement::Assert(Assert { test, msg, .. }) => {
                write!(f, "assert {}", test)?;
                if let Some(msg) = msg {
                    write!(f, ", {}", msg)?;
                }
                Ok(())
            }
            Statement::Import(Import { names, .. }) => {
                f.write_str("import ")?;
                write_joined(f, names, ", ")
            }
            Statement::ImportFrom(node) => {
                f.write_str("from ")?;
                for _ in 0..node.level {
                    f.write_str(".")?;
                }
                if let Some(module) = &node.module {
                    write!(f, "{}", module)?;
                }
                f.write_str(" import ")?;
                write_joined(f, &node.names, ", ")
            }
            Statement::Global(Global { names, .. }) => {
                f.write_str("global ")?;
                write_joined(f, names, ", ")
            }
            Statement::Nonlocal(Nonlocal { names, .. }) => {
                f.write_str("nonlocal ")?;
                write_joined(f, names, ", ")
            }
            Statement::Expr(Expr { value, .. }) => write!(f, "{}", value),
            Statement::Pass(_) => f.write_str("pass"),
            Statement::Break(_) => f.write_str("break"),
            Statement::Continue(_) => f.write_str("continue"),
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
        match self {
            Expression::BoolOp(node) => write!(f, "{} {} {}", node.left, node.op, node.right),
            Expression::BinOp(node) => write!(f, "{} {} {}", node.left, node.op, node.right),
            Expression::UnaryOp(UnaryOp { op, operand, .. }) => write!(f, "{} {}", op, operand),
            Expression::Lambda(Lambda { args, body, .. }) => write!(f, "lambda {}: {}", args, body),
            Expression::IfExp(node) => {
                write!(f, "{} if {} else {}", node.body, node.test, node.orelse)
            }
            Expression::Dict(Dict { keys, values, .. }) => {
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
            Expression::Set(Set { elts, .. }) => {
                f.write_str("{")?;
                write_joined(f, elts, ", ")?;
                f.write_str("}")
            }
            Expression::ListComp(node) => {
                write!(f, "[{} ", node.elt)?;
                write_joined(f, &node.generators, " ")?;
                f.write_str("]")
            }
            Expression::SetComp(node) => {
                write!(f, "{{{} ", node.elt)?;
                write_joined(f, &node.generators, " ")?;
                f.write_str("}")
            }
            Expression::DictComp(node) => {
                write!(f, "{{{}: {} ", node.key, node.value)?;
                write_joined(f, &node.generators, " ")?;
                f.write_str("}")
            }
            Expression::GeneratorExp(node) => {
                write!(f, "({} ", node.elt)?;
                write_joined(f, &node.generators, " ")?;
                f.write_str(")")
            }
            Expression::Await(Await { value, .. }) => write!(f, "await {}", value),
            Expression::Yield(Yield { value, .. }) => match value {
                Some(value) => write!(f, "yield {}", value),
                _ => write!(f, "yield"),
            },
            Expression::YieldFrom(YieldFrom { value, .. }) => write!(f, "yield from {}", value),
            Expression::Compare(node) => {
                write!(f, "{}", node.left)?;
                for (op, e) in node.ops.iter().zip(&node.comparators) {
                    write!(f, " {} {}", op, e)?
                }
                Ok(())
            }
            Expression::Call(node) => {
                write!(f, "{}(", node.func)?;
                write_joined(f, &node.args, ", ")?;
                if !node.args.is_empty() && !node.keywords.is_empty() {
                    f.write_str(", ")?;
                }
                write_joined(f, &node.keywords, ", ")?;
                f.write_str(")")
            }
            Expression::Num(Num { value, .. }) => write!(f, "{}", value),
            Expression::Str(Str { value, .. }) => {
                f.write_str("\"")?;
                for b in value.bytes() {
                    write_escaped(f, b)?;
                }
                f.write_str("\"")
            }
            Expression::FormattedValue(FormattedValue { .. }) => unimplemented!(),
            Expression::JoinedStr(JoinedStr { .. }) => unimplemented!(),
            Expression::Bytes(Bytes { value, .. }) => {
                f.write_str("b\"")?;
                for b in value {
                    write_escaped(f, *b)?;
                }
                f.write_str("\"")
            }
            Expression::NameConstant(NameConstant { value, .. }) => write!(f, "{}", value),
            Expression::Ellipsis(Ellipsis { .. }) => write!(f, "..."),
            Expression::Attribute(Attribute { value, attr, .. }) => write!(f, "{}.{}", value, attr),
            Expression::Subscript(Subscript { value, slice, .. }) => {
                write!(f, "{}[{}]", value, slice)
            }
            Expression::Starred(Starred { value, .. }) => write!(f, "*{}", value),
            Expression::Name(Name { id, .. }) => write!(f, "{}", id),
            Expression::List(List { elts, .. }) => {
                f.write_str("[")?;
                write_joined(f, elts, ", ")?;
                f.write_str("]")
            }
            Expression::Tuple(Tuple { elts, .. }) => {
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

impl Display for BoolOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BoolOpKind::And => "and",
            BoolOpKind::Or => "or",
        };
        f.write_str(s)
    }
}

impl Display for OpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            OpKind::Addition => "+",
            OpKind::Subtraction => "-",
            OpKind::Multiplication => "*",
            OpKind::MatrixMultiplication => "@",
            OpKind::Division => "/",
            OpKind::Modulo => "%",
            OpKind::Power => "**",
            OpKind::LeftShift => "<<",
            OpKind::RightShift => ">>",
            OpKind::BitOr => "|",
            OpKind::BitXor => "^",
            OpKind::BitAnd => "&",
            OpKind::FloorDivision => "//",
        };
        f.write_str(s)
    }
}

impl Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            UnaryOpKind::Invert => "~",
            UnaryOpKind::Not => "not",
            UnaryOpKind::Plus => "+",
            UnaryOpKind::Minus => "-",
        };
        f.write_str(s)
    }
}

impl Display for ComparisonOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            ComparisonOpKind::Equal => "==",
            ComparisonOpKind::NotEqual => "!=",
            ComparisonOpKind::Less => "<",
            ComparisonOpKind::LessEqual => "<=",
            ComparisonOpKind::Greater => ">",
            ComparisonOpKind::GreaterEqual => ">=",
            ComparisonOpKind::Is => "is",
            ComparisonOpKind::IsNot => "is not",
            ComparisonOpKind::In => "in",
            ComparisonOpKind::NotIn => "not in",
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

impl Display for NumKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumKind::Integer(n) => write!(f, "{}", n),
            NumKind::Float(n) | NumKind::Complex(n) => write!(f, "{}", n),
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
