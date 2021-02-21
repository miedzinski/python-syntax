use crate::ast;

pub trait Visitor {
    type T;

    fn visit_module(&mut self, node: &ast::Module) -> Self::T;

    fn visit_interactive(&mut self, node: &ast::Interactive) -> Self::T;

    fn visit_eval(&mut self, node: &ast::Eval) -> Self::T;

    fn visit_function_def(&mut self, node: &ast::FunctionDef) -> Self::T;

    fn visit_async_function_def(&mut self, node: &ast::AsyncFunctionDef) -> Self::T;

    fn visit_class_def(&mut self, node: &ast::ClassDef) -> Self::T;

    fn visit_return(&mut self, node: &ast::Return) -> Self::T;

    fn visit_delete(&mut self, node: &ast::Delete) -> Self::T;

    fn visit_assign(&mut self, node: &ast::Assign) -> Self::T;

    fn visit_aug_assign(&mut self, node: &ast::AugAssign) -> Self::T;

    fn visit_ann_assign(&mut self, node: &ast::AnnAssign) -> Self::T;

    fn visit_for(&mut self, node: &ast::For) -> Self::T;

    fn visit_async_for(&mut self, node: &ast::AsyncFor) -> Self::T;

    fn visit_while(&mut self, node: &ast::While) -> Self::T;

    fn visit_if(&mut self, node: &ast::If) -> Self::T;

    fn visit_with(&mut self, node: &ast::With) -> Self::T;

    fn visit_async_with(&mut self, node: &ast::AsyncWith) -> Self::T;

    fn visit_raise(&mut self, node: &ast::Raise) -> Self::T;

    fn visit_try(&mut self, node: &ast::Try) -> Self::T;

    fn visit_assert(&mut self, node: &ast::Assert) -> Self::T;

    fn visit_import(&mut self, node: &ast::Import) -> Self::T;

    fn visit_import_from(&mut self, node: &ast::ImportFrom) -> Self::T;

    fn visit_global(&mut self, node: &ast::Global) -> Self::T;

    fn visit_nonlocal(&mut self, node: &ast::Nonlocal) -> Self::T;

    fn visit_expr(&mut self, node: &ast::Expr) -> Self::T;

    fn visit_pass(&mut self, node: &ast::Pass) -> Self::T;

    fn visit_break(&mut self, node: &ast::Break) -> Self::T;

    fn visit_continue(&mut self, node: &ast::Continue) -> Self::T;

    fn visit_bool_op(&mut self, node: &ast::BoolOp) -> Self::T;

    fn visit_bin_op(&mut self, node: &ast::BinOp) -> Self::T;

    fn visit_unary_op(&mut self, node: &ast::UnaryOp) -> Self::T;

    fn visit_lambda(&mut self, node: &ast::Lambda) -> Self::T;

    fn visit_if_exp(&mut self, node: &ast::IfExp) -> Self::T;

    fn visit_dict(&mut self, node: &ast::Dict) -> Self::T;

    fn visit_set(&mut self, node: &ast::Set) -> Self::T;

    fn visit_list_comp(&mut self, node: &ast::ListComp) -> Self::T;

    fn visit_set_comp(&mut self, node: &ast::SetComp) -> Self::T;

    fn visit_dict_comp(&mut self, node: &ast::DictComp) -> Self::T;

    fn visit_generator_exp(&mut self, node: &ast::GeneratorExp) -> Self::T;

    fn visit_await(&mut self, node: &ast::Await) -> Self::T;

    fn visit_yield(&mut self, node: &ast::Yield) -> Self::T;

    fn visit_yield_from(&mut self, node: &ast::YieldFrom) -> Self::T;

    fn visit_compare(&mut self, node: &ast::Compare) -> Self::T;

    fn visit_call(&mut self, node: &ast::Call) -> Self::T;

    fn visit_num(&mut self, node: &ast::Num) -> Self::T;

    fn visit_str(&mut self, node: &ast::Str) -> Self::T;

    fn visit_formatted_value(&mut self, node: &ast::FormattedValue) -> Self::T;

    fn visit_joined_str(&mut self, node: &ast::JoinedStr) -> Self::T;

    fn visit_bytes(&mut self, node: &ast::Bytes) -> Self::T;

    fn visit_name_constant(&mut self, node: &ast::NameConstant) -> Self::T;

    fn visit_ellipsis(&mut self, node: &ast::Ellipsis) -> Self::T;

    fn visit_attribute(&mut self, node: &ast::Attribute) -> Self::T;

    fn visit_subscript(&mut self, node: &ast::Subscript) -> Self::T;

    fn visit_starred(&mut self, node: &ast::Starred) -> Self::T;

    fn visit_name(&mut self, node: &ast::Name) -> Self::T;

    fn visit_list(&mut self, node: &ast::List) -> Self::T;

    fn visit_tuple(&mut self, node: &ast::Tuple) -> Self::T;
}

pub fn walk_module<V: Visitor>(visitor: &mut V, node: &ast::Module) {
    for e in &node.body {
        e.accept(visitor);
    }
}

pub fn walk_interactive<V: Visitor>(visitor: &mut V, node: &ast::Interactive) {
    for e in &node.body {
        e.accept(visitor);
    }
}

pub fn walk_eval<V: Visitor>(visitor: &mut V, node: &ast::Eval) {
    node.body.accept(visitor);
}

pub fn walk_function_def<V: Visitor>(visitor: &mut V, node: &ast::FunctionDef) {
    walk_arguments(visitor, &node.args);
    for e in &node.body {
        e.accept(visitor);
    }
    for e in &node.decorator_list {
        e.accept(visitor);
    }
    if let Some(e) = &node.returns {
        e.accept(visitor);
    }
}

pub fn walk_async_function_def<V: Visitor>(visitor: &mut V, node: &ast::AsyncFunctionDef) {
    walk_function_def(visitor, node);
}

pub fn walk_class_def<V: Visitor>(visitor: &mut V, node: &ast::ClassDef) {
    for e in &node.bases {
        e.accept(visitor);
    }
    for e in &node.keywords {
        walk_keyword(visitor, e);
    }
    for e in &node.body {
        e.accept(visitor);
    }
    for e in &node.decorator_list {
        e.accept(visitor);
    }
}

pub fn walk_return<V: Visitor>(visitor: &mut V, node: &ast::Return) {
    if let Some(e) = &node.value {
        e.accept(visitor);
    }
}

pub fn walk_delete<V: Visitor>(visitor: &mut V, node: &ast::Delete) {
    for e in &node.targets {
        e.accept(visitor);
    }
}

pub fn walk_assign<V: Visitor>(visitor: &mut V, node: &ast::Assign) {
    for e in &node.targets {
        e.accept(visitor);
    }
    node.value.accept(visitor);
}

pub fn walk_aug_assign<V: Visitor>(visitor: &mut V, node: &ast::AugAssign) {
    node.target.accept(visitor);
    node.value.accept(visitor);
}

pub fn walk_ann_assign<V: Visitor>(visitor: &mut V, node: &ast::AnnAssign) {
    node.target.accept(visitor);
    node.annotation.accept(visitor);
    if let Some(e) = &node.value {
        e.accept(visitor);
    }
}

pub fn walk_for<V: Visitor>(visitor: &mut V, node: &ast::For) {
    node.target.accept(visitor);
    node.iter.accept(visitor);
    for e in &node.body {
        e.accept(visitor);
    }
    for e in &node.orelse {
        e.accept(visitor);
    }
}

pub fn walk_async_for<V: Visitor>(visitor: &mut V, node: &ast::AsyncFor) {
    walk_for(visitor, node);
}

pub fn walk_while<V: Visitor>(visitor: &mut V, node: &ast::While) {
    node.test.accept(visitor);
    for e in &node.body {
        e.accept(visitor);
    }
    for e in &node.orelse {
        e.accept(visitor);
    }
}

pub fn walk_if<V: Visitor>(visitor: &mut V, node: &ast::If) {
    node.test.accept(visitor);
    for e in &node.body {
        e.accept(visitor);
    }
    for e in &node.orelse {
        e.accept(visitor);
    }
}

pub fn walk_with<V: Visitor>(visitor: &mut V, node: &ast::With) {
    for e in &node.items {
        e.context_expr.accept(visitor);
        if let Some(e) = &e.optional_vars {
            e.accept(visitor);
        }
    }
    for e in &node.body {
        e.accept(visitor);
    }
}

pub fn walk_async_with<V: Visitor>(visitor: &mut V, node: &ast::AsyncWith) {
    walk_with(visitor, node);
}

pub fn walk_raise<V: Visitor>(visitor: &mut V, node: &ast::Raise) {
    if let Some(e) = &node.exc {
        e.accept(visitor);
    }
    if let Some(e) = &node.cause {
        e.accept(visitor);
    }
}

pub fn walk_try<V: Visitor>(visitor: &mut V, node: &ast::Try) {
    for e in &node.body {
        e.accept(visitor);
    }
    for hdl in &node.handlers {
        if let Some(e) = &hdl.typ {
            e.accept(visitor);
        }
        for e in &hdl.body {
            e.accept(visitor);
        }
    }
    for e in &node.orelse {
        e.accept(visitor);
    }
    for e in &node.finalbody {
        e.accept(visitor);
    }
}

pub fn walk_assert<V: Visitor>(visitor: &mut V, node: &ast::Assert) {
    node.test.accept(visitor);
    if let Some(e) = &node.msg {
        e.accept(visitor);
    }
}

pub fn walk_import<V: Visitor>(_visitor: &mut V, _node: &ast::Import) {}

pub fn walk_import_from<V: Visitor>(_visitor: &mut V, _node: &ast::ImportFrom) {}

pub fn walk_global<V: Visitor>(_visitor: &mut V, _node: &ast::Global) {}

pub fn walk_nonlocal<V: Visitor>(_visitor: &mut V, _node: &ast::Nonlocal) {}

pub fn walk_expr<V: Visitor>(visitor: &mut V, node: &ast::Expr) {
    node.value.accept(visitor);
}

pub fn walk_pass<V: Visitor>(_visitor: &mut V, _node: &ast::Pass) {}

pub fn walk_break<V: Visitor>(_visitor: &mut V, _node: &ast::Break) {}

pub fn walk_continue<V: Visitor>(_visitor: &mut V, _node: &ast::Continue) {}

pub fn walk_bool_op<V: Visitor>(visitor: &mut V, node: &ast::BoolOp) {
    node.left.accept(visitor);
    node.right.accept(visitor);
}

pub fn walk_bin_op<V: Visitor>(visitor: &mut V, node: &ast::BinOp) {
    node.left.accept(visitor);
    node.right.accept(visitor);
}

pub fn walk_unary_op<V: Visitor>(visitor: &mut V, node: &ast::UnaryOp) {
    node.operand.accept(visitor);
}

pub fn walk_lambda<V: Visitor>(visitor: &mut V, node: &ast::Lambda) {
    walk_arguments(visitor, &node.args);
    node.body.accept(visitor);
}

pub fn walk_if_exp<V: Visitor>(visitor: &mut V, node: &ast::IfExp) {
    node.test.accept(visitor);
    node.body.accept(visitor);
    node.orelse.accept(visitor);
}

pub fn walk_dict<V: Visitor>(visitor: &mut V, node: &ast::Dict) {
    for e in &node.keys {
        if let Some(e) = e {
            e.accept(visitor);
        }
    }
    for e in &node.values {
        e.accept(visitor);
    }
}

pub fn walk_set<V: Visitor>(visitor: &mut V, node: &ast::Set) {
    for e in &node.elts {
        e.accept(visitor);
    }
}

pub fn walk_list_comp<V: Visitor>(visitor: &mut V, node: &ast::ListComp) {
    node.elt.accept(visitor);
    for e in &node.generators {
        walk_comprehension(visitor, e);
    }
}

pub fn walk_set_comp<V: Visitor>(visitor: &mut V, node: &ast::SetComp) {
    node.elt.accept(visitor);
    for e in &node.generators {
        walk_comprehension(visitor, e);
    }
}

pub fn walk_dict_comp<V: Visitor>(visitor: &mut V, node: &ast::DictComp) {
    node.key.accept(visitor);
    node.value.accept(visitor);
    for e in &node.generators {
        walk_comprehension(visitor, e);
    }
}

pub fn walk_generator_exp<V: Visitor>(visitor: &mut V, node: &ast::GeneratorExp) {
    node.elt.accept(visitor);
    for e in &node.generators {
        walk_comprehension(visitor, e)
    }
}

pub fn walk_await<V: Visitor>(visitor: &mut V, node: &ast::Await) {
    node.value.accept(visitor);
}

pub fn walk_yield<V: Visitor>(visitor: &mut V, node: &ast::Yield) {
    if let Some(e) = &node.value {
        e.accept(visitor);
    }
}

pub fn walk_yield_from<V: Visitor>(visitor: &mut V, node: &ast::YieldFrom) {
    node.value.accept(visitor);
}

pub fn walk_compare<V: Visitor>(visitor: &mut V, node: &ast::Compare) {
    node.left.accept(visitor);
    for e in &node.comparators {
        e.accept(visitor);
    }
}

pub fn walk_call<V: Visitor>(visitor: &mut V, node: &ast::Call) {
    node.func.accept(visitor);
    for e in &node.args {
        e.accept(visitor);
    }
    for e in &node.keywords {
        walk_keyword(visitor, e);
    }
}

pub fn walk_num<V: Visitor>(_visitor: &mut V, _node: &ast::Num) {}

pub fn walk_str<V: Visitor>(_visitor: &mut V, _node: &ast::Str) {}

pub fn walk_formatted_value<V: Visitor>(visitor: &mut V, node: &ast::FormattedValue) {
    node.value.accept(visitor);
    node.format_spec.accept(visitor);
}

pub fn walk_joined_str<V: Visitor>(visitor: &mut V, node: &ast::JoinedStr) {
    for e in &node.values {
        e.accept(visitor);
    }
}

pub fn walk_bytes<V: Visitor>(_visitor: &mut V, _node: &ast::Bytes) {}

pub fn walk_name_constant<V: Visitor>(_visitor: &mut V, _node: &ast::NameConstant) {}

pub fn walk_ellipsis<V: Visitor>(_visitor: &mut V, _node: &ast::Ellipsis) {}

pub fn walk_attribute<V: Visitor>(visitor: &mut V, node: &ast::Attribute) {
    node.value.accept(visitor);
}

pub fn walk_subscript<V: Visitor>(visitor: &mut V, node: &ast::Subscript) {
    node.value.accept(visitor);
}

pub fn walk_starred<V: Visitor>(visitor: &mut V, node: &ast::Starred) {
    node.value.accept(visitor);
}

pub fn walk_name<V: Visitor>(_visitor: &mut V, _node: &ast::Name) {}

pub fn walk_list<V: Visitor>(visitor: &mut V, node: &ast::List) {
    for e in &node.elts {
        e.accept(visitor);
    }
}

pub fn walk_tuple<V: Visitor>(visitor: &mut V, node: &ast::Tuple) {
    for e in &node.elts {
        e.accept(visitor);
    }
}

pub fn walk_arguments<V: Visitor>(visitor: &mut V, node: &ast::Arguments) {
    node.args
        .iter()
        .chain(&node.vararg)
        .chain(&node.kwonlyargs)
        .chain(&node.kwarg)
        .for_each(|arg| walk_arg(visitor, &arg));
}

pub fn walk_arg<V: Visitor>(visitor: &mut V, node: &ast::Arg) {
    if let Some(e) = &node.annotation {
        e.accept(visitor);
    }
    if let ast::ArgKind::Optional(e) = &node.kind {
        e.accept(visitor);
    }
}

pub fn walk_keyword<V: Visitor>(visitor: &mut V, node: &ast::Keyword) {
    node.value.accept(visitor);
}

pub fn walk_comprehension<V: Visitor>(visitor: &mut V, node: &ast::Comprehension) {
    node.target.accept(visitor);
    node.iter.accept(visitor);
    for e in &node.ifs {
        e.accept(visitor);
    }
}

pub trait Accept {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::T;
}

impl Accept for ast::Program {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::T {
        match self {
            ast::Program::Module(x) => visitor.visit_module(x),
            ast::Program::Interactive(x) => visitor.visit_interactive(x),
            ast::Program::Eval(x) => visitor.visit_eval(x),
        }
    }
}

impl Accept for ast::Statement {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::T {
        match self {
            ast::Statement::FunctionDef(x) => visitor.visit_function_def(x),
            ast::Statement::AsyncFunctionDef(x) => visitor.visit_async_function_def(x),
            ast::Statement::ClassDef(x) => visitor.visit_class_def(x),
            ast::Statement::Return(x) => visitor.visit_return(x),
            ast::Statement::Delete(x) => visitor.visit_delete(x),
            ast::Statement::Assign(x) => visitor.visit_assign(x),
            ast::Statement::AugAssign(x) => visitor.visit_aug_assign(x),
            ast::Statement::AnnAssign(x) => visitor.visit_ann_assign(x),
            ast::Statement::For(x) => visitor.visit_for(x),
            ast::Statement::AsyncFor(x) => visitor.visit_async_for(x),
            ast::Statement::While(x) => visitor.visit_while(x),
            ast::Statement::If(x) => visitor.visit_if(x),
            ast::Statement::With(x) => visitor.visit_with(x),
            ast::Statement::AsyncWith(x) => visitor.visit_async_with(x),
            ast::Statement::Raise(x) => visitor.visit_raise(x),
            ast::Statement::Try(x) => visitor.visit_try(x),
            ast::Statement::Assert(x) => visitor.visit_assert(x),
            ast::Statement::Import(x) => visitor.visit_import(x),
            ast::Statement::ImportFrom(x) => visitor.visit_import_from(x),
            ast::Statement::Global(x) => visitor.visit_global(x),
            ast::Statement::Nonlocal(x) => visitor.visit_nonlocal(x),
            ast::Statement::Expr(x) => visitor.visit_expr(x),
            ast::Statement::Pass(x) => visitor.visit_pass(x),
            ast::Statement::Break(x) => visitor.visit_break(x),
            ast::Statement::Continue(x) => visitor.visit_continue(x),
        }
    }
}

impl Accept for ast::Expression {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::T {
        match self {
            ast::Expression::BoolOp(x) => visitor.visit_bool_op(x),
            ast::Expression::BinOp(x) => visitor.visit_bin_op(x),
            ast::Expression::UnaryOp(x) => visitor.visit_unary_op(x),
            ast::Expression::Lambda(x) => visitor.visit_lambda(x),
            ast::Expression::IfExp(x) => visitor.visit_if_exp(x),
            ast::Expression::Dict(x) => visitor.visit_dict(x),
            ast::Expression::Set(x) => visitor.visit_set(x),
            ast::Expression::ListComp(x) => visitor.visit_list_comp(x),
            ast::Expression::SetComp(x) => visitor.visit_set_comp(x),
            ast::Expression::DictComp(x) => visitor.visit_dict_comp(x),
            ast::Expression::GeneratorExp(x) => visitor.visit_generator_exp(x),
            ast::Expression::Await(x) => visitor.visit_await(x),
            ast::Expression::Yield(x) => visitor.visit_yield(x),
            ast::Expression::YieldFrom(x) => visitor.visit_yield_from(x),
            ast::Expression::Compare(x) => visitor.visit_compare(x),
            ast::Expression::Call(x) => visitor.visit_call(x),
            ast::Expression::Num(x) => visitor.visit_num(x),
            ast::Expression::Str(x) => visitor.visit_str(x),
            ast::Expression::FormattedValue(x) => visitor.visit_formatted_value(x),
            ast::Expression::JoinedStr(x) => visitor.visit_joined_str(x),
            ast::Expression::Bytes(x) => visitor.visit_bytes(x),
            ast::Expression::NameConstant(x) => visitor.visit_name_constant(x),
            ast::Expression::Ellipsis(x) => visitor.visit_ellipsis(x),
            ast::Expression::Attribute(x) => visitor.visit_attribute(x),
            ast::Expression::Subscript(x) => visitor.visit_subscript(x),
            ast::Expression::Starred(x) => visitor.visit_starred(x),
            ast::Expression::Name(x) => visitor.visit_name(x),
            ast::Expression::List(x) => visitor.visit_list(x),
            ast::Expression::Tuple(x) => visitor.visit_tuple(x),
        }
    }
}
