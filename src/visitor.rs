use crate::ast;

#[allow(unused_variables)]
trait Visitor: Sized {
    fn visit_module(&mut self, node: &ast::Module) {
        for e in &node.body {
            e.accept(self)
        }
    }

    fn visit_interactive(&mut self, node: &ast::Interactive) {
        for e in &node.body {
            e.accept(self)
        }
    }

    fn visit_eval(&mut self, node: &ast::Eval) {
        node.body.accept(self)
    }

    fn visit_function_def(&mut self, node: &ast::FunctionDef) {
        walk_arguments(self, &node.args);
        for e in &node.body {
            e.accept(self)
        }
        for e in &node.decorator_list {
            e.accept(self)
        }
        if let Some(e) = &node.returns {
            e.accept(self)
        }
    }

    fn visit_async_function_def(&mut self, node: &ast::AsyncFunctionDef) {
        self.visit_function_def(node)
    }

    fn visit_class_def(&mut self, node: &ast::ClassDef) {
        for e in &node.bases {
            e.accept(self)
        }
        for e in &node.keywords {
            walk_keyword(self, e)
        }
        for e in &node.body {
            e.accept(self)
        }
        for e in &node.decorator_list {
            e.accept(self)
        }
    }

    fn visit_return(&mut self, node: &ast::Return) {
        if let Some(e) = &node.value {
            e.accept(self)
        }
    }

    fn visit_delete(&mut self, node: &ast::Delete) {
        for e in &node.targets {
            e.accept(self)
        }
    }

    fn visit_assign(&mut self, node: &ast::Assign) {
        for e in &node.targets {
            e.accept(self)
        }
        node.value.accept(self)
    }

    fn visit_aug_assign(&mut self, node: &ast::AugAssign) {
        node.target.accept(self);
        node.value.accept(self)
    }

    fn visit_ann_assign(&mut self, node: &ast::AnnAssign) {
        node.target.accept(self);
        node.annotation.accept(self);
        if let Some(e) = &node.value {
            e.accept(self)
        }
    }

    fn visit_for(&mut self, node: &ast::For) {
        node.target.accept(self);
        node.iter.accept(self);
        for e in &node.body {
            e.accept(self)
        }
        for e in &node.orelse {
            e.accept(self)
        }
    }

    fn visit_async_for(&mut self, node: &ast::AsyncFor) {
        self.visit_for(node)
    }

    fn visit_while(&mut self, node: &ast::While) {
        node.test.accept(self);
        for e in &node.body {
            e.accept(self)
        }
        for e in &node.orelse {
            e.accept(self)
        }
    }

    fn visit_if(&mut self, node: &ast::If) {
        node.test.accept(self);
        for e in &node.body {
            e.accept(self)
        }
        for e in &node.orelse {
            e.accept(self)
        }
    }

    fn visit_with(&mut self, node: &ast::With) {
        for e in &node.items {
            e.context_expr.accept(self);
            if let Some(e) = &e.optional_vars {
                e.accept(self)
            }
        }
        for e in &node.body {
            e.accept(self)
        }
    }

    fn visit_async_with(&mut self, node: &ast::AsyncWith) {
        self.visit_with(node)
    }

    fn visit_raise(&mut self, node: &ast::Raise) {
        if let Some(e) = &node.exc {
            e.accept(self)
        }
        if let Some(e) = &node.cause {
            e.accept(self)
        }
    }

    fn visit_try(&mut self, node: &ast::Try) {
        for e in &node.body {
            e.accept(self)
        }
        for hdl in &node.handlers {
            if let Some(e) = &hdl.typ {
                e.accept(self)
            }
            for e in &hdl.body {
                e.accept(self)
            }
        }
        for e in &node.orelse {
            e.accept(self)
        }
        for e in &node.finalbody {
            e.accept(self)
        }
    }

    fn visit_assert(&mut self, node: &ast::Assert) {
        node.test.accept(self);
        if let Some(e) = &node.msg {
            e.accept(self)
        }
    }

    fn visit_import(&mut self, node: &ast::Import) {}

    fn visit_import_from(&mut self, node: &ast::ImportFrom) {}

    fn visit_global(&mut self, node: &ast::Global) {}

    fn visit_nonlocal(&mut self, node: &ast::Nonlocal) {}

    fn visit_expr(&mut self, node: &ast::Expr) {
        node.value.accept(self)
    }

    fn visit_pass(&mut self, node: &ast::Pass) {}

    fn visit_break(&mut self, node: &ast::Break) {}

    fn visit_continue(&mut self, node: &ast::Continue) {}

    fn visit_bool_op(&mut self, node: &ast::BoolOp) {
        node.left.accept(self);
        node.right.accept(self);
    }

    fn visit_bin_op(&mut self, node: &ast::BinOp) {
        node.left.accept(self);
        node.right.accept(self);
    }

    fn visit_unary_op(&mut self, node: &ast::UnaryOp) {
        node.operand.accept(self);
    }

    fn visit_lambda(&mut self, node: &ast::Lambda) {
        walk_arguments(self, &node.args);
        node.body.accept(self);
    }

    fn visit_if_exp(&mut self, node: &ast::IfExp) {
        node.test.accept(self);
        node.body.accept(self);
        node.orelse.accept(self);
    }

    fn visit_dict(&mut self, node: &ast::Dict) {
        for e in &node.keys {
            if let Some(e) = e {
                e.accept(self)
            }
        }
        for e in &node.values {
            e.accept(self)
        }
    }

    fn visit_set(&mut self, node: &ast::Set) {
        for e in &node.elts {
            e.accept(self)
        }
    }

    fn visit_list_comp(&mut self, node: &ast::ListComp) {
        node.elt.accept(self);
        for e in &node.generators {
            walk_comprehension(self, e)
        }
    }

    fn visit_set_comp(&mut self, node: &ast::SetComp) {
        node.elt.accept(self);
        for e in &node.generators {
            walk_comprehension(self, e)
        }
    }

    fn visit_dict_comp(&mut self, node: &ast::DictComp) {
        node.key.accept(self);
        node.value.accept(self);
        for e in &node.generators {
            walk_comprehension(self, e)
        }
    }

    fn visit_generator_exp(&mut self, node: &ast::GeneratorExp) {
        node.elt.accept(self);
        for e in &node.generators {
            walk_comprehension(self, e)
        }
    }

    fn visit_await(&mut self, node: &ast::Await) {
        node.value.accept(self);
    }

    fn visit_yield(&mut self, node: &ast::Yield) {
        if let Some(e) = &node.value {
            e.accept(self)
        }
    }

    fn visit_yield_from(&mut self, node: &ast::YieldFrom) {
        node.value.accept(self);
    }

    fn visit_compare(&mut self, node: &ast::Compare) {
        node.left.accept(self);
        for e in &node.comparators {
            e.accept(self)
        }
    }

    fn visit_call(&mut self, node: &ast::Call) {
        node.func.accept(self);
        for e in &node.args {
            e.accept(self)
        }
        for e in &node.keywords {
            walk_keyword(self, e)
        }
    }

    fn visit_num(&mut self, node: &ast::Num) {}

    fn visit_str(&mut self, node: &ast::Str) {}

    fn visit_formatted_value(&mut self, node: &ast::FormattedValue) {
        node.value.accept(self);
        node.format_spec.accept(self);
    }

    fn visit_joined_str(&mut self, node: &ast::JoinedStr) {
        for e in &node.values {
            e.accept(self)
        }
    }

    fn visit_bytes(&mut self, node: &ast::Bytes) {}

    fn visit_name_constant(&mut self, node: &ast::NameConstant) {}

    fn visit_ellipsis(&mut self, node: &ast::Ellipsis) {}

    fn visit_attribute(&mut self, node: &ast::Attribute) {
        node.value.accept(self);
    }

    fn visit_subscript(&mut self, node: &ast::Subscript) {
        node.value.accept(self);
    }

    fn visit_starred(&mut self, node: &ast::Starred) {
        node.value.accept(self);
    }

    fn visit_name(&mut self, node: &ast::Name) {}

    fn visit_list(&mut self, node: &ast::List) {
        for e in &node.elts {
            e.accept(self)
        }
    }

    fn visit_tuple(&mut self, node: &ast::Tuple) {
        for e in &node.elts {
            e.accept(self)
        }
    }
}

fn walk_arguments<V: Visitor>(visitor: &mut V, node: &ast::Arguments) {
    node.args
        .iter()
        .chain(&node.vararg)
        .chain(&node.kwonlyargs)
        .chain(&node.kwarg)
        .for_each(|arg| walk_arg(visitor, &arg))
}

fn walk_arg<V: Visitor>(visitor: &mut V, node: &ast::Arg) {
    if let Some(e) = &node.annotation {
        e.accept(visitor)
    }
    if let ast::ArgKind::Optional(e) = &node.kind {
        e.accept(visitor)
    }
}

fn walk_keyword<V: Visitor>(visitor: &mut V, node: &ast::Keyword) {
    node.value.accept(visitor)
}

fn walk_comprehension<V: Visitor>(visitor: &mut V, node: &ast::Comprehension) {
    node.target.accept(visitor);
    node.iter.accept(visitor);
    for e in &node.ifs {
        e.accept(visitor)
    }
}

trait Visitable {
    fn accept<V: Visitor>(&self, visitor: &mut V);
}

impl Visitable for ast::Program {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        match self {
            ast::Program::Module(x) => visitor.visit_module(x),
            ast::Program::Interactive(x) => visitor.visit_interactive(x),
            ast::Program::Eval(x) => visitor.visit_eval(x),
        }
    }
}

impl Visitable for ast::Statement {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
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

impl Visitable for ast::Expression {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
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
