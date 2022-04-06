mod engine;

#[derive(Default)]
struct Compiler {
    ctx: engine::Context,
}

#[derive(Debug)]
enum UntypedExpr {
    Annotate(Box<UntypedExpr>, engine::UnresolvedType),
    Instance(engine::UnresolvedType),
    Call(Box<UntypedExpr>, Box<UntypedExpr>),
}

#[derive(Debug)]
struct UnresolvedExpr {
    ty: engine::UnresolvedType,
    kind: UnresolvedExprKind,
}

#[derive(Debug)]
enum UnresolvedExprKind {
    Instance,
    Call(Box<UnresolvedExpr>, Box<UnresolvedExpr>),
    Imp(engine::Imp),
}

impl UnresolvedExpr {
    fn merge(&mut self, ty: engine::ResolvedType) {
        if let Some(imp) = ty.imp {
            self.kind = UnresolvedExprKind::Imp(imp);
        }

        self.ty = ty.into_unresolved();
    }
}

impl Compiler {
    fn typecheck(&mut self, expr: UntypedExpr) -> Result<UnresolvedExpr, String> {
        match expr {
            UntypedExpr::Annotate(expr, ty) => {
                let mut expr = self.typecheck(*expr)?;
                expr.merge(self.ctx.unify(expr.ty.clone(), ty)?);
                Ok(expr)
            }
            UntypedExpr::Instance(ty) => Ok(UnresolvedExpr {
                ty,
                kind: UnresolvedExprKind::Instance,
            }),
            UntypedExpr::Call(func, arg) => {
                let mut func = self.typecheck(*func)?;
                let mut arg = self.typecheck(*arg)?;

                let output_ty = engine::UnresolvedType::Var(self.ctx.new_var());

                let resolved_func_ty = self.ctx.unify(
                    func.ty.clone(),
                    engine::UnresolvedType::Func(
                        Box::new(arg.ty.clone()),
                        Box::new(output_ty.clone()),
                    ),
                )?;
                func.merge(resolved_func_ty.clone());

                let resolved_arg_ty = match resolved_func_ty.kind {
                    engine::ResolvedTypeKind::Func(arg, _) => *arg,
                    _ => unreachable!(),
                };
                arg.merge(resolved_arg_ty);

                Ok(UnresolvedExpr {
                    ty: output_ty,
                    kind: UnresolvedExprKind::Call(Box::new(func), Box::new(arg)),
                })
            }
        }
    }
}

fn main() {
    let mut compiler = Compiler::default();

    compiler.ctx.add_impl(
        "Show",
        engine::Impl::new(
            engine::UnresolvedType::Func(
                Box::new(engine::UnresolvedType::Named("Number")),
                Box::new(engine::UnresolvedType::Named("Text")),
            ),
            "Number -> Text",
        ),
    );

    let show = UntypedExpr::Instance(engine::UnresolvedType::Trait("Show"));

    let call = UntypedExpr::Call(
        Box::new(show),
        Box::new(UntypedExpr::Instance(engine::UnresolvedType::Named(
            "Number",
        ))),
    );

    match compiler.typecheck(call) {
        Ok(mut expr) => {
            expr.ty.apply(&compiler.ctx);
            eprintln!("{:?} :: {:?}", expr.kind, expr.ty)
        }
        Err(error) => eprintln!("error: {}", error),
    }
}
