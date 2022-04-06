use std::collections::HashMap;

pub type TypeVar = usize;
pub type TypeParam = &'static str;
pub type TypeName = &'static str;
pub type TypeTrait = &'static str;

pub type Imp = &'static str;

#[derive(Debug, Clone)]
pub enum UnresolvedType {
    Var(TypeVar),
    Param(TypeParam),
    Named(TypeName),
    Func(Box<UnresolvedType>, Box<UnresolvedType>),
    Trait(TypeTrait),
}

#[derive(Debug, Clone)]
pub struct ResolvedType {
    pub imp: Option<Imp>,
    pub kind: ResolvedTypeKind,
}

#[derive(Debug, Clone)]
pub enum ResolvedTypeKind {
    Var(TypeVar),
    Param(TypeParam),
    Named(TypeName),
    Func(Box<ResolvedType>, Box<ResolvedType>),
}

#[derive(Debug, Clone)]
pub enum Type {
    Var(TypeVar),
    Param(TypeParam),
    Named(TypeName),
    Func(Box<Type>, Box<Type>),
}

impl ResolvedType {
    pub fn into_unresolved(self) -> UnresolvedType {
        match self.kind {
            ResolvedTypeKind::Var(var) => UnresolvedType::Var(var),
            ResolvedTypeKind::Param(param) => UnresolvedType::Param(param),
            ResolvedTypeKind::Named(name) => UnresolvedType::Named(name),
            ResolvedTypeKind::Func(input, output) => UnresolvedType::Func(
                Box::new(input.into_unresolved()),
                Box::new(output.into_unresolved()),
            ),
        }
    }

    pub fn into_type(self) -> Type {
        match self.kind {
            ResolvedTypeKind::Var(var) => Type::Var(var),
            ResolvedTypeKind::Param(param) => Type::Param(param),
            ResolvedTypeKind::Named(name) => Type::Named(name),
            ResolvedTypeKind::Func(input, output) => {
                Type::Func(Box::new(input.into_type()), Box::new(output.into_type()))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub ty: UnresolvedType,
    pub constant: Imp,
}

impl Impl {
    pub fn new(ty: UnresolvedType, constant: Imp) -> Self {
        Impl { ty, constant }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Context {
    next_var: TypeVar,
    substitutions: HashMap<TypeVar, UnresolvedType>,
    impls: HashMap<TypeTrait, Vec<Impl>>,
}

impl Context {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_var(&mut self) -> TypeVar {
        let var = self.next_var;
        self.next_var += 1;
        var
    }

    pub fn add_impl(&mut self, tr: TypeTrait, imp: Impl) {
        self.impls.entry(tr).or_default().push(imp);
    }

    pub fn unify(
        &mut self,
        mut actual: UnresolvedType,
        mut expected: UnresolvedType,
    ) -> Result<ResolvedType, String> {
        actual.apply(self);
        expected.apply(self);

        match (actual, expected) {
            (UnresolvedType::Var(var), ty) | (ty, UnresolvedType::Var(var)) => {
                if ty.contains(var) {
                    Err(String::from("recursive type"))
                } else {
                    self.substitutions.insert(var, ty);

                    Ok(ResolvedType {
                        imp: None,
                        kind: ResolvedTypeKind::Var(var),
                    })
                }
            }
            (UnresolvedType::Param(param), _) => Ok(ResolvedType {
                imp: None,
                kind: ResolvedTypeKind::Param(param),
            }),
            (actual, expected @ UnresolvedType::Param(_)) => {
                Err(format!("mismatched types {:?} and {:?}", actual, expected))
            }
            (UnresolvedType::Named(actual), UnresolvedType::Named(expected)) => {
                if actual == expected {
                    Ok(ResolvedType {
                        imp: None,
                        kind: ResolvedTypeKind::Named(actual),
                    })
                } else {
                    Err(format!(
                        "mismatched types {:?} and {:?}",
                        UnresolvedType::Named(actual),
                        UnresolvedType::Named(expected)
                    ))
                }
            }
            (
                UnresolvedType::Func(actual_input, actual_output),
                UnresolvedType::Func(expected_input, expected_output),
            ) => {
                let input = self.unify(*actual_input, *expected_input)?;
                let output = self.unify(*actual_output, *expected_output)?;

                Ok(ResolvedType {
                    imp: None,
                    kind: ResolvedTypeKind::Func(Box::new(input), Box::new(output)),
                })
            }
            (UnresolvedType::Trait(tr), ty) | (ty, UnresolvedType::Trait(tr)) => self
                .impls
                .get(tr)
                .cloned()
                .and_then(|impls| {
                    impls.iter().find_map(|imp| {
                        let mut ctx = self.clone();

                        match ctx.unify(ty.clone(), imp.ty.clone()) {
                            Ok(ResolvedType { kind, .. }) => {
                                *self = ctx;
                                Some(ResolvedType {
                                    imp: Some(imp.constant),
                                    kind,
                                })
                            }
                            Err(_) => None,
                        }
                    })
                })
                .ok_or_else(|| {
                    format!(
                        "could not find implementation for trait {:?} matching type {:?}",
                        tr, ty
                    )
                }),
            (actual, expected) => Err(format!("mismatched types {:?} and {:?}", actual, expected)),
        }
    }
}

impl UnresolvedType {
    pub fn apply(&mut self, ctx: &Context) {
        match self {
            UnresolvedType::Var(var) => {
                if let Some(ty) = ctx.substitutions.get(var) {
                    *self = ty.clone();
                    self.apply(ctx)
                }
            }
            UnresolvedType::Func(input, output) => {
                input.apply(ctx);
                output.apply(ctx);
            }
            _ => {}
        }
    }

    pub fn contains(&self, var: TypeVar) -> bool {
        match self {
            UnresolvedType::Var(v) => *v == var,
            UnresolvedType::Func(input, output) => input.contains(var) || output.contains(var),
            _ => false,
        }
    }
}
