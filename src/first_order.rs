// In this tutorial we'll contrast two ways of defining an embedded DSL in Rust.
// Embedded DSLs sound complicated, but quite a few problems can be re-cast as
// writing an embedded DSL. I often find it useful to think of API design as
// embedded DSL design - are you coming up with an API for dataframe manipulation,
// plotting, or some other domain-specific task? Then you can think about the API
// as an embeddded DSL too, and the implementation of the API becomes the interpreter
// (or compiler) of the DSL.

// What makes a DSL "embedded" is simply that it's embedded in a "general purporse" host language
// like Rust. This is both convenient and limiting. It's convenient because you can use the
// host language's features to implement the DSL, and limiting because most host languages have
// some limitations on what is expressible, e.g. one often wants to overload literals.

// Consider the "canonical" way of defining a simple expression language as an embedded DSL.
// For reasons that will become clear later, it's just slightly more
// expressive (with negation) than Hutton's razor.

// We usually start out with defining the abstract syntax tree:

use std::marker::PhantomData;

#[derive(Debug, Clone)]
enum Expr {
    Lit(i32),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
}

// We can define some helpful constructors:
impl Expr {
    fn lit(i: i32) -> Expr {
        Expr::Lit(i)
    }

    fn neg(r: Expr) -> Expr {
        Expr::Neg(Box::new(r))
    }

    fn add(r1: Expr, r2: Expr) -> Expr {
        Expr::Add(Box::new(r1), Box::new(r2))
    }

    // And now we can evaluate expressions in the language:
    fn eval(&self) -> i32 {
        match self {
            Expr::Lit(i) => *i,
            Expr::Neg(r) => -r.eval(),
            Expr::Add(r1, r2) => r1.eval() + r2.eval(),
        }
    }

    // We can also pretty-print expressions:
    fn view(&self) -> String {
        match self {
            Expr::Lit(i) => i.to_string(),
            Expr::Neg(r) => format!("(-{})", r.view()),
            Expr::Add(r1, r2) => format!("({} + {})", r1.view(), r2.view()),
        }
    }

    // and do a bunch of other things.
}

// Note - this relies heavily on pattern matching. Oleg calls this the "initial" style,
// in contrast to the "final" style we'll define now.

// As a first approximation, let's suppose we only want to evaluate our language. Then
// we can skip the enum entirely and instead define our language directly as functions
// in the host language:

type Repr = i32;

fn lit(i: i32) -> Repr {
    i
}

fn neg(r: Repr) -> Repr {
    -r
}

fn add(r1: Repr, r2: Repr) -> Repr {
    r1 + r2
}

// That's not flexible enough - we want to re-interpret in different ways,
// e.g. eval and view. We'd like to essentially overload these functions -
// exactly what traits are for:
trait ExprSym {
    type Repr;

    fn lit(i: i32) -> Self::Repr;
    fn neg(r: Self::Repr) -> Self::Repr;
    fn add(r1: Self::Repr, r2: Self::Repr) -> Self::Repr;
}

// The trait definition of the syntax is isomorphic to the enum definition.
// Now we can implement the trait for different representations. First, eval:
struct Eval;
impl ExprSym for Eval {
    type Repr = i32;

    fn lit(i: i32) -> Self::Repr {
        i
    }

    fn neg(r: Self::Repr) -> Self::Repr {
        -r
    }

    fn add(r1: Self::Repr, r2: Self::Repr) -> Self::Repr {
        r1 + r2
    }
}
// trick to make rust infer the type, without explicit type arguments.
// Rust can't infer the type of the trait from the repr, so this provides
// a link back from the implemented repr type (e.g. i32) to the interpreter
// type (e.g. Eval).
trait HasExprSym {
    type ES: ExprSym;
}

impl HasExprSym for i32 {
    type ES = Eval;
}

fn exprsym_eval(e: i32) -> i32 {
    e
}

// And here is view:
struct View;
impl ExprSym for View {
    type Repr = String;

    fn lit(i: i32) -> Self::Repr {
        i.to_string()
    }

    fn neg(r: Self::Repr) -> Self::Repr {
        format!("(-{})", r)
    }

    fn add(r1: Self::Repr, r2: Self::Repr) -> Self::Repr {
        format!("({} + {})", r1, r2)
    }
}

impl HasExprSym for String {
    type ES = View;
}

fn exprsym_view(e: String) -> String {
    e
}

// You probably have questions now. In particular, why would you use this weird final style over the familiar intial style?
// And are the two really equivalent? For example, we seem to be losing pattern matching in the final style.

// We'll tackle some of these questions in turn.

// First, why would you use this final style over the initial style? One reason could be extensibility. We've already seen that
// both the initial and final style are easily extensible with new interpreters. In the initial style, we just write a new function
// and pattern match. In the final style, we add a new trait implementation.

// The final style is additionally easily extensible with new syntax. In the initial style, we'd have to add a new enum variant:

enum ExprUgh {
    Lit(i32),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
}

// and rewrite all interpreters (eval, view, ...). Ugh.

// In the final style, we just add extend the trait:

trait MulExprSym: ExprSym {
    fn mul(r1: Self::Repr, r2: Self::Repr) -> Self::Repr;
}

// and add a new implementation:
impl MulExprSym for Eval {
    fn mul(r1: Self::Repr, r2: Self::Repr) -> Self::Repr {
        r1 * r2
    }
}

// Thus the final style effectively solves the expression problem.

// A second advantage may be that we got rid of the Boxes in the enum - so potentially final style is faster?
// TODO: benchmark

// Now let's turn our attention to the what seems like final style's major limitation: the lack of pattern matching.

// For an example where pattern matching is convenient, let's consider the case where we want to push down negation
// to literals, getting rid of double negation. You can think of this as an example of an optimization pass.

impl Expr {
    fn push_neg(self) -> Expr {
        match &self {
            Expr::Lit(_) => self,
            Expr::Neg(content) => match content.as_ref() {
                Expr::Lit(_) => self,
                Expr::Neg(c) => c.to_owned().push_neg(),
                Expr::Add(r1, r2) => Expr::add(
                    Expr::Neg(r1.clone()).push_neg(),
                    Expr::Neg(r2.clone()).push_neg(),
                ),
            },
            Expr::Add(r1, r2) => Expr::add(r1.to_owned().push_neg(), r2.to_owned().push_neg()),
        }
    }
}

// The result is a new expression which again we can interpret in many ways, e.g. eval and view.

// Now let's see how we can do the same thing in the final style. In the inital style, it's clear that
// the transformation depends on context - in particular we need to push down negation if an expression
// occurs as part of another negation.

// In the final stye, all we can really do is write a new interpreter - as a new implementation of the ExprSym trait.
// The trait is parametrized by the associated type Repr, and we can achieve the same effect by making the context
// explicit:
enum Ctx {
    Pos,
    Neg,
}

struct CtxFun<TRepr>(Box<dyn Fn(&Ctx) -> TRepr>);

impl<TRepr> CtxFun<TRepr> {
    fn new<F>(f: F) -> Self
    where
        for<'a> F: Fn(&Ctx) -> TRepr + 'a,
    {
        CtxFun(Box::new(f))
    }
}

// PhantomData here to get around "unconstrained type parameter T" in trait impl.
struct PushNeg<T>(PhantomData<T>);
impl<T: ExprSym> ExprSym for PushNeg<T>
where
    for<'a> T: 'a,
{
    type Repr = CtxFun<T::Repr>;

    fn lit(i: i32) -> Self::Repr {
        CtxFun::new(move |ctx| match ctx {
            Ctx::Pos => T::lit(i),
            Ctx::Neg => T::neg(T::lit(i)),
        })
    }

    fn neg(r: Self::Repr) -> Self::Repr {
        CtxFun::new(move |ctx| match ctx {
            Ctx::Pos => r.0(&Ctx::Neg),
            Ctx::Neg => r.0(&Ctx::Pos),
        })
    }

    fn add(r1: Self::Repr, r2: Self::Repr) -> Self::Repr {
        CtxFun::new(move |ctx| T::add(r1.0(ctx), r2.0(ctx)))
    }
}

fn exprsym_push_neg0<S: ExprSym>(e: CtxFun<S::Repr>) -> S::Repr {
    e.0(&Ctx::Pos)
}

// What's this business with the phantom data?
// (Should introduce the PushNeg without PhantomData first)
// It is only needed to implement HasExprSym.
struct CtxFunPh<TRepr, T>(Box<dyn Fn(&Ctx) -> TRepr>, PhantomData<T>);

impl<TRepr, T> CtxFunPh<TRepr, T> {
    fn new<F>(f: F) -> Self
    where
        for<'a> F: Fn(&Ctx) -> TRepr + 'a,
    {
        CtxFunPh(Box::new(f), PhantomData)
    }
}

// PhantomData here to get around "unconstrained type parameter T" in trait impl.
struct PushNegPh<T>(PhantomData<T>);
impl<T: ExprSym> ExprSym for PushNegPh<T>
where
    for<'a> T: 'a,
{
    type Repr = CtxFunPh<T::Repr, T>;

    fn lit(i: i32) -> Self::Repr {
        CtxFunPh::new(move |ctx| match ctx {
            Ctx::Pos => T::lit(i),
            Ctx::Neg => T::neg(T::lit(i)),
        })
    }

    fn neg(r: Self::Repr) -> Self::Repr {
        CtxFunPh::new(move |ctx| match ctx {
            Ctx::Pos => r.0(&Ctx::Neg),
            Ctx::Neg => r.0(&Ctx::Pos),
        })
    }

    fn add(r1: Self::Repr, r2: Self::Repr) -> Self::Repr {
        CtxFunPh::new(move |ctx| T::add(r1.0(ctx), r2.0(ctx)))
    }
}

// Here I'd love to write just CtxFun<T::Repr>, but then the compiler complains
// T is not constrained. So we pass on the T into CtxFun as phantomdata.
impl<T: ExprSym> HasExprSym for CtxFunPh<T::Repr, T>
where
    for<'a> T: 'a,
{
    type ES = PushNegPh<T>;
}

fn exprsym_push_neg<S: ExprSym<Repr = T>, T: HasExprSym<ES = S>>(e: CtxFunPh<T, S>) -> T {
    e.0(&Ctx::Pos)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ti1() -> Expr {
        Expr::add(
            Expr::lit(8),
            Expr::neg(Expr::add(Expr::lit(1), Expr::lit(2))),
        )
    }

    fn tf1_pre<E: ExprSym>() -> E::Repr {
        E::add(E::lit(8), E::neg(E::add(E::lit(1), E::lit(2))))
    }

    fn tf1<E: ExprSym<Repr = T>, T: HasExprSym<ES = E>>() -> T {
        E::add(E::lit(8), E::neg(E::add(E::lit(1), E::lit(2))))
    }

    #[test]
    fn eval_equal() {
        // exprsym_eval(tf1_pre());
        let initial_style = Expr::eval(&ti1());
        let final_style = exprsym_eval(tf1());

        assert_eq!(initial_style, final_style);
        dbg!(final_style);
    }
    #[test]
    fn view_equal() {
        let initial_style = Expr::view(&ti1());
        let final_style = exprsym_view(tf1());

        assert_eq!(initial_style, final_style);
        dbg!(final_style);
    }

    fn tfm1<E: MulExprSym<Repr = T>, T: HasExprSym<ES = E>>() -> T {
        E::add(E::lit(7), E::neg(E::mul(E::lit(1), E::lit(2))))
    }

    #[test]
    fn mul_extensibility() {
        let final_style = exprsym_eval(tfm1());
        assert_eq!(5, final_style);

        // Type safety without pattern match exhaustiveness checking:
        // error[E0277]: the trait bound `View: MulExprSym` is not satisfied
        // let final_style = exprsym_view(tfm1());
        // because we have indeed not implement MulExprSym for View.
    }

    #[test]
    fn push_neg_equal() {
        let initial_style = ti1().push_neg();
        let final_style = exprsym_push_neg(tf1());

        assert_eq!(Expr::view(&initial_style), exprsym_view(final_style));
        dbg!(Expr::view(&initial_style));

        let r = tf1_pre::<PushNeg<View>>();
        dbg!(r.0(&Ctx::Pos));
    }
}
