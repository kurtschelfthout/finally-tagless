// In first_order.rs, we implemented a simple language and interpreted it in initial and final style.
// More "advanced" APIs/languages will have some higher order aspect - which needs functions as first class
// values, and function application. For example, in a dataframe API we might have an operation that maps a
// function to a particular column. Since this operation takes a function as an argument, we call it a higher-order
// operation.

// We will consider a similar language as before, but now we'll add the ability to define functions, and apply them (aka call them).

// Let's start again by doing this in the initial style, using an Expr enum.

use std::rc::Rc;

type VarId = usize;

#[derive(Clone)]
enum Expr {
    Var(VarId),
    Int(i32),
    Lam(Rc<Expr>),
    App(Rc<Expr>, Rc<Expr>),
}

impl Expr {
    fn var(id: VarId) -> Expr {
        Expr::Var(id)
    }

    fn int(i: i32) -> Expr {
        Expr::Int(i)
    }

    fn lam(body: Expr) -> Expr {
        Expr::Lam(Rc::new(body))
    }

    fn app(f: Expr, arg: Expr) -> Expr {
        Expr::App(Rc::new(f), Rc::new(arg))
    }
}

// We now have a way to define functions using Lam, and apply them using App. We also need variables, to define function arguments.

fn ti1() -> Expr {
    Expr::app(Expr::lam(Expr::var(0)), Expr::int(1))
}

// Writing an evaluator for this language is trickier than in the first order case.
// In particular, we need to keep track of the environment, which maps variables to values.

type Env = Vec<Val>;

// The bigger issue is that the result of evaluating an expression can be either an i32, or a function. The latter we'll represent by a
// rust function. To even start implementing eval, we first need to define a "universal type":

#[derive(Clone)]
enum Val {
    Int(i32),
    Fun(Rc<dyn Fn(Val) -> Val>),
}

// implementing eval is quite tricky - I had a hard time getting everything to work with the borrow checker.
// Perhaps there is an easier/better way to do this - I basically ended up throwing Rcs and clones everywhere.

// That's not really the point of this example though. What is important to realize is that this eval can go wrong
// in various ways:
// - First, there is the obvious panic when we try to apply something that isn't a function
// - Second, looking up variables in the environment is not guaranteed to work either

impl Expr {
    fn eval(env: Env, expr: Expr) -> Val {
        match expr {
            Expr::Var(id) => env[id].clone(),
            Expr::Int(i) => Val::Int(i),
            Expr::Lam(e) => Val::Fun(Rc::new(move |x| {
                let mut envr = env.clone();
                envr.push(x);
                Expr::eval(envr, e.as_ref().clone())
            })),

            Expr::App(e1, e2) => {
                let eval_e1 = Expr::eval(env.clone(), e1.as_ref().clone());
                let eval_e2 = Expr::eval(env, e2.as_ref().clone());
                match eval_e1 {
                    Val::Fun(f) => f(eval_e2),
                    _ => panic!("Expected function"),
                }
            }
        }
    }
}

// We can solve some of these issues by writing a type checker for the language.

// However, we already have a language that type checks such things for us: Rust. Can we leverage Rust's type system
// to avoid these problems directly?

// The canonical answer to this is GADTs (Generalized Algebraic Data Types). Rust does not support GADTS though.

// It turns out we don't need GADTs if we representing our language in the final style.

type Fun<A, B> = Box<dyn Fn(A) -> B>;

trait ExprSym {
    type Repr<T>;

    fn int(i: i32) -> Self::Repr<i32>;
    fn add(a: &Self::Repr<i32>, b: &Self::Repr<i32>) -> Self::Repr<i32>;
    fn lam<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(f: F) -> Self::Repr<Fun<A, B>>
    where
        for<'a> F: 'a;
    fn app<F: Fn(A) -> B, A, B>(f: Self::Repr<F>, arg: Self::Repr<A>) -> Self::Repr<B>;
}

// The implementation of eval is practically trivial - like writing the identity function:

struct Eval;
impl ExprSym for Eval {
    type Repr<T> = T;

    fn int(i: i32) -> Self::Repr<i32> {
        i
    }

    fn add(a: &Self::Repr<i32>, b: &Self::Repr<i32>) -> Self::Repr<i32> {
        a + b
    }

    fn lam<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(f: F) -> Self::Repr<Box<dyn Fn(A) -> B>>
    where
        for<'a> F: 'a,
    {
        Box::new(f)
    }

    fn app<F: Fn(A) -> B, A, B>(f: Self::Repr<F>, arg: Self::Repr<A>) -> Self::Repr<B> {
        f(arg)
    }
}

// We can see that the interpreter is meta-circular: integers _are_ Rust integers, functions _are_ Rust functions, and so on.
// Also, the interpreter obviously does not panic. Well-formedness of the DSL is guaranteed by the Rust type system.

// We can now write a pretty printer for the language:

struct View;
type VarCounter = i32;
impl ExprSym for View {
    type Repr<T> = Rc<dyn Fn(VarCounter) -> String>;

    fn int(i: i32) -> Self::Repr<i32> {
        Rc::new(move |_| format!("{i}"))
    }

    fn add(a: &Self::Repr<i32>, b: &Self::Repr<i32>) -> Self::Repr<i32> {
        let a = Rc::clone(a);
        let b = Rc::clone(b);
        Rc::new(move |count| format!("({} + {})", a(count), b(count)))
    }

    fn lam<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(f: F) -> Self::Repr<Fun<A, B>>
    where
        for<'a> F: 'a,
    {
        Rc::new(move |count| {
            format!(
                "(\\x{} -> {})",
                count,
                f(Rc::new(move |_| format!("x{count}")))(count + 1)
            )
        })
    }

    fn app<F: Fn(A) -> B, A, B>(f: Self::Repr<F>, arg: Self::Repr<A>) -> Self::Repr<B> {
        let f = Rc::clone(&f);
        let arg = Rc::clone(&arg);
        Rc::new(move |count| format!("({} {})", f(count), arg(count)))
    }
}

// And extending the DSL works like before, e.g.:
// trait MulSumSym: ExprSym

#[cfg(test)]
mod tests {
    use super::*;

    fn ti2a() -> Expr {
        Expr::app(Expr::int(2), Expr::int(3))
    }

    fn ti2b() -> Expr {
        Expr::app(Expr::lam(Expr::var(0)), Expr::int(3))
    }

    #[test]
    #[should_panic(expected = "Expected function")]
    fn expr_eval_apply_int() {
        Expr::eval(vec![], ti2a());
    }

    #[test]
    fn expr_eval_app() {
        let r = Expr::eval(vec![], ti2b());
        match r {
            Val::Int(i) => assert_eq!(i, 3),
            _ => panic!("Expected int"),
        }
    }

    // The equivalent of ti2a does not compile:
    // error[E0277]: expected a `std::ops::Fn<(_,)>` closure, found `i32`
    // fn tf2a<T, E: ExprSym>() -> E::Repr<T> {
    //     E::app(E::int(2), E::int(3))
    // }

    fn th1<E: ExprSym>() -> E::Repr<i32> {
        E::add(&E::int(1), &E::int(2))
    }

    fn th2<E: ExprSym>() -> E::Repr<Fun<i32, i32>> {
        E::lam(|x| E::add(&x, &x))
    }

    #[test]
    fn symexp_eval() {
        // I've not implemented the HasExprSym trait.
        // So we have to use a type argument here.
        let r = th1::<Eval>();
        assert_eq!(r, 3);

        let r = th2::<Eval>()(2);
        assert_eq!(r, 4);
    }

    #[test]
    fn symexp_view() {
        let r = th1::<View>();
        assert_eq!(r(0), "(1 + 2)");

        let r = th2::<View>();
        assert_eq!(r(0), "(\\x0 -> (x0 + x0))");
    }
}
