use std::rc::Rc;

// https://doc.rust-lang.org/std/fmt/struct.Arguments.html says:
// "This structure represents a safely precompiled version of a format string and its arguments.
// This cannot be generated at runtime because it cannot safely be done, so no constructors are given and the fields are private to prevent modification."

// By translating Oleg Kiselyov's tagless-final sprintf (aka format! in Rust) interpreter to Rust. https://okmij.org/ftp/tagless-final/course/lecture.pdf
// This demonstrates that it is not only possible, but has extensibility advantages.
// I demonstrate the extensibility by interpreting the format DSL as a parser.

// The implementation in Rust terms is probably inefficient. I have not benchmarked, but it uses heap allocation, ref counts, closures and cloning liberally.
// My aim here was just to demonstrate the principle.

type Fun<A, B> = Rc<dyn Fn(A) -> B>;

fn new_fun<A, B>(f: impl Fn(A) -> B + 'static) -> Fun<A, B> {
    Rc::new(f)
}

trait FormattingSpec {
    type Repr<A, B>;

    fn lit<A>(s: &str) -> Self::Repr<A, A>;
    fn int<A: 'static>() -> Self::Repr<A, Fun<i32, A>>;
    fn char<A: 'static>() -> Self::Repr<A, Fun<char, A>>;
    fn compose<A: 'static, B: 'static, C: 'static>(
        f: Self::Repr<B, C>,
        g: Self::Repr<A, B>,
    ) -> Self::Repr<A, C>;
}

// (String -> A) -> B
struct FPrint<A, B>(Fun<Fun<String, A>, B>);

struct Print;
impl FormattingSpec for Print {
    type Repr<A, B> = FPrint<A, B>;

    fn lit<A>(s: &str) -> Self::Repr<A, A> {
        let s = s.to_string();
        FPrint(new_fun(move |k: Fun<_, _>| k(s.clone())))
    }

    fn int<A: 'static>() -> Self::Repr<A, Fun<i32, A>> {
        FPrint(new_fun(move |k: Fun<_, _>| {
            new_fun(move |i: i32| k(i.to_string()))
        }))
    }

    fn char<A: 'static>() -> Self::Repr<A, Fun<char, A>> {
        FPrint(new_fun(move |k: Fun<_, _>| {
            new_fun(move |c: char| k(c.to_string()))
        }))
    }

    fn compose<A: 'static, B: 'static, C: 'static>(
        f: Self::Repr<B, C>,
        g: Self::Repr<A, B>,
    ) -> Self::Repr<A, C> {
        FPrint(new_fun(move |k: Fun<_, _>| {
            let f = Rc::clone(&f.0);
            let g = Rc::clone(&g.0);
            (f)(new_fun(move |sf: String| {
                let k = Rc::clone(&k);
                (g)(new_fun(move |sg: String| (k)(sf.clone() + &sg)))
            }))
        }))
    }
}

fn sprintf<B>(f: FPrint<String, B>) -> B {
    f.0(Rc::new(|s| s))
}

struct FScan<A, B>(Fun<(String, B), Option<(A, String)>>);

fn int_from_front(s: &str) -> (Option<i32>, &str) {
    let iter = s.char_indices();
    let (mut f, mut rest) = (None, s);
    for (i, c) in iter {
        if !char::is_numeric(c) || i == s.len() - 1 {
            let (ff, restt) = s.split_at(i + 1);
            f = Some(ff.parse().unwrap());
            rest = restt;
            break;
        }
    }
    (f, rest)
}

struct Scan;
impl FormattingSpec for Scan {
    type Repr<A, B> = FScan<A, B>;

    fn lit<A>(s: &str) -> FScan<A, A> {
        let s = s.to_string();
        FScan(new_fun(move |(inp, k): (String, A)| {
            if !inp.starts_with(&s) {
                None
            } else {
                let (_, r) = inp.split_at(s.len());
                Some((k, r.to_string()))
            }
        }))
    }

    fn int<A>() -> FScan<A, Fun<i32, A>> {
        FScan(new_fun(move |(inp, k): (String, Fun<i32, A>)| {
            let (f, r) = int_from_front(&inp);
            f.map(|f| (k(f), r.to_string()))
        }))
    }

    fn char<A>() -> FScan<A, Fun<char, A>> {
        FScan(new_fun(move |(inp, k): (String, Fun<char, A>)| {
            if inp.is_empty() {
                None
            } else {
                let (f, r) = inp.split_at(1);
                Some((k(f.chars().next().unwrap()), r.to_string()))
            }
        }))
    }

    fn compose<A: 'static, B: 'static, C: 'static>(a: FScan<B, C>, b: FScan<A, B>) -> FScan<A, C> {
        FScan(new_fun(move |(inp, f): (String, C)| {
            let r = a.0((inp, f));
            r.and_then(|(vb, inp2)| b.0((inp2, vb)))
        }))
    }
}

fn sscanf<A, B>(str: &str, scan: FScan<A, B>, b: B) -> Option<A> {
    scan.0((str.to_string(), b)).map(|(a, _)| a)
}

#[cfg(test)]
mod tests {

    use super::*;

    // Although the API can be used directly, it is quite cumbersome.
    // If Rust would allow defining new infix operators for compose, it
    // might look somewhat better.
    // I have not found a way to overload say the + operator for this purpose,
    // because it requires implementing Add for compose which has lots of generic
    // parameters not constrained by the implementing type F::Repr<A,B>.
    // A relatively simple set of macros should do the trick here - all the
    // relevant constraints are enforced in the type system.
    fn fmt1<F: FormattingSpec, A>() -> F::Repr<A, A> {
        F::lit("Hello, ")
    }

    fn fmt2<F: FormattingSpec, A: 'static>() -> F::Repr<A, Fun<char, A>> {
        F::compose(F::lit("Hello, world"), F::char())
    }

    fn fmt3<F: FormattingSpec, A: 'static>() -> F::Repr<A, Fun<char, Fun<i32, A>>> {
        F::compose(
            F::lit("The value of "),
            F::compose(F::char(), F::compose(F::lit(" is "), F::int())),
        )
    }

    #[test]
    fn print_fmt1() {
        // Here it is actually hard/annoying to implement something like HasFormattingSpec,
        // because of the generic function type variable A, and the generic associated type.
        let s = sprintf(fmt1::<Print, _>());
        assert_eq!(s, "Hello, ");
    }

    #[test]
    fn print_fmt2() {
        let r = fmt2::<Print, _>();
        let s = sprintf(r)('!');
        assert_eq!(s, "Hello, world!");
    }

    #[test]
    fn print_fmt3() {
        let r = fmt3::<Print, _>();
        let s = sprintf(r)('C')(67);

        assert_eq!(s, "The value of C is 67");
    }

    #[test]
    fn scan_fmt1() {
        let r = fmt1::<Scan, _>();
        let scan_r = sscanf("Hello, ", r, ());
        assert_eq!(Some(()), scan_r);

        let r = fmt1::<Scan, _>();
        let scan_r = sscanf("Hello ", r, ());
        assert_eq!(None, scan_r);
    }

    #[test]
    fn scan_fmt2() {
        let r = fmt2::<Scan, _>();
        let s = sscanf("Hello, world?", r, new_fun(|x| x));
        assert_eq!(s, Some('?'));
    }

    #[test]
    fn scan_fmt3() {
        let r = fmt3::<Scan, _>();
        let s = sscanf(
            "The value of C is 67",
            r,
            new_fun(|c| new_fun(move |i| (c, i))),
        );

        assert_eq!(s, Some(('C', 67)));
    }

    #[test]
    fn test_split() {
        let s = "123Hello456";
        let iter = s.char_indices();
        let (mut a, mut b) = ("", s);
        for (i, c) in iter {
            if !char::is_numeric(c) {
                (a, b) = s.split_at(i);
                break;
            }
        }
        assert_eq!(a, "123");
        assert_eq!(b, "Hello456");
    }
}
