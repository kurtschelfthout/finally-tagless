# Typed Final Tagless Interpreters

A translation of some of the examples in Oleg Kiselyov's [lecture notes](https://okmij.org/ftp/tagless-final/course/lecture.pdf) to Rust.

Logical order:

- first_order.rs: a simple first order language with addition and negation, with printing and evaluating in initial and final style.
- higher_order.rs: higher order lambda calculus with addition, again printing and evaluating, in initial and final style.
- print.rs: an application of the final style to type-safe formatting and parsing.