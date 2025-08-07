use std::marker::PhantomData;

use crate::{
    error::Error,
    prelude::{PInput, PSuccess, Parser, ParserCore},
};

/// A parser combinator that applies left-associative folding.
///
/// `PFoldLeft` takes three parsers:
/// - `head`: a parser that produces the initial value (`O`)
/// - `tail`: a parser that produces an iterable of additional values (`O2`)
/// - `f: Fn(O, O2) -> O`.
///
/// This is useful for building left-associative constructs such as arithmetic expressions
/// like `1 - 2 - 3` which should parse as `((1 - 2) - 3)`.
///
/// # Type Parameters
///
/// - `P`: Parser for the head element (left-most value).
/// - `TP`: Parser for the tail elements (right-side iterable).
/// - `F`: The folding function applied from left to right.
/// - `I`: The iterable type returned by the tail parser.
/// - `O2`: The item type inside the tail iterable.
///
#[derive(Clone)]
pub struct PFoldLeft<P, TP, F, I, O2> {
    head: P,
    tail: TP,
    f: F,
    _marker: PhantomData<(I, O2)>,
}

/// Constructs a `PFoldLeft` parser.
///
/// This combinator executes `head`, then `tail`, and folds the result of
/// `tail` into the result of `head` using the folding function `f`.
///
/// # Example
///
/// ```rust
/// use cypress::prelude::*;
///
/// // Parses: number followed by zero or more (+ number)
/// let parser = pfoldl(
///     pnum().map(|num: u8| num - b'0'),                        // initial value
///     just('+').then(pnum().map(|num: u8| num - b'0')).many(), // iterable values
///     |acc, (_, next)| acc + next        // fold function
/// );
///
/// match parser.parse("1+2+3".into_input()) {
///     Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 6),
///     Err(_) => assert!(false),
/// };
/// ```
pub fn pfoldl<P, TP, F, I, O2>(head: P, tail: TP, f: F) -> PFoldLeft<P, TP, F, I, O2> {
    PFoldLeft {
        head,
        tail,
        f,
        _marker: PhantomData,
    }
}

impl<'a, K, O, P, TP, F, I, O2> ParserCore<'a, K, O> for PFoldLeft<P, TP, F, I, O2>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    P: Parser<'a, K, O>,
    I: IntoIterator<Item = O2> + Clone + 'a,
    TP: Parser<'a, K, I>,
    F: Fn(O, O2) -> O + Clone + 'a,
{
    /// Runs the head parser, then the tail parser, and folds the results.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, Error<'a, K>> {
        let PSuccess { val: head, rest } = self.head.parse(i)?;
        let PSuccess { val: tail, rest } = self.tail.parse(rest)?;

        let res = tail.into_iter().fold(head, |acc, next| (self.f)(acc, next));
        Ok(PSuccess { val: res, rest })
    }
}

impl<'a, K, O, P, TP, F, I, O2> Parser<'a, K, O> for PFoldLeft<P, TP, F, I, O2>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    O2: Clone + 'a,
    P: Parser<'a, K, O>,
    I: IntoIterator<Item = O2> + Clone + 'a,
    TP: Parser<'a, K, I>,
    F: Fn(O, O2) -> O + Clone + 'a,
{
}
