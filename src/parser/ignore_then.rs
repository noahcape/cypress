use std::marker::PhantomData;

use crate::prelude::{PInput, PSuccess, Parser, ParserCore, error::Error};

/// A combinator that runs two parsers sequentially,
/// discarding the result of the first (`ignore`) and keeping only the result of the second (`then`).
///
/// This is useful when you want to skip over some syntax (e.g. punctuation, keywords)
/// and keep only the meaningful result.
///
/// # Type Parameters
/// - `P1`: The parser whose result is ignored.
/// - `P2`: The parser whose result is kept.
/// - `OI`: The output type of the ignored parser (not used directly).
#[derive(Clone)]
pub struct PIgnoreThen<P1, P2, OI> {
    /// The parser whose result will be ignored.
    ignore: P1,
    /// The parser whose result will be returned.
    then: P2,
    /// Marker for the unused output type `OI`.
    _marker: PhantomData<OI>,
}

/// Constructs a [`PIgnoreThen`] combinator from two parsers.
///
/// This is a convenience function to create the combinator without needing to specify the generic types.
///
/// # Example
/// ```rust
/// use cypress::prelude::*;
///
/// let parser = just('(').ignore_then(pident("x"));
/// match parser.parse("(x".into_input()) {
///     Ok(PSuccess {val, rest: _ }) => assert_eq!(val, "x"),
///     Err(_) => assert!(false)
/// }
/// ```
///
/// # Parameters
/// - `ignore`: The parser to run first and discard its output.
/// - `then`: The parser to run second and return its output.
///
/// # Returns
/// A new parser that sequences the two parsers but only returns the second's result.
pub fn pignore_then<P1, P2, OI>(ignore: P1, then: P2) -> PIgnoreThen<P1, P2, OI> {
    PIgnoreThen {
        ignore,
        then,
        _marker: PhantomData,
    }
}

impl<'a, K, O, P1, P2, OI> ParserCore<'a, K, O> for PIgnoreThen<P1, P2, OI>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    OI: Clone + 'a,
    P1: Parser<'a, K, OI>,
    P2: Parser<'a, K, O>,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, Error<'a, K>> {
        // First, parse using the `ignore` parser, but discard its result.
        let PSuccess { val: _, rest } = self.ignore.parse(i)?;
        // Then, parse the remaining input with the `then` parser and return its result.
        self.then.parse(rest)
    }
}

// Blanket implementation of `Parser` trait for `PIgnoreThen`.
// This allows it to be used anywhere a `Parser` is expected.
impl<'a, K, O, P1, P2, OI> Parser<'a, K, O> for PIgnoreThen<P1, P2, OI>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    OI: Clone + 'a,
    P1: Parser<'a, K, OI>,
    P2: Parser<'a, K, O>,
{
}
