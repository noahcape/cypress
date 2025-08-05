use std::marker::PhantomData;

use crate::prelude::{PInput, PSuccess, Parser, ParserCore, error::Error};

/// A combinator that runs two parsers in sequence,
/// returning the result of the first (`then`) and discarding the result of the second (`ignore`).
///
/// This is useful when you want to consume a trailing token (like a delimiter or punctuation)
/// but keep only the preceding value.
///
/// # Type Parameters
/// - `P1`: The parser whose result is kept.
/// - `P2`: The parser whose result is ignored.
/// - `OI`: The output type of the ignored parser.
#[derive(Clone)]
pub struct PThenIgnore<P1, P2, OI> {
    /// The parser whose result will be kept.
    then: P1,
    /// The parser whose result will be discarded.
    ignore: P2,
    /// Marker for the unused output type `OI`.
    _marker: PhantomData<OI>,
}

/// Constructs a [`PThenIgnore`] combinator from two parsers.
///
/// This is a convenience function to create the combinator without needing to specify the generic types.
///
/// # Example
/// ```rust
/// use cypress::prelude::*;
///
/// let parser = pident("x").then_ignore(just(';'));
/// match parser.parse("x;".into_input()) {
///     Ok(PSuccess {val, rest: _ }) => assert_eq!(val, "x"),
///     Err(_) => assert!(false)
/// }
/// ```
///
/// # Parameters
/// - `then`: The parser to run first and return its result.
/// - `ignore`: The parser to run after and discard its result.
///
/// # Returns
/// A new parser that sequences the two parsers but only returns the first's result.
pub fn pthen_ignore<P1, P2, OI>(then: P1, ignore: P2) -> PThenIgnore<P1, P2, OI> {
    PThenIgnore {
        then,
        ignore,
        _marker: PhantomData,
    }
}

impl<'a, K, O, P1, P2, OI> ParserCore<'a, K, O> for PThenIgnore<P1, P2, OI>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    OI: Clone + 'a,
    P1: Parser<'a, K, O>,
    P2: Parser<'a, K, OI>,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, Error<'a, K>> {
        // First, try to parse using the `then` parser to consume the trailing value.
        let PSuccess { val, rest } = self.then.parse(i)?;
        // Then, parse the remaining input using the `ignore` parser and forget its value.
        let PSuccess { val: _, rest } = self.ignore.parse(rest)?;

        Ok(PSuccess { val, rest })
    }
}

// Blanket implementation of `Parser` for `PThenIgnore`
// so it can be used anywhere a `Parser` is expected.
impl<'a, K, O, P1, P2, OI> Parser<'a, K, O> for PThenIgnore<P1, P2, OI>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    OI: Clone + 'a,
    P1: Parser<'a, K, O>,
    P2: Parser<'a, K, OI>,
{
}
