use crate::parser::*;
use std::marker::PhantomData;

/// A parser combinator that applies two parsers in sequence,
/// but only returns the result of the first.
///
/// The second parser must succeed for the whole `PAnd` parser to succeed,
/// but its result is ignored. This is useful when you want to ensure
/// that something follows a valid parse, but don't care about capturing it.
///
/// Note: The second parser is applied at the *same location* as the first,
/// making this more of a "lookahead validator" than a true sequential combinator.
pub struct PAnd<P1, P2, A> {
    first: P1,
    second: P2,
    _marker: PhantomData<A>,
}

/// Construct a `PAnd` parser from two parsers.
///
/// `first` is the main parser whose result is returned.
/// `second` must also succeed, but its result is discarded.
///
/// # Type Parameters
/// - `P1`: The type of the first parser
/// - `P2`: The type of the second parser
/// - `A`: The output type of the second parser (phantom only)
///
/// # Example
/// ```rust
/// use cypress::prelude::*;
/// let input = "\"This is a string\"".into_input();
/// let parser = just('\"')
///         .then(any().and(just('\"').not()).many())
///         .then(just('\"'))
///         .map(|((_, xs), _)| String::from_utf8(xs).unwrap());
///
/// match parser.parse(input) {
///     Ok(PSuccess { val, rest: _ }) => assert_eq!(val, String::from("This is a string")),
///     Err(_) => assert!(false),
/// }
/// ```
pub fn pand<P1, P2, A>(first: P1, second: P2) -> PAnd<P1, P2, A> {
    PAnd {
        first,
        second,
        _marker: PhantomData,
    }
}

impl<'a, K, O, P1, P2, A> ParserCore<'a, K, O> for PAnd<P1, P2, A>
where
    K: PartialEq + Clone + 'a,
    O: 'a,
    A: 'a,
    P1: Parser<'a, K, O>,
    P2: Parser<'a, K, A>,
{
    /// Parse input using the `first` parser, then verify the `second` parser also succeeds.
    ///
    /// Only the result of `first` is returned.
    /// `second` is applied to the original input (same location) and must succeed,
    /// but its value is discarded.
    ///
    /// Returns `Ok(PSuccess)` with the output from `first` if both succeed,
    /// or `Err(PFail)` if either fails.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>> {
        let loc = i.loc;
        let tokens = i.tokens;

        // Try parsing with the first parser
        let PSuccess { val, rest } = self.first.parse(i)?;

        // Re-parse from original position with the second parser
        // to validate its presence, discarding its output
        let PSuccess { val: _, rest: _ } = self.second.parse(PInput { tokens, loc })?;

        // Return the result of the first parser
        Ok(PSuccess { val, rest })
    }
}

impl<'a, K, O, P1, P2, A> Parser<'a, K, O> for PAnd<P1, P2, A>
where
    K: PartialEq + Clone + 'a,
    O: 'a,
    A: 'a,
    P1: Parser<'a, K, O>,
    P2: Parser<'a, K, A>,
{
}
