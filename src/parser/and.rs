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
/// use parsec::prelude::*;
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
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
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
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P1: Parser<'a, K, O>,
    P2: Parser<'a, K, A>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (O, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, O>
    where
        T: Parser<'a, K, O>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(O) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A_, P1_, P2_>(self, l: P1_, r: P2_) -> impl Parser<'a, K, O>
    where
        P1_: Parser<'a, K, A_>,
        P2_: Parser<'a, K, A_>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<O>> {
        pmany(self)
    }

    fn delimited_by<PD, A_>(self, delim: PD) -> impl Parser<'a, K, Vec<O>>
    where
        PD: Parser<'a, K, A_>,
    {
        pdelim(self, delim)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn padded_by<P, A_>(self, pad: P) -> impl Parser<'a, K, O>
    where
        P: Parser<'a, K, A_> + Clone,
    {
        ppadded(self, pad)
    }

    fn into_<Out>(self, out: Out) -> impl Parser<'a, K, Out>
    where
        Out: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }

    fn debug(self, label: &'static str) -> impl Parser<'a, K, O> {
        debug(self, label)
    }

    fn and<P2_, A_>(self, second: P2_) -> impl Parser<'a, K, O>
    where
        P2_: Parser<'a, K, A_>,
    {
        pand(self, second)
    }
}
