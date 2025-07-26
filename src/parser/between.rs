use crate::parser::*;
use std::marker::PhantomData;

/// A parser combinator that parses a value surrounded by two delimiters.
///
/// This combinator runs three parsers in sequence:
/// - `l`: the left delimiter parser
/// - `p`: the main content parser
/// - `r`: the right delimiter parser
///
/// It discards the results from `l` and `r`, and returns the result from `p`.
///
/// This is commonly used for parsing things like:
/// - Parentheses: `( expression )`
/// - Brackets: `[ item ]`
/// - Quotes: `"text"`
///
/// # Type Parameters
/// - `L`: Parser for the left delimiter
/// - `P`: Parser for the main content
/// - `R`: Parser for the right delimiter
/// - `A`: Output type of the left and right parsers (discarded)
pub struct PBetween<L, P, R, A> {
    l: L,
    p: P,
    r: R,
    _marker: PhantomData<A>,
}

/// Constructs a `PBetween` combinator.
///
/// This runs three parsers in order: `l`, `p`, and `r`. It returns the result of `p`,
/// while discarding the results of `l` and `r`.
///
/// Useful for building structure-aware parsers that wrap content with delimiters.
///
/// # Parameters
/// - `l`: Parser to run first (e.g., left delimiter)
/// - `p`: Parser to extract the main value
/// - `r`: Parser to run last (e.g., right delimiter)
///
/// # Example
/// ```rust
/// use parsec::prelude::*;
///
/// let input = "[A]".as_bytes().into_input();
/// let parser = pbetween(just('['), just('A'), just(']'));
///
/// match parser.parse(input) {
///     Ok(PSuccess { val, rest: _ }) => assert_eq!(val, b'A'),
///     Err(_) => assert!(false),
/// }
/// ```
pub fn pbetween<L, P, R, A>(l: L, p: P, r: R) -> PBetween<L, P, R, A> {
    PBetween {
        l,
        p,
        r,
        _marker: PhantomData,
    }
}

impl<'a, K, O, L, P, R, A> ParserCore<'a, K, O> for PBetween<L, P, R, A>
where
    K: PartialEq + Copy + Clone + 'a,
    L: Parser<'a, K, A>,
    P: Parser<'a, K, O>,
    R: Parser<'a, K, A>,
{
    /// Parses the input using three parsers in sequence: `l`, `p`, and `r`.
    ///
    /// Only the result of `p` is returned. If any parser fails, the whole parse fails.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>> {
        // Apply the left parser (discard its result)
        let PSuccess {
            val: _,
            rest: after_l,
        } = self.l.parse(i)?;

        // Apply the main parser (capture its result)
        let PSuccess {
            val,
            rest: after_main,
        } = self.p.parse(after_l)?;

        // Apply the right parser (discard its result)
        let PSuccess {
            val: _,
            rest: after_r,
        } = self.r.parse(after_main)?;

        // Return the result of the main parser
        Ok(PSuccess { val, rest: after_r })
    }
}

impl<'a, K, O, L, P, R, A> Parser<'a, K, O> for PBetween<L, P, R, A>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    L: Parser<'a, K, A>,
    P: Parser<'a, K, O>,
    R: Parser<'a, K, A>,
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

    fn between<B, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, O>
    where
        P1: Parser<'a, K, B>,
        P2: Parser<'a, K, B>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<O>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A_>(self, delim: PD) -> impl Parser<'a, K, Vec<O>>
    where
        PD: Parser<'a, K, A_>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, C>(self, pad: Pad) -> impl Parser<'a, K, O>
    where
        Pad: Parser<'a, K, C> + Clone,
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

    fn and<P2, A_>(self, second: P2) -> impl Parser<'a, K, O>
    where
        P2: Parser<'a, K, A_>,
    {
        pand(self, second)
    }
}
