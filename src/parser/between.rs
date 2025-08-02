use crate::{
    error::{Error, ErrorKind},
    parser::*,
};
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
/// use cypress::prelude::*;
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
    K: PartialEq + Clone + 'a,
    A: 'a,
    O: 'a,
    L: Parser<'a, K, A>,
    P: Parser<'a, K, O>,
    R: Parser<'a, K, A>,
{
    /// Parses the input using three parsers in sequence: `l`, `p`, and `r`.
    ///
    /// Only the result of `p` is returned. If any parser fails, the whole parse fails.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, Error<'a, K>> {
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
        match self.r.parse(after_main) {
            Ok(PSuccess {
                val: _,
                rest: after_r,
            }) => Ok(PSuccess { val, rest: after_r }),
            Err(Error {
                mut kind,
                span,
                state,
            }) => {
                kind.push(ErrorKind::Custom("Missing end parsing between."));
                Err(Error { kind, span, state })
            }
        }
    }
}

impl<'a, K, O, L, P, R, A> Parser<'a, K, O> for PBetween<L, P, R, A>
where
    K: PartialEq + Clone + 'a,
    O: 'a,
    A: 'a,
    L: Parser<'a, K, A>,
    P: Parser<'a, K, O>,
    R: Parser<'a, K, A>,
{
}
