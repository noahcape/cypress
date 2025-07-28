use crate::parser::*;
use std::marker::PhantomData;

/// A parser combinator that succeeds only if the inner parser `p` fails at the current input position.
///
/// This is a "negative lookahead" combinator: it checks whether `p` *does not* match
/// at the current input location without consuming any input.
/// If `p` succeeds, `PNot` fails; if `p` fails, `PNot` succeeds without consuming input.
///
/// # Type Parameters
///
/// * `P` - The inner parser type.
/// * `O` - The output type of the inner parser `p` (ignored here).
pub struct PNot<P, O> {
    /// The inner parser to negate.
    p: P,
    /// Phantom marker for the inner parser's output type.
    _marker: PhantomData<O>,
}

/// Creates a new `PNot` parser combinator that succeeds only if parser `p` fails.
///
/// # Arguments
///
/// * `p` - The parser to negate.
///
/// # Returns
///
/// A `PNot` instance implementing `ParserCore` which succeeds if `p` fails,
/// and fails if `p` succeeds, without consuming any input.
pub fn pnot<P, O>(p: P) -> PNot<P, O> {
    PNot {
        p,
        _marker: PhantomData,
    }
}

impl<'a, K, O, P> ParserCore<'a, K, ()> for PNot<P, O>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P: Parser<'a, K, O>,
{
    /// Attempts to parse the input with the inner parser `p`.
    ///
    /// If `p` succeeds, `PNot` fails with an error indicating unexpected input.
    /// If `p` fails, `PNot` succeeds without consuming any input, returning unit `()`.
    ///
    /// # Arguments
    ///
    /// * `i` - The parser input.
    ///
    /// # Returns
    ///
    /// * `Ok(PSuccess)` with unit `()` if `p` fails (input not matching `p`).
    /// * `Err(PFail)` if `p` succeeds (input matches `p`).
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, ()>, PFail<'a, K>> {
        let start = i.loc;
        match self.p.parse(i) {
            Ok(PSuccess { val: _, rest }) => Err(PFail {
                error: "Was what was not expected.".to_string(),
                span: (start, rest.loc),
                rest,
            }),
            Err(PFail {
                error: _,
                span: _,
                rest,
            }) => Ok(PSuccess { val: (), rest }),
        }
    }
}

impl<'a, K, O, P> Parser<'a, K, ()> for PNot<P, O>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P: Parser<'a, K, O>,
{
}
