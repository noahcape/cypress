use crate::{error::Error, parser::*};
use std::marker::PhantomData;

/// A parser combinator that runs an inner parser `p` but replaces its output
/// with a predefined value `out`.
///
/// This is useful when you want to parse some input pattern but disregard
/// the actual parsed value, instead returning a fixed output value.
///
/// # Type Parameters
///
/// * `P` - The type of the inner parser.
/// * `In` - The output type of the inner parser `p` (ignored).
/// * `Out` - The output type this combinator produces.
///
/// # Fields
///
/// * `p` - The inner parser to run.
/// * `out` - The value to output regardless of the inner parser's result.
/// * `_marker` - PhantomData to hold the `In` type.
pub struct PInto<P, In, Out> {
    /// Inner parser whose output is ignored.
    p: P,
    /// The output value to return instead of the inner parser's output.
    out: Out,
    /// Phantom type marker for the inner parser's output type.
    _marker: PhantomData<In>,
}

/// Creates a new `PInto` parser combinator that runs the parser `p`
/// and replaces its output with `out`.
///
/// # Arguments
///
/// * `p` - The inner parser to run.
/// * `out` - The value to return after parsing.
///
/// # Returns
///
/// A `PInto` instance that implements `ParserCore`, returning `out`
/// whenever `p` successfully parses input.
pub fn pinto<P, In, Out>(p: P, out: Out) -> PInto<P, In, Out> {
    PInto {
        p,
        out,
        _marker: PhantomData,
    }
}

impl<'a, K, P, In, Out> ParserCore<'a, K, Out> for PInto<P, In, Out>
where
    K: PartialEq + Clone + 'a,
    In: 'a,
    P: Parser<'a, K, In>,
    Out: PartialEq + Clone,
{
    /// Parses input using the inner parser `p`, then returns the fixed output `out`.
    ///
    /// # Arguments
    ///
    /// * `i` - The parser input.
    ///
    /// # Returns
    ///
    /// * `Ok(PSuccess)` containing the fixed output `out` and the remaining input if parsing succeeds.
    /// * `Err(Error)` if the inner parser `p` fails.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, Out>, Error<'a, K>> {
        let PSuccess { val: _, rest } = self.p.parse(i)?;

        Ok(PSuccess {
            val: self.out.clone(),
            rest,
        })
    }
}

impl<'a, K, P, In, Out> Parser<'a, K, Out> for PInto<P, In, Out>
where
    K: PartialEq + Clone + 'a,
    Out: PartialEq + Clone + 'a,
    In: 'a,
    P: Parser<'a, K, In>,
{
}
