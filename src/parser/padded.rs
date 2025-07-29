use crate::parser::*;
use std::marker::PhantomData;

/// A parser combinator that parses a pattern `p` padded by zero or more occurrences
/// of another pattern `pad` both before and after `p`.
///
/// This combinator applies the `pad` parser repeatedly (zero or more times),
/// then parses the main parser `p`, then applies `pad` repeatedly again.
/// It returns the result of parsing `p`, ignoring the padding.
///
/// # Type Parameters
///
/// * `P` - The parser type for the main pattern.
/// * `PD` - The parser type for the padding pattern.
/// * `A` - The output type of the padding parser (usually ignored).
pub struct PPaddedBy<P, PD, A> {
    /// The main parser to parse between paddings.
    p: P,
    /// The parser used for padding before and after `p`.
    pad: PD,
    /// Phantom marker to hold the padding parser's output type.
    _marker: PhantomData<A>,
}

/// Creates a new `PPaddedBy` parser combinator.
///
/// # Arguments
///
/// * `p` - The main parser to parse content.
/// * `pad` - The parser for the padding pattern.
///
/// # Returns
///
/// A `PPaddedBy` instance that implements `ParserCore`, parsing `pad* p pad*`
/// and returning the output of `p`.
pub fn ppadded<P, PD, A>(p: P, pad: PD) -> PPaddedBy<P, PD, A> {
    PPaddedBy {
        p,
        pad,
        _marker: PhantomData,
    }
}

impl<'a, K, O, P, PD, A> ParserCore<'a, K, O> for PPaddedBy<P, PD, A>
where
    K: PartialEq + Clone + 'a,
    O: 'a,
    A: 'a,
    P: Parser<'a, K, O>,
    PD: Parser<'a, K, A> + Clone,
{
    /// Parses input padded by zero or more occurrences of `pad` before and after the main parser `p`.
    ///
    /// The parsing sequence is:
    /// 1. Parse zero or more `pad`s.
    /// 2. Parse the main parser `p`.
    /// 3. Parse zero or more `pad`s.
    ///
    /// Returns the output of `p` and the remaining input after trailing padding.
    ///
    /// # Arguments
    ///
    /// * `i` - The input to parse.
    ///
    /// # Returns
    ///
    /// * `Ok(PSuccess)` containing the output of `p` and the remaining input.
    /// * `Err(PFail)` if any of the parsing steps fail.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>> {
        // Parse zero or more paddings before main parser
        let pad = self.pad.clone().many();

        let PSuccess {
            rest: after_pad1, ..
        } = pad.parse(i)?;

        // Parse the main parser `p`
        let PSuccess {
            val,
            rest: after_main,
        } = self.p.parse(after_pad1)?;

        // Parse zero or more paddings after main parser
        let PSuccess {
            rest: after_pad2, ..
        } = pad.parse(after_main)?;

        // Return the main parser's value and the remaining input after trailing padding
        Ok(PSuccess {
            val,
            rest: after_pad2,
        })
    }
}

impl<'a, K, O, P, PD, B> Parser<'a, K, O> for PPaddedBy<P, PD, B>
where
    K: PartialEq + Clone + 'a,
    O: 'a,
    B: 'a,
    P: Parser<'a, K, O>,
    PD: Parser<'a, K, B> + Clone,
{
}
