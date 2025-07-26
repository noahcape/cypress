use crate::parser::*;
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
    K: PartialEq + Copy + Clone + 'a,
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
    /// * `Err(PFail)` if the inner parser `p` fails.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, Out>, PFail<'a, K>> {
        let PSuccess { val: _, rest } = self.p.parse(i)?;

        Ok(PSuccess {
            val: self.out.clone(),
            rest,
        })
    }
}

impl<'a, K, P, In, Out> Parser<'a, K, Out> for PInto<P, In, Out>
where
    K: PartialEq + Copy + Clone + 'a,
    Out: PartialEq + Clone + 'a,
    P: Parser<'a, K, In>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (Out, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, Out>
    where
        T: Parser<'a, K, Out>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(Out) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, Out>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<Out>> {
        pmany(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<Out>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn padded_by<Pr, A>(self, pad: Pr) -> impl Parser<'a, K, Out>
    where
        Pr: Parser<'a, K, A> + Clone,
    {
        ppadded(self, pad)
    }

    fn into_<Out_>(self, out: Out_) -> impl Parser<'a, K, Out_>
    where
        Out_: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }

    fn debug(self, label: &'static str) -> impl Parser<'a, K, Out> {
        debug(self, label)
    }

    fn and<P2, A>(self, second: P2) -> impl Parser<'a, K, Out>
    where
        P2: Parser<'a, K, A>,
    {
        pand(self, second)
    }
}
