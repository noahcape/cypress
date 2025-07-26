use crate::parser::*;
use std::marker::PhantomData;

/// A parser combinator that parses one or more occurrences of a pattern `p`
/// separated (delimited) by another pattern `delim`.
///
/// This combinator repeatedly applies the parser `p` and expects `delim`
/// to appear between consecutive results. It returns a `Vec` of parsed values
/// from `p`.
///
/// # Type Parameters
///
/// * `P1` - The parser type for the main pattern to parse.
/// * `P2` - The parser type for the delimiter pattern.
/// * `A` - A phantom type to track the output type of the delimiter parser (typically ignored).
pub struct PDelim<P1, P2, A> {
    /// The parser for the main pattern to be parsed multiple times.
    p: P1,
    /// The parser for the delimiter that separates each main pattern occurrence.
    delim: P2,
    /// Phantom type to hold the output type of the delimiter parser.
    _marker: PhantomData<A>,
}

/// Creates a new `PDelim` parser combinator that parses zero or more
/// occurrences of `p` separated by `delim`.
///
/// # Arguments
///
/// * `p` - The parser for the main pattern to parse repeatedly.
/// * `delim` - The parser for the delimiter between main pattern occurrences.
///
/// # Returns
///
/// A `PDelim` instance which implements `ParserCore` and parses
/// sequences of `p` separated by `delim`, returning a vector of parsed values.
pub fn pdelim<P1, P2, A>(p: P1, delim: P2) -> PDelim<P1, P2, A> {
    PDelim {
        p,
        delim,
        _marker: PhantomData,
    }
}

impl<'a, K, O, P, PDelim_, B> ParserCore<'a, K, Vec<O>> for PDelim<P, PDelim_, B>
where
    K: PartialEq + Copy + Clone + 'a,
    P: Parser<'a, K, O>,
    PDelim_: Parser<'a, K, B>,
{
    /// Parses zero or more occurrences of `p` separated by `delim` from the input.
    ///
    /// This function tries to parse `p` repeatedly, expecting `delim` between each
    /// pair of `p` parses. It collects all successfully parsed values of `p` into a vector.
    ///
    /// If parsing `p` fails before any values have been parsed, it returns the failure.
    /// If at least one value is parsed and the next parse of `p` fails, it returns
    /// success with the collected values.
    ///
    /// # Arguments
    ///
    /// * `i` - The input to parse from.
    ///
    /// # Returns
    ///
    /// * `Ok(PSuccess)` with a vector of parsed values if at least one parse succeeded.
    /// * `Err(PFail)` if no values could be parsed.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, Vec<O>>, PFail<'a, K>> {
        let mut vals = vec![];
        let mut input = i;

        loop {
            match self.p.parse(input.clone()) {
                Ok(PSuccess { val, rest }) => {
                    vals.push(val);
                    input = rest;

                    match self.delim.parse(input.clone()) {
                        Ok(PSuccess { val: _, rest }) => {
                            // Delimiter found, continue parsing more p's
                            input = rest;
                            continue;
                        }
                        Err(_) => {
                            // No more delimiters, parsing complete
                            return Ok(PSuccess {
                                val: vals,
                                rest: input,
                            });
                        }
                    }
                }
                Err(PFail { error, span, rest }) => {
                    if !vals.is_empty() {
                        // Return partial success if at least one value parsed
                        return Ok(PSuccess { val: vals, rest });
                    } else {
                        // Fail if no values parsed
                        return Err(PFail { error, span, rest });
                    }
                }
            }
        }
    }
}

impl<'a, K, O, P, PDelim_, B> Parser<'a, K, Vec<O>> for PDelim<P, PDelim_, B>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P: Parser<'a, K, O>,
    PDelim_: Parser<'a, K, B>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (Vec<O>, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, Vec<O>>
    where
        T: Parser<'a, K, Vec<O>>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(Vec<O>) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, Vec<O>>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<Vec<O>>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<Vec<O>>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, A>(self, pad: Pad) -> impl Parser<'a, K, Vec<O>>
    where
        Pad: Parser<'a, K, A> + Clone,
    {
        ppadded(self, pad)
    }

    fn into_<Out>(self, out: Out) -> impl Parser<'a, K, Out>
    where
        Out: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }

    fn debug(self, label: &'static str) -> impl Parser<'a, K, Vec<O>> {
        debug(self, label)
    }

    fn and<P2, A>(self, second: P2) -> impl Parser<'a, K, Vec<O>>
    where
        P2: Parser<'a, K, A>,
    {
        pand(self, second)
    }
}
