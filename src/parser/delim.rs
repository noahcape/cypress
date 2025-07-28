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
    O: 'a,
    B: 'a,
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
    B: 'a,
    P: Parser<'a, K, O>,
    PDelim_: Parser<'a, K, B>,
{
}
