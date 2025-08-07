use crate::{
    error::{Error, ErrorKind},
    parser::*,
};
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
#[derive(Clone)]
pub struct PDelim<P1, P2, A> {
    /// The parser for the main pattern to be parsed multiple times.
    p: P1,
    /// The parser for the delimiter that separates each main pattern occurrence.
    delim: P2,
    /// fewest number of success parses of `p`
    at_least: usize,
    /// most number of successful parses of `p`
    at_most: usize,
    /// allow a trailing delimiter
    allow_trailing: bool,
    /// Phantom type to hold the output type of the delimiter parser.
    _marker: PhantomData<A>,
}

/// Creates a new `PDelim` parser combinator that parses `at_least` to `at_most`
/// occurrences of `p` separated by `delim` with or without a trailing delimiter.
///
/// # Arguments
///
/// * `p` - The parser for the main pattern to parse repeatedly.
/// * `delim` - The parser for the delimiter between main pattern occurrences.
///
/// # Returns
///
/// A `PDelim` instance which implements `ParserCore` and parses
/// sequences of `p` separated by `delim`, returning
/// a vector of parsed values.
pub fn pdelim<P1, P2, A>(
    p: P1,
    delim: P2,
    at_least: usize,
    at_most: usize,
    allow_trailing: bool,
) -> PDelim<P1, P2, A> {
    PDelim {
        p,
        delim,
        at_least,
        at_most,
        allow_trailing,
        _marker: PhantomData,
    }
}

/// [`PDelim`] specific methods to specialize the parser
impl<P, PDelim_, A> PDelim<P, PDelim_, A> {
    /// Do not allow trailing delimiters in parsing
    pub fn no_trailing(mut self) -> Self {
        self.allow_trailing = false;
        self
    }

    /// Force parser to find at least `n` occurances of `p`
    pub fn at_least(mut self, n: usize) -> Self {
        self.at_least = n;
        self
    }

    /// Restrict to at most `n` occurances of `p`
    pub fn at_most(mut self, n: usize) -> Self {
        self.at_most = n;
        self
    }
}

impl<'a, K, O, P, PDelim_, B> ParserCore<'a, K, Vec<O>> for PDelim<P, PDelim_, B>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    B: Clone + 'a,
    P: Parser<'a, K, O>,
    PDelim_: Parser<'a, K, B>,
{
    /// Parses `at_least` to `at_most` occurrences of `p` separated by `delim` from the input.
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
    /// * `Err(Error)` if no values could be parsed.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, Vec<O>>, Error<'a, K>> {
        let start = i.loc;

        let mut vals = vec![];
        let mut input = i;

        loop {
            let loop_start = input.loc;
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
                        Err(Error {
                            kind: _,
                            span,
                            state,
                        }) => {
                            // If we return here there is no trailing delim
                            if vals.len() >= self.at_least && vals.len() <= self.at_most {
                                // No more delimiters, parsing complete
                                return Ok(PSuccess {
                                    val: vals,
                                    rest: input,
                                });
                            } else {
                                return Err(Error {
                                    kind: vec![ErrorKind::Custom(format!(
                                        "Expected {} elements, but found {} elements.",
                                        if self.at_most == usize::MAX {
                                            format!("at least {}", self.at_least)
                                        } else {
                                            format!(
                                                "between {} and {}",
                                                self.at_least, self.at_most
                                            )
                                        },
                                        vals.len()
                                    ))],
                                    span: (start, span.1),
                                    state,
                                });
                            }
                        }
                    }
                }

                // if no token to read then return found tokens
                Err(Error {
                    kind: _,
                    span,
                    state,
                }) => {
                    if !self.allow_trailing && !vals.is_empty() {
                        return Err(Error {
                            kind: vec![ErrorKind::Custom("Trailing delimiter found.".to_string())],
                            span: (loop_start, state.loc),
                            state,
                        });
                    } else if vals.len() >= self.at_least && vals.len() <= self.at_most {
                        // No more delimiters, parsing complete
                        return Ok(PSuccess {
                            val: vals,
                            rest: input,
                        });
                    } else {
                        return Err(Error {
                            kind: vec![ErrorKind::Custom(format!(
                                "Expected {} elements, but found {} elements.",
                                if self.at_most == usize::MAX {
                                    format!("at least {}", self.at_least)
                                } else {
                                    format!("between {} and {}", self.at_least, self.at_most)
                                },
                                vals.len()
                            ))],
                            span: (start, span.1),
                            state,
                        });
                    }
                }
            }
        }
    }
}

impl<'a, K, O, P, PDelim_, B> Parser<'a, K, Vec<O>> for PDelim<P, PDelim_, B>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    B: Clone + 'a,
    P: Parser<'a, K, O>,
    PDelim_: Parser<'a, K, B>,
{
}
