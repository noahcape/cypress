use crate::{
    error::{Error, ErrorKind},
    parser::*,
};

/// A parser combinator that applies the inner parser `p` between
/// `at_least` and `at_most` times, collecting all successful parse
/// results into a vector.
///
/// This combinator repeatedly applies the parser `p` until it fails,
/// and returns a vector of all successfully parsed values.
///
/// # Type Parameters
///
/// * `P` - The parser type to apply repeatedly.
#[derive(Clone)]
pub struct PMany<P> {
    /// The inner parser to apply repeatedly.
    p: P,
    /// Fewest occurances of `p` for successful parsing
    at_least: usize,
    /// Max occurances of `p` for successful parsing
    at_most: usize,
}

/// Creates a new `PMany` parser combinator that applies parser `p` between
/// `at_least` and `at_most` times.
///
/// # Arguments
///
/// * `p` - The parser to apply repeatedly.
///
/// # Returns
///
/// A `PMany` instance implementing `ParserCore` that collects multiple parse results
/// into a `Vec`.
pub fn pmany<P>(p: P, at_least: usize, at_most: usize) -> PMany<P> {
    PMany {
        p,
        at_least,
        at_most,
    }
}

/// [`PMany`] specific methods to specialize the parser
impl<P> PMany<P> {
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

impl<'a, K, O, P> ParserCore<'a, K, Vec<O>> for PMany<P>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    P: Parser<'a, K, O>,
{
    /// Parses between `at_least` and `at_most` occurrences of the inner parser `p`.
    ///
    /// This method repeatedly attempts to parse the input using `p`, pushing
    /// each successful parse result into a vector. Parsing stops when `p` fails.
    ///
    /// # Arguments
    ///
    /// * `i` - The input to parse.
    ///
    /// # Returns
    ///
    /// Always returns `Ok(PSuccess)` with a vector of all parsed values (which
    /// may be empty) and the remaining input.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, Vec<O>>, Error<'a, K>> {
        let start = i.loc;

        let mut vals: Vec<O> = vec![];
        let mut input = i;

        // Repeatedly parse input with `p` until it fails
        while let Ok(PSuccess { val, rest }) = self.p.parse(input.clone()) {
            vals.push(val);
            input = rest;
        }

        if vals.len() >= self.at_least && vals.len() <= self.at_most {
            // Return all successfully parsed values and the remaining input
            Ok(PSuccess {
                val: vals,
                rest: input,
            })
        } else {
            Err(Error {
                kind: vec![ErrorKind::Custom(format!(
                    "Expected {} elements, but found {} elements.",
                    if self.at_most == usize::MAX {
                        format!("at least {}", self.at_least)
                    } else {
                        format!("between {} and {}", self.at_least, self.at_most)
                    },
                    vals.len()
                ))],
                span: (start, input.loc),
                state: input,
            })
        }
    }
}

impl<'a, K, O, P> Parser<'a, K, Vec<O>> for PMany<P>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    P: Parser<'a, K, O>,
{
}
