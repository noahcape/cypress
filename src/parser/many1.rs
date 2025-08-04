use crate::{
    error::{Error, ErrorKind},
    parser::*,
};

/// A parser combinator that applies the inner parser `p` one or more times,
/// collecting all successful parse results into a vector.
///
/// This combinator repeatedly applies the parser `p` until it fails,
/// and returns a vector of all successfully parsed values.
///
/// # Type Parameters
///
/// * `P` - The parser type to apply repeatedly.
#[derive(Clone)]
pub struct PMany1<P> {
    /// The inner parser to apply repeatedly.
    p: P,
}

/// Creates a new `PMany1` parser combinator that applies parser `p` one or more times.
/// Use [`crate::parser::many::pmany`] for applying zero or more times.
///
/// # Arguments
///
/// * `p` - The parser to apply repeatedly.
///
/// # Returns
///
/// A `PMany1` instance implementing `ParserCore` that collects multiple parse results
/// into a `Vec`.
pub fn pmany1<P>(p: P) -> PMany1<P> {
    PMany1 { p }
}

impl<'a, K, O, P> ParserCore<'a, K, Vec<O>> for PMany1<P>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    P: Parser<'a, K, O>,
{
    /// Parses one or more occurrences of the inner parser `p`.
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
    /// Always returns `Ok(PSuccess)` with a vector of all parsed values and the
    /// remaining input.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, Vec<O>>, Error<'a, K>> {
        let mut vals: Vec<O> = vec![];
        let mut input = i;

        // Repeatedly parse input with `p` until it fails
        while let Ok(PSuccess { val, rest }) = self.p.parse(input.clone()) {
            vals.push(val);
            input = rest;
        }

        if vals.is_empty() {
            Err(Error {
                kind: vec![ErrorKind::Custom(
                    "Expected at least one instance for `many1` but found none.",
                )],
                span: (input.loc, input.loc + 1),
                state: input,
            })
        } else {
            // Return all successfully parsed values and the remaining input
            Ok(PSuccess {
                val: vals,
                rest: input,
            })
        }
    }
}

impl<'a, K, O, P> Parser<'a, K, Vec<O>> for PMany1<P>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    P: Parser<'a, K, O>,
{
}
