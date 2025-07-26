use crate::parser::*;

/// A parser wrapper that adds debug printing to another parser.
///
/// `PDebug` wraps an inner parser and prints debugging information
/// about the success or failure of the parsing operation,
/// including a custom label provided at construction time.
pub struct PDebug<P> {
    /// The inner parser that does the actual parsing work.
    inner: P,
    /// A static string label used to identify the parser in debug output.
    label: &'static str,
}

/// Creates a new debug parser wrapping the given `inner` parser with the specified `label`.
///
/// # Arguments
///
/// * `inner` - The parser to wrap for debugging purposes.
/// * `label` - A static string label used for identifying the parser in debug messages.
///
/// # Returns
///
/// A `PDebug` instance that implements the `ParserCore` trait, which
/// will print debug information during parsing.
pub fn debug<P>(inner: P, label: &'static str) -> PDebug<P> {
    PDebug { inner, label }
}

impl<'a, K, O, P> ParserCore<'a, K, O> for PDebug<P>
where
    K: PartialEq + Copy + Clone + 'a,
    P: Parser<'a, K, O>,
{
    /// Attempts to parse input using the inner parser, printing debug information on success or failure.
    ///
    /// # Arguments
    ///
    /// * `i` - The input to parse.
    ///
    /// # Returns
    ///
    /// * `Ok(PSuccess)` if the inner parser succeeds, printing a success message with the label.
    /// * `Err(PFail)` if the inner parser fails, printing an error message with the label, error, and span.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>> {
        match self.inner.parse(i) {
            Ok(psuccess) => {
                println!("Successfully parsed with {}", self.label);
                Ok(psuccess)
            }
            Err(PFail { error, span, rest }) => {
                println!(
                    "Failed {}: with msg: {} at position span ({}, {})",
                    self.label, error, span.0, span.1
                );
                Err(PFail { error, span, rest })
            }
        }
    }
}

impl<'a, K, O, Inner> Parser<'a, K, O> for PDebug<Inner>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    Inner: Parser<'a, K, O>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (O, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, O>
    where
        T: Parser<'a, K, O>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(O) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, O>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<O>> {
        pmany(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<O>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, O>
    where
        P: Parser<'a, K, A> + Clone,
    {
        ppadded(self, pad)
    }

    fn into_<Out>(self, out: Out) -> impl Parser<'a, K, Out>
    where
        Out: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }

    fn debug(self, label: &'static str) -> impl Parser<'a, K, O> {
        debug(self, label)
    }

    fn and<P2, A>(self, second: P2) -> impl Parser<'a, K, O>
    where
        P2: Parser<'a, K, A>,
    {
        pand(self, second)
    }
}
