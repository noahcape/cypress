use crate::{
    error::{Error, ErrorDisplay},
    parser::*,
};

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
    K: PartialEq + Clone + ErrorDisplay + 'a,
    O: 'a,
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
    /// * `Err(Error)` if the inner parser fails, printing an error message with the label, error, and span.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, Error<'a, K>> {
        match self.inner.parse(i) {
            Ok(psuccess) => {
                println!("Successfully parsed with {}", self.label);
                Ok(psuccess)
            }
            Err(Error { kind, span, state }) => {
                println!(
                    "Failed {} with msg: {} at position span ({}, {})",
                    self.label,
                    kind.last().unwrap(),
                    span.0,
                    span.1
                );
                Err(Error { kind, span, state })
            }
        }
    }
}

impl<'a, K, O, Inner> Parser<'a, K, O> for PDebug<Inner>
where
    K: PartialEq + Clone + ErrorDisplay + 'a,
    O: 'a,
    Inner: Parser<'a, K, O>,
{
}
