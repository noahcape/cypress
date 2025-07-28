use crate::parser::*;

/// A parser combinator that tries to parse input with the first parser `p1`.
/// If `p1` fails, it tries the second parser `p2`.
///
/// This combinator represents an "OR" choice between two parsers:
/// it succeeds if either `p1` or `p2` succeeds, trying `p1` first.
///
/// # Type Parameters
///
/// * `P1` - The first parser type.
/// * `P2` - The second parser type.
pub struct POr<P1, P2> {
    /// The first parser to try.
    p1: P1,
    /// The second parser to try if `p1` fails.
    p2: P2,
}

/// Creates a new `POr` parser combinator that tries `p1`, and if it fails, tries `p2`.
///
/// # Arguments
///
/// * `p1` - The first parser to attempt.
/// * `p2` - The second parser to attempt if `p1` fails.
///
/// # Returns
///
/// A `POr` instance implementing `ParserCore` that tries `p1` and then `p2`.
pub fn por<P1, P2>(p1: P1, p2: P2) -> POr<P1, P2> {
    POr { p1, p2 }
}

/// A convenient macro to create a choice parser from two or more parsers.
///
/// This macro recursively combines multiple parsers into nested `por` combinators,
/// allowing you to write `choice!(p1, p2, p3, ...)` instead of nested calls.
///
/// # Examples
///
/// ```
/// use parsec::prelude::*;
/// let input = b"B".into_input();
/// let parser = just('A').or(just('B'));
///
/// match parser.parse(input) {
///     Ok(PSuccess { val, rest: _ }) => assert_eq!(val, b'B'),
///     Err(_) => assert!(false),
/// }
/// ```
///
/// This is equivalent to `por(p1, por(p2, p3))`.
#[macro_export]
macro_rules! choice {
    ($p:expr, $q:expr $(,)?) => {
        $crate::parser::or::por($p, $q)
    };
    ($p:expr, $( $rest:expr ),* $(,)?) => {
        $crate::parser::or::por($p, $crate::choice!($($rest),*))
    };
}

impl<'a, K, O, P1, P2> ParserCore<'a, K, O> for POr<P1, P2>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P1: Parser<'a, K, O>,
    P2: Parser<'a, K, O>,
{
    /// Attempts to parse input using the first parser `p1`.
    ///
    /// If `p1` succeeds, returns its result immediately.
    /// If `p1` fails, attempts to parse input using the second parser `p2`.
    ///
    /// # Arguments
    ///
    /// * `i` - The input to parse.
    ///
    /// # Returns
    ///
    /// * `Ok(PSuccess)` if either `p1` or `p2` succeed.
    /// * `Err(PFail)` if both `p1` and `p2` fail.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>> {
        match self.p1.parse(i.clone()) {
            Ok(psuccess) => Ok(psuccess),
            Err(_) => self.p2.parse(i),
        }
    }
}

impl<'a, K, O, P1, P2> Parser<'a, K, O> for POr<P1, P2>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P1: Parser<'a, K, O>,
    P2: Parser<'a, K, O>,
{
}
