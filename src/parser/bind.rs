use crate::parser::*;
use std::sync::Arc;

/// A parser combinator that applies a transformation function to the result of a parser.
///
/// This is useful for mapping parsed values into a new representation,
/// such as converting a parsed string into an integer, or wrapping a result in a custom enum.
///
/// # Type Parameters
/// - `P`: The inner parser
/// - `O1`: The output type of the inner parser `P`
/// - `O2`: The output type after applying the transformation function
pub struct PBind<P, O1, O2> {
    p: P,
    f: Arc<dyn Fn(O1) -> O2>,
}

/// Constructs a `PBind` parser that applies a function `f` to the result of parser `p`.
///
/// This is similar to the `map` or `bind` operation in functional programming.
///
/// # Parameters
/// - `p`: The parser to apply
/// - `f`: The transformation function that converts the parser’s result
///
/// # Returns
/// A new parser that parses with `p` and transforms its result using `f`.
///
/// # Example
/// ```rust
/// use parsec::prelude::*;
///
/// let input = b"A".into_input();
/// let parser = just('A').map(|_| 1);
///
/// match parser.parse(input) {
///     Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 1),
///     Err(_) => assert!(false),
/// }
/// ```
pub fn pbind<P, F, O1, O2>(p: P, f: F) -> PBind<P, O1, O2>
where
    F: Fn(O1) -> O2 + 'static,
{
    PBind { p, f: Arc::new(f) }
}

impl<'a, P, K, O1, O2> ParserCore<'a, K, O2> for PBind<P, O1, O2>
where
    K: PartialEq + Clone + 'a,
    O1: 'a,
    P: Parser<'a, K, O1>,
{
    /// Applies the inner parser, then transforms its result using the function `f`.
    ///
    /// If parsing succeeds, the transformation function is applied to the result,
    /// and the transformed value is returned.
    ///
    /// # Errors
    /// Returns a parse failure if the inner parser `p` fails.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O2>, PFail<'a, K>> {
        // Apply the inner parser
        let PSuccess { val, rest } = self.p.parse(i)?;

        // Transform the result and return
        Ok(PSuccess {
            val: (self.f)(val),
            rest,
        })
    }
}

impl<'a, P, K, O1, O2> Parser<'a, K, O2> for PBind<P, O1, O2>
where
    K: PartialEq + Clone + 'a,
    O1: 'a,
    O2: 'a,
    P: Parser<'a, K, O1>,
{
}
