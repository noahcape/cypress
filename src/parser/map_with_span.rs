use std::sync::Arc;

use crate::prelude::{Error, PInput, PSuccess, Parser, ParserCore, Span};

/// Combinator to map result of parser with the span successfully parsed with
/// the parser.
///
/// Useful for wrapping parsed tokens with their span for better error messages
/// after parsing in evaluation or validation.
///
/// ## Example
///
/// ```rust
/// use cypress::prelude::*;
///
/// #[derive(Clone, PartialEq, Debug)]
/// struct SpannedToken(String, Span);
///
/// let input = "\"this is a string\"";
///
/// let parser = just('\"').ignore_then(pletter().or(pws()).many().map(|cs| String::from_utf8(cs).unwrap()).map_with_span(|str_, span| SpannedToken(str_, span))).then_ignore(just('\"'));
///
/// match parser.parse(input.into_input()) {
///     Ok(PSuccess { val, rest: _ }) => assert_eq!(val, SpannedToken("this is a string".to_string(), (1, 17))),
///     Err(_) => assert!(false),
/// }
/// ```
#[derive(Clone)]
pub struct PMapWithSpan<P, O1, O2> {
    parser: P,
    f: Arc<dyn Fn(O1, Span) -> O2>,
}

/// Function to easily build new [`PMapWithSpan`] though see [`crate::parser::core::Parser::map_with_span`]
/// for easiest usage as a combinator.
///
/// # Parameters
/// - `parser`: The parser to apply
/// - `f`: The transformation function that converts the parser’s result with the span it parsed
///
/// # Returns
/// A new parser that parses with `p` and transforms its result using `f`.
pub fn pmap_with_span<P, F, O1, O2>(parser: P, f: F) -> PMapWithSpan<P, O1, O2>
where
    F: Fn(O1, Span) -> O2 + 'static,
{
    PMapWithSpan {
        parser,
        f: Arc::new(f),
    }
}

impl<'a, K, P, O1, O2> ParserCore<'a, K, O2> for PMapWithSpan<P, O1, O2>
where
    K: PartialEq + Clone + 'a,
    O1: Clone + 'a,
    P: Parser<'a, K, O1>,
{
    /// Applies the inner parser, then transforms its result along with its span using the function `f`.
    ///
    /// If parsing succeeds, the transformation function is applied to the result and its span,
    /// and the transformed value is returned.
    ///
    /// # Errors
    /// Returns a parse failure if the inner parser `p` fails.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O2>, Error<'a, K>> {
        let start = i.loc;

        let PSuccess { val, rest } = self.parser.parse(i)?;

        let span = (start, rest.loc);

        Ok(PSuccess {
            val: (self.f)(val, span),
            rest,
        })
    }
}

impl<'a, K, P, O1, O2> Parser<'a, K, O2> for PMapWithSpan<P, O1, O2>
where
    K: PartialEq + Clone + 'a,
    O1: Clone + 'a,
    O2: Clone + 'a,
    P: Parser<'a, K, O1>,
{
}
