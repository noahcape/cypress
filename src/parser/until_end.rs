use std::{borrow::Cow, marker::PhantomData};

use crate::{
    error::{Error, ErrorKind, TokenPattern},
    prelude::{PInput, PSuccess, Parser, ParserCore},
};

/// A parser combinator that ensures the wrapped parser consumes all input.
///
/// If the inner parser succeeds but does **not** reach the end of input,
/// `PUntilEnd` will return an `Unexpected` error indicating unexpected trailing tokens.
///
/// This is useful for validating complete expressions, configurations, or files.
///
/// # Example
/// ```rust
/// use cypress::prelude::*;
///
/// // A parser that accepts only the identifier "hello"
/// let parser = pident("hello").until_end();
///
/// // Successful parse when input exactly matches
/// let input = "hello".into_input();
/// let result = parser.parse(input);
/// assert!(result.is_ok());
///
/// // Failure when extra input follows
/// let bad_input = "hello world".into_input();
/// let bad_result = parser.parse(bad_input);
/// assert!(bad_result.is_err());
/// ```
#[derive(Clone)]
pub struct PUntilEnd<P, O> {
    inner: P,
    _marker: PhantomData<O>,
}

/// Constructs a [`PUntilEnd`] parser that requires its inner parser to consume all input.
///
/// See [`PUntilEnd`] for more details.
pub fn puntil_end<P, O>(inner: P) -> PUntilEnd<P, O> {
    PUntilEnd {
        inner,
        _marker: PhantomData,
    }
}

impl<'a, P, K, O> ParserCore<'a, K, O> for PUntilEnd<P, O>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    P: Parser<'a, K, O>,
{
    /// Parses in put with `inner` and succeeds only if `inner` succeeds
    /// and it consumes all input.
    ///
    ///
    /// # Arguments
    ///
    /// * `i` - The parser input.
    ///
    /// # Returns
    ///
    /// * `Ok(PSuccess)` with the result of `inner`
    /// * `Err(Error)` either if the parser or it does not consume all input.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, Error<'a, K>> {
        let PSuccess { val, rest } = self.inner.parse(i)?;

        if rest.loc == rest.tokens.len() {
            Ok(PSuccess { val, rest })
        } else {
            Err(Error {
                kind: vec![ErrorKind::Unexpected {
                    expected: vec![TokenPattern::String(Cow::Borrowed("End of file"))],
                    found: TokenPattern::Tokens(Cow::Borrowed(&rest.tokens[rest.loc..])),
                }],
                span: (rest.loc, rest.loc + 1),
                state: rest,
            })
        }
    }
}

impl<'a, P, K, O> Parser<'a, K, O> for PUntilEnd<P, O>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    P: Parser<'a, K, O>,
{
}
