use std::sync::Arc;

use crate::{
    error::Error,
    prelude::{PInput, PSuccess, Parser, ParserCore},
};

/// Combinator to map an error into a more language specific error.
///
/// ## Example
///
/// ```rust
/// use cypress::prelude::*;
///
/// let input = "1 + 2 + a + 3";
///
/// let parser = pnum()
///     .delimited_by(just('+').padded_by(pws()))
///     .until_end()
///     .map_error(|Error { kind, span, state }| Error {
///         kind: vec![ErrorKind::Custom("Custom error".to_string())],
///         span,
///         state,
///     });
///
/// match parser.parse(input.into_input()) {
///     Err(Error { kind, span, state }) => {
///         assert_eq!(*kind.first().unwrap(), ErrorKind::Custom("Custom error".to_string()))
///     },
///     Ok(_) => assert!(false)
/// };
/// ```
#[derive(Clone)]
pub struct PMapError<'a, K, P>
where
    K: PartialEq + Clone + 'a,
{
    parser: P,
    f: Arc<dyn Fn(Error<'a, K>) -> Error<'a, K>>,
}

/// Function to easily build new [`PMapError`] though see [`crate::parser::core::Parser::map_error`]
/// to use as combinator with iterator style chaining.
///
/// # Parameters
/// - `parser`: the parser to apply
/// - `f`: The transformation to apply to parser error of `parser` if found
///
/// # Returns the result of `parser` if successful or the mapped error if `parser` fails
pub fn pmap_error<'a, K, F, P>(parser: P, f: F) -> PMapError<'a, K, P>
where
    K: PartialEq + Clone + 'a,
    F: Fn(Error<'a, K>) -> Error<'a, K> + 'static,
{
    PMapError {
        parser,
        f: Arc::new(f),
    }
}

impl<'a, K, P, O> ParserCore<'a, K, O> for PMapError<'a, K, P>
where
    K: PartialEq + Clone + 'a,
    O: Clone + 'a,
    P: Parser<'a, K, O>,
{
    /// Applies the inner parser, if it fails it transforms the error using the function `f`.
    ///
    /// # Errors
    /// Returns a mapped parse failure with `f` if the inner parser `parser` fails.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, Error<'a, K>> {
        match self.parser.parse(i) {
            Ok(psuccess) => Ok(psuccess),
            Err(perror) => Err((self.f)(perror)),
        }
    }
}

impl<'a, K, P, O> Parser<'a, K, O> for PMapError<'a, K, P>
where
    K: PartialEq + Clone + 'a,
    P: Parser<'a, K, O>,
    O: Clone + 'a,
{
}
