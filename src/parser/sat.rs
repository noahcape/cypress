use crate::{
    error::{Error, ErrorKind},
    parser::*,
};
use std::sync::Arc;

/// A parser that succeeds if the next input token satisfies a predicate function.
///
/// `PSat` holds a test function (predicate) and a vector of tokens that could be
/// expected to find. It consumes exactly one token if the predicate passes.
///
/// # Type Parameters
///
/// * `K` - The type of tokens to parse. Must implement `PartialEq`.
#[derive(Clone)]
pub struct PSat<'a, K: PartialEq + Clone> {
    /// The predicate function to test a token.
    test: Arc<dyn Fn(&K) -> bool>,
    /// A description of the expected condition for error reporting.
    expected: Vec<TokenPattern<'a, K>>,
}

/// Creates a new `PSat` parser that succeeds when a token satisfies the given predicate.
///
/// # Arguments
///
/// * `test` - A function or closure that returns `true` if the token is accepted.
/// * `expected` - A vector of `TokenPattern`s that could have been expected.
///
/// # Returns
///
/// A `PSat` parser instance that parses one token satisfying `test`.
pub fn psat<'a, K, F>(test: F, expected: Vec<TokenPattern<'a, K>>) -> PSat<'a, K>
where
    K: PartialEq + Clone,
    F: Fn(&K) -> bool + 'static,
{
    let func = move |input: &K| test(input);

    PSat {
        test: Arc::new(func),
        expected,
    }
}

impl<'a, K> ParserCore<'a, K, K> for PSat<'a, K>
where
    K: PartialEq + Clone + 'a,
{
    /// Attempts to parse a single token that satisfies the predicate.
    ///
    /// If the next token satisfies `test`, consumes it and returns success with the token.
    /// If the token does not satisfy `test`, returns failure with the condition message.
    /// If no tokens remain, returns failure indicating no token to read.
    ///
    /// # Arguments
    ///
    /// * `i` - The input parser state containing tokens and current location.
    ///
    /// # Returns
    ///
    /// * `Ok(PSuccess)` with the token and advanced input location if parsing succeeds.
    /// * `Err(Error)` with an error message and span if parsing fails.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, K>, Error<'a, K>> {
        match i.tokens.get(i.loc) {
            Some(tok) => {
                if (self.test)(tok) {
                    Ok(PSuccess {
                        val: i.tokens[i.loc].clone(),
                        rest: PInput {
                            tokens: i.tokens,
                            loc: i.loc + 1,
                        },
                    })
                } else {
                    Err(Error {
                        kind: vec![ErrorKind::Unexpected {
                            expected: self.expected.clone(),
                            found: TokenPattern::Token(std::borrow::Cow::Borrowed(tok)),
                        }],
                        span: (i.loc, i.loc + 1),
                        state: PInput {
                            tokens: i.tokens,
                            loc: i.loc + 1,
                        },
                    })
                }
            }
            None => Err(Error {
                kind: vec![ErrorKind::Unexpected {
                    expected: self.expected.clone(),
                    found: TokenPattern::String(std::borrow::Cow::Borrowed("No token to read")),
                }],
                span: (i.loc, i.loc),
                state: PInput {
                    tokens: i.tokens,
                    loc: i.loc + 1,
                },
            }),
        }
    }
}

impl<'a, K> Parser<'a, K, K> for PSat<'a, K> where K: PartialEq + Clone + 'a {}
