use crate::parser::*;
use std::sync::Arc;

/// A parser that succeeds if the next input token satisfies a predicate function.
///
/// `PSat` holds a test function (predicate) and a condition string describing
/// the expected condition. It consumes exactly one token if the predicate passes.
///
/// # Type Parameters
///
/// * `K` - The type of tokens to parse. Must implement `PartialEq`.
#[derive(Clone)]
pub struct PSat<K: PartialEq> {
    /// The predicate function to test a token.
    test: Arc<dyn Fn(K) -> bool>,
    /// A description of the expected condition for error reporting.
    condition: String,
}

/// Creates a new `PSat` parser that succeeds when a token satisfies the given predicate.
///
/// # Arguments
///
/// * `test` - A function or closure that returns `true` if the token is accepted.
/// * `condition` - A string description of the expected token (used in error messages).
///
/// # Returns
///
/// A `PSat` parser instance that parses one token satisfying `test`.
pub fn psat<K, F>(test: F, condition: impl Into<String>) -> PSat<K>
where
    K: PartialEq,
    F: Fn(K) -> bool + 'static,
{
    let func = move |input: K| test(input);

    PSat {
        test: Arc::new(func),
        condition: condition.into(),
    }
}

impl<'a, K> ParserCore<'a, K, K> for PSat<K>
where
    K: PartialEq + Copy + Clone + 'a,
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
    /// * `Err(PFail)` with an error message and span if parsing fails.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, K>, PFail<'a, K>> {
        match i.tokens.get(i.loc) {
            Some(tok) => {
                if (self.test)(*tok) {
                    Ok(PSuccess {
                        val: *tok,
                        rest: PInput {
                            tokens: i.tokens,
                            loc: i.loc + 1,
                        },
                    })
                } else {
                    Err(PFail {
                        error: self.condition.clone(),
                        span: (i.loc, i.loc + 1),
                        rest: PInput {
                            tokens: i.tokens,
                            loc: i.loc + 1,
                        },
                    })
                }
            }
            None => Err(PFail {
                error: "No token to read".to_string(),
                span: (i.loc, i.loc),
                rest: PInput {
                    tokens: i.tokens,
                    loc: i.loc + 1,
                },
            }),
        }
    }
}

impl<'a, K> Parser<'a, K, K> for PSat<K>
where
    K: PartialEq + Copy + 'a,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (K, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, K>
    where
        T: Parser<'a, K, K>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(K) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, K>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<K>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<K>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, K>
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

    fn debug(self, label: &'static str) -> impl Parser<'a, K, K> {
        debug(self, label)
    }

    fn and<P2, A>(self, second: P2) -> impl Parser<'a, K, K>
    where
        P2: Parser<'a, K, A>,
    {
        pand(self, second)
    }
}
