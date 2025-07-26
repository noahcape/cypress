use crate::parser::*;

/// A parser that matches a fixed identifier string in the input.
///
/// `PIdent` attempts to parse a specific string slice (`ident`) from the input tokens.
/// The input tokens are converted into characters to compare with the target identifier.
///
/// # Lifetime
///
/// * `'a` - Lifetime of the input string slice `ident`.
pub struct PIdent<'a> {
    /// The string slice representing the identifier to match.
    ident: &'a str,
}

/// Creates a new `PIdent` parser that matches the exact string `ident`.
///
/// # Arguments
///
/// * `ident` - The identifier string to match against the input.
///
/// # Returns
///
/// A `PIdent` instance that implements `ParserCore` and attempts to match
/// the given identifier string from the input tokens.
pub fn pident<'a>(ident: &'a str) -> PIdent<'a> {
    PIdent { ident }
}

impl<'a, K> ParserCore<'a, K, String> for PIdent<'a>
where
    K: PartialEq + Copy + Clone + Into<char> + 'a,
{
    /// Attempts to parse the identifier string from the input.
    ///
    /// This method extracts a slice from the input tokens of length equal to the
    /// identifier's length, converts each token into a `char`, and collects them
    /// into a `String`. It then compares this string with the expected identifier.
    ///
    /// # Arguments
    ///
    /// * `i` - The parser input containing tokens and current location.
    ///
    /// # Returns
    ///
    /// * `Ok(PSuccess)` with the matched identifier string and updated input location
    ///   if the input matches the identifier exactly.
    /// * `Err(PFail)` containing an error message, the span of the failed match,
    ///   and the input position after the attempted parse if the match fails.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, String>, PFail<'a, K>> {
        let ident_len = self.ident.len();

        // Extract a substring from input tokens matching the length of the identifier
        let potential_match = i.tokens[i.loc..i.loc + ident_len]
            .iter()
            .map(|t| Into::<char>::into(*t))
            .collect::<String>();

        if self.ident.eq(&potential_match) {
            // Successful parse: return matched string and updated location
            Ok(PSuccess {
                val: potential_match,
                rest: PInput {
                    tokens: i.tokens,
                    loc: i.loc + ident_len,
                },
            })
        } else {
            // Failed parse: return error message, span, and updated input location
            Err(PFail {
                error: format!("Expected {} found {}", self.ident, potential_match),
                span: (i.loc, i.loc + ident_len),
                rest: PInput {
                    tokens: i.tokens,
                    loc: i.loc + ident_len,
                },
            })
        }
    }
}

impl<'a, K> Parser<'a, K, String> for PIdent<'a>
where
    K: PartialEq + Copy + Clone + Into<char> + 'a,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (String, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, String>
    where
        T: Parser<'a, K, String>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(String) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, String>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<String>> {
        pmany(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<String>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, String>
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

    fn debug(self, label: &'static str) -> impl Parser<'a, K, String> {
        debug(self, label)
    }

    fn and<P2, A>(self, second: P2) -> impl Parser<'a, K, String>
    where
        P2: Parser<'a, K, A>,
    {
        pand(self, second)
    }
}
