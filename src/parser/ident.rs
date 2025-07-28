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

        if i.tokens.len() < i.loc + ident_len {
            return Err(PFail {
                error: format!("Not enough tokens to match {}", self.ident),
                span: (i.loc, i.tokens.len()),
                rest: PInput {
                    tokens: i.tokens,
                    loc: i.loc,
                },
            });
        }

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

impl<'a, K> Parser<'a, K, String> for PIdent<'a> where K: PartialEq + Copy + Clone + Into<char> + 'a {}
