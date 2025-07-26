use crate::parser::*;

pub struct PIdent<'a> {
    ident: &'a str,
}

pub fn pident<'a>(ident: &'a str) -> PIdent<'a> {
    PIdent { ident }
}

impl<'a, K> ParserCore<'a, K, String> for PIdent<'a>
where
    K: PartialEq + Copy + Clone + Into<char> + 'a,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, String>, PFail<'a, K>> {
        let ident_len = self.ident.len();
        let potential_match = i.tokens[i.loc..i.loc + ident_len]
            .iter()
            .map(|t| Into::<char>::into(*t))
            .collect::<String>();

        if self.ident.eq(&potential_match) {
            Ok(PSuccess {
                val: potential_match,
                rest: PInput {
                    tokens: i.tokens,
                    loc: i.loc + ident_len,
                },
            })
        } else {
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
