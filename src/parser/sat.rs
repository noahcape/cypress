use crate::parser::*;

#[derive(Clone)]
pub struct PSat<K: PartialEq> {
    test: Arc<dyn Fn(K) -> bool>,
    condition: String,
}

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
