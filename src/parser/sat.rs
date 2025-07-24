use crate::parser::*;

#[derive(Clone)]
pub struct PSat<K: PartialEq> {
    test: Arc<dyn Fn(K) -> bool>,
    condition: String,
}

pub fn psat<K, F>(test: F, condition: String) -> PSat<K>
where
    K: PartialEq + Display,
    F: Fn(K) -> bool + 'static,
{
    let func = move |input: K| test(input);

    PSat {
        test: Arc::new(func),
        condition,
    }
}

impl<'a, K, I> ParserCore<'a, K, I, K> for PSat<K>
where
    K: PartialEq + Copy,
    I: Iterator<Item = K> + Clone,
{
    fn parse(&self, mut i: PInput<K, I>) -> Result<PSuccess<K, I, K>, PFail<K, I>> {
        match i.tokens.next() {
            Some(tok) => {
                if (self.test)(tok) {
                    Ok(PSuccess {
                        val: tok,
                        rest: PInput {
                            tokens: i.tokens.clone(),
                            loc: i.loc + 1,
                        },
                    })
                } else {
                    Err(PFail {
                        error: self.condition.clone(),
                        span: (i.loc, i.loc + 1),
                        rest: PInput {
                            tokens: i.tokens.clone(),
                            loc: i.loc + 1,
                        },
                    })
                }
            }
            None => Err(PFail {
                error: "No token to read".to_string(),
                span: (i.loc, i.loc),
                rest: PInput {
                    tokens: i.tokens.clone(),
                    loc: i.loc + 1,
                },
            }),
        }
    }
}

impl<'a, K, I> Parser<'a, K, I, K> for PSat<K>
where
    K: PartialEq + Copy + 'a,
    I: Iterator<Item = K> + Clone + 'a,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, I, (K, O2)>
    where
        T: Parser<'a, K, I, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, I, K>
    where
        T: Parser<'a, K, I, K>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, I, O2>
    where
        F: Fn(K) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, K>
    where
        P1: Parser<'a, K, I, A>,
        P2: Parser<'a, K, I, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<K>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, I, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, I, Vec<K>>
    where
        PD: Parser<'a, K, I, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, I, K>
    where
        P: Parser<'a, K, I, A> + Clone,
    {
        ppadded(self, pad)
    }
}
