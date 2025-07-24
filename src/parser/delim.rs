use crate::parser::*;

pub struct PDelim<P1, P2, A> {
    p: P1,
    delim: P2,
    _marker: PhantomData<A>,
}

pub fn pdelim<P1, P2, A>(p: P1, delim: P2) -> PDelim<P1, P2, A> {
    PDelim {
        p,
        delim,
        _marker: PhantomData,
    }
}

impl<'a, K, I, O, P, PDelim_, B> ParserCore<'a, K, I, Vec<O>> for PDelim<P, PDelim_, B>
where
    K: PartialEq + Copy + Clone,
    I: Iterator<Item = K> + Clone,
    P: Parser<'a, K, I, O>,
    PDelim_: Parser<'a, K, I, B>,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, Vec<O>>, PFail<K, I>> {
        let mut vals = vec![];
        let mut input = i;

        loop {
            match self.p.parse(input.clone()) {
                Ok(PSuccess { val, rest }) => {
                    vals.push(val);
                    input = rest;
                    match self.delim.parse(input.clone()) {
                        Ok(PSuccess { val: _, rest }) => {
                            input = rest;
                            continue;
                        }
                        Err(_) => {
                            return Ok(PSuccess {
                                val: vals,
                                rest: input,
                            });
                        }
                    }
                }
                Err(PFail { error, span, rest }) => {
                    if !vals.is_empty() {
                        return Ok(PSuccess { val: vals, rest });
                    } else {
                        return Err(PFail { error, span, rest });
                    }
                }
            }
        }
    }
}

impl<'a, K, I, O, P, PDelim_, B> Parser<'a, K, I, Vec<O>> for PDelim<P, PDelim_, B>
where
    K: PartialEq + Copy + Clone + 'a,
    I: Iterator<Item = K> + Clone + 'a,
    O: 'a,
    P: Parser<'a, K, I, O>,
    PDelim_: Parser<'a, K, I, B>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, I, (Vec<O>, O2)>
    where
        T: Parser<'a, K, I, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, I, Vec<O>>
    where
        T: Parser<'a, K, I, Vec<O>>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, I, O2>
    where
        F: Fn(Vec<O>) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, Vec<O>>
    where
        P1: Parser<'a, K, I, A>,
        P2: Parser<'a, K, I, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<Vec<O>>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, I, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, I, Vec<Vec<O>>>
    where
        PD: Parser<'a, K, I, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, A>(self, pad: Pad) -> impl Parser<'a, K, I, Vec<O>>
    where
        Pad: Parser<'a, K, I, A> + Clone,
    {
        ppadded(self, pad)
    }

    fn into_<Out>(self, out: Out) -> impl Parser<'a, K, I, Out>
    where
        Out: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }
}
