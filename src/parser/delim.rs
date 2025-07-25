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

impl<'a, K, O, P, PDelim_, B> ParserCore<'a, K, Vec<O>> for PDelim<P, PDelim_, B>
where
    K: PartialEq + Copy + Clone + 'a,
    P: Parser<'a, K, O>,
    PDelim_: Parser<'a, K, B>,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, Vec<O>>, PFail<'a, K>> {
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

impl<'a, K, O, P, PDelim_, B> Parser<'a, K, Vec<O>> for PDelim<P, PDelim_, B>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P: Parser<'a, K, O>,
    PDelim_: Parser<'a, K, B>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (Vec<O>, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, Vec<O>>
    where
        T: Parser<'a, K, Vec<O>>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(Vec<O>) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, Vec<O>>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<Vec<O>>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<Vec<O>>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, A>(self, pad: Pad) -> impl Parser<'a, K, Vec<O>>
    where
        Pad: Parser<'a, K, A> + Clone,
    {
        ppadded(self, pad)
    }

    fn into_<Out>(self, out: Out) -> impl Parser<'a, K, Out>
    where
        Out: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }
}
