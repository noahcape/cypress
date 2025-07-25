use crate::parser::*;

pub struct PMany<P> {
    p: P,
}

pub fn pmany<P>(p: P) -> PMany<P> {
    PMany { p }
}

impl<'a, K, O, P> ParserCore<'a, K, Vec<O>> for PMany<P>
where
    K: PartialEq + Copy + Clone + 'a,
    P: Parser<'a, K, O>,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, Vec<O>>, PFail<'a, K>> {
        let mut vals: Vec<O> = vec![];
        let mut input = i;

        while let Ok(PSuccess { val, rest }) = self.p.parse(input.clone()) {
            vals.push(val);
            input = rest;
        }

        Ok(PSuccess {
            val: vals,
            rest: input,
        })
    }
}

impl<'a, K, O, P> Parser<'a, K, Vec<O>> for PMany<P>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P: Parser<'a, K, O>,
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
