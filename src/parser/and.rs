use crate::parser::*;

pub struct PAnd<P1, P2, A> {
    first: P1,
    second: P2,
    _marker: PhantomData<A>,
}

pub fn pand<P1, P2, A>(first: P1, second: P2) -> PAnd<P1, P2, A> {
    PAnd {
        first,
        second,
        _marker: PhantomData,
    }
}

impl<'a, K, O, P1, P2, A> ParserCore<'a, K, O> for PAnd<P1, P2, A>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P1: Parser<'a, K, O>,
    P2: Parser<'a, K, A>,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>> {
        let loc = i.loc;
        let tokens = i.tokens;

        let PSuccess { val, rest } = self.first.parse(i)?;
        let PSuccess { val: _, rest: _ } = self.second.parse(PInput { tokens, loc })?;

        Ok(PSuccess { val, rest })
    }
}

impl<'a, K, O, P1, P2, A> Parser<'a, K, O> for PAnd<P1, P2, A>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P1: Parser<'a, K, O>,
    P2: Parser<'a, K, A>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (O, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, O>
    where
        T: Parser<'a, K, O>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(O) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A_, P1_, P2_>(self, l: P1_, r: P2_) -> impl Parser<'a, K, O>
    where
        P1_: Parser<'a, K, A_>,
        P2_: Parser<'a, K, A_>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<O>> {
        pmany(self)
    }

    fn delimited_by<PD, A_>(self, delim: PD) -> impl Parser<'a, K, Vec<O>>
    where
        PD: Parser<'a, K, A_>,
    {
        pdelim(self, delim)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn padded_by<P, A_>(self, pad: P) -> impl Parser<'a, K, O>
    where
        P: Parser<'a, K, A_> + Clone,
    {
        ppadded(self, pad)
    }

    fn into_<Out>(self, out: Out) -> impl Parser<'a, K, Out>
    where
        Out: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }

    fn debug(self, label: &'static str) -> impl Parser<'a, K, O> {
        debug(self, label)
    }

    fn and<P2_, A_>(self, second: P2_) -> impl Parser<'a, K, O>
    where
        P2_: Parser<'a, K, A_>,
    {
        pand(self, second)
    }
}
