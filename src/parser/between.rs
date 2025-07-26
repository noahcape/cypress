use crate::parser::*;

pub struct PBetween<L, P, R, A> {
    l: L,
    p: P,
    r: R,
    _marker: PhantomData<A>,
}

pub fn pbetween<L, P, R, A>(l: L, p: P, r: R) -> PBetween<L, P, R, A> {
    PBetween {
        l,
        p,
        r,
        _marker: PhantomData,
    }
}

impl<'a, K, O, L, P, R, A> ParserCore<'a, K, O> for PBetween<L, P, R, A>
where
    K: PartialEq + Copy + Clone + 'a,
    L: Parser<'a, K, A>,
    P: Parser<'a, K, O>,
    R: Parser<'a, K, A>,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>> {
        let PSuccess {
            val: _,
            rest: after_l,
        } = self.l.parse(i)?;

        let PSuccess {
            val,
            rest: after_main,
        } = self.p.parse(after_l)?;

        let PSuccess {
            val: _,
            rest: after_r,
        } = self.r.parse(after_main)?;

        Ok(PSuccess { val, rest: after_r })
    }
}

impl<'a, K, O, L, P, R, A> Parser<'a, K, O> for PBetween<L, P, R, A>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    L: Parser<'a, K, A>,
    P: Parser<'a, K, O>,
    R: Parser<'a, K, A>,
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

    fn between<B, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, O>
    where
        P1: Parser<'a, K, B>,
        P2: Parser<'a, K, B>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<O>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A_>(self, delim: PD) -> impl Parser<'a, K, Vec<O>>
    where
        PD: Parser<'a, K, A_>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, C>(self, pad: Pad) -> impl Parser<'a, K, O>
    where
        Pad: Parser<'a, K, C> + Clone,
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
}
