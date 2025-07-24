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

impl<'a, K, I, O, L, P, R, A> ParserCore<'a, K, I, O> for PBetween<L, P, R, A>
where
    K: PartialEq + Copy + Clone,
    I: Iterator<Item = K> + Clone,
    L: Parser<'a, K, I, A>,
    P: Parser<'a, K, I, O>,
    R: Parser<'a, K, I, A>,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, O>, PFail<K, I>> {
        match self.l.parse(i) {
            Ok(PSuccess { val: _, rest }) => match self.p.parse(rest) {
                Ok(PSuccess { val, rest }) => match self.r.parse(rest) {
                    Ok(PSuccess { val: _, rest }) => Ok(PSuccess { val, rest }),
                    Err(pfail) => Err(pfail),
                },
                Err(pfail) => Err(pfail),
            },
            Err(pfail) => Err(pfail),
        }
    }
}

impl<'a, K, I, O, L, P, R, A> Parser<'a, K, I, O> for PBetween<L, P, R, A>
where
    K: PartialEq + Copy + Clone + 'a,
    I: Iterator<Item = K> + Clone + 'a,
    O: 'a,
    L: Parser<'a, K, I, A>,
    P: Parser<'a, K, I, O>,
    R: Parser<'a, K, I, A>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, I, (O, O2)>
    where
        T: Parser<'a, K, I, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, I, O>
    where
        T: Parser<'a, K, I, O>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, I, O2>
    where
        F: Fn(O) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<B, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, O>
    where
        P1: Parser<'a, K, I, B>,
        P2: Parser<'a, K, I, B>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<O>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, I, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A_>(self, delim: PD) -> impl Parser<'a, K, I, Vec<O>>
    where
        PD: Parser<'a, K, I, A_>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, C>(self, pad: Pad) -> impl Parser<'a, K, I, O>
    where
        Pad: Parser<'a, K, I, C> + Clone,
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
