use crate::parser::*;

pub struct PBind<P, O1, O2> {
    p: P,
    f: Arc<dyn Fn(O1) -> O2>,
}

pub fn pbind<P, F, O1, O2>(p: P, f: F) -> PBind<P, O1, O2>
where
    F: Fn(O1) -> O2 + 'static,
{
    PBind { p, f: Arc::new(f) }
}
impl<'a, P, K, O1, O2> ParserCore<'a, K, O2> for PBind<P, O1, O2>
where
    K: PartialEq + Copy + Clone + 'a,
    P: Parser<'a, K, O1>,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O2>, PFail<'a, K>> {
        let PSuccess { val, rest } = self.p.parse(i)?;

        Ok(PSuccess {
            val: (self.f)(val),
            rest,
        })
    }
}

impl<'a, P, K, O1, O2> Parser<'a, K, O2> for PBind<P, O1, O2>
where
    K: PartialEq + Copy + Clone + 'a,
    O1: 'a,
    O2: 'a,
    P: Parser<'a, K, O1>,
{
    fn then<O3, T>(self, p2: T) -> impl Parser<'a, K, (O2, O3)>
    where
        T: Parser<'a, K, O3>,
        O3: 'a,
    {
        pseq(self, p2)
    }

    fn or<T: Parser<'a, K, O2>>(self, p2: T) -> impl Parser<'a, K, O2> {
        por(self, p2)
    }

    fn map<O3, F>(self, f: F) -> impl Parser<'a, K, O3>
    where
        F: Fn(O2) -> O3 + 'static,
        O3: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, O2>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<O2>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<O2>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, A>(self, pad: Pad) -> impl Parser<'a, K, O2>
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
