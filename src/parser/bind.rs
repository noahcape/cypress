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
impl<'a, P, K, I, O1, O2> ParserCore<'a, K, I, O2> for PBind<P, O1, O2>
where
    K: PartialEq + Copy + Clone,
    I: Iterator<Item = K> + Clone,
    P: Parser<'a, K, I, O1>,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, O2>, PFail<K, I>> {
        match self.p.parse(i) {
            Ok(PSuccess { val, rest }) => Ok(PSuccess {
                val: (self.f)(val),
                rest,
            }),
            Err(pfail) => Err(pfail),
        }
    }
}

impl<'a, P, K, I, O1, O2> Parser<'a, K, I, O2> for PBind<P, O1, O2>
where
    K: PartialEq + Copy + Clone + 'a,
    I: Iterator<Item = K> + Clone + 'a,
    O1: 'a,
    O2: 'a,
    P: Parser<'a, K, I, O1>,
{
    fn then<O3, T>(self, p2: T) -> impl Parser<'a, K, I, (O2, O3)>
    where
        T: Parser<'a, K, I, O3>,
        O3: 'a,
    {
        pseq(self, p2)
    }

    fn or<T: Parser<'a, K, I, O2>>(self, p2: T) -> impl Parser<'a, K, I, O2> {
        por(self, p2)
    }

    fn map<O3, F>(self, f: F) -> impl Parser<'a, K, I, O3>
    where
        F: Fn(O2) -> O3 + 'static,
        O3: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, O2>
    where
        P1: Parser<'a, K, I, A>,
        P2: Parser<'a, K, I, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<O2>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, I, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, I, Vec<O2>>
    where
        PD: Parser<'a, K, I, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, A>(self, pad: Pad) -> impl Parser<'a, K, I, O2>
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
