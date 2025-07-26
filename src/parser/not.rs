use crate::parser::*;

pub struct PNot<P, O> {
    p: P,
    _marker: PhantomData<O>,
}

pub fn pnot<P, O>(p: P) -> PNot<P, O> {
    PNot {
        p,
        _marker: PhantomData,
    }
}

impl<'a, K, O, P> ParserCore<'a, K, ()> for PNot<P, O>
where
    K: PartialEq + Copy + Clone + 'a,
    P: Parser<'a, K, O>,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, ()>, PFail<'a, K>> {
        let start = i.loc;
        match self.p.parse(i) {
            Ok(PSuccess { val: _, rest }) => Err(PFail {
                error: "Was what was not expected.".to_string(),
                span: (start, rest.loc),
                rest,
            }),
            Err(PFail {
                error: _,
                span: _,
                rest,
            }) => Ok(PSuccess { val: (), rest }),
        }
    }
}

impl<'a, K, O, P> Parser<'a, K, ()> for PNot<P, O>
where
    K: PartialEq + Copy + Clone + 'a,
    P: Parser<'a, K, O>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, ((), O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, ()>
    where
        T: Parser<'a, K, ()>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(()) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, ()>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<()>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<()>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, A>(self, pad: Pad) -> impl Parser<'a, K, ()>
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

    fn debug(self, label: &'static str) -> impl Parser<'a, K, ()> {
        debug(self, label)
    }

    fn and<P2, A>(self, second: P2) -> impl Parser<'a, K, ()>
    where
        P2: Parser<'a, K, A>,
    {
        pand(self, second)
    }
}
