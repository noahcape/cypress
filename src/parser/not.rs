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

impl<'a, K, I, O, P> ParserCore<'a, K, I, ()> for PNot<P, O>
where
    K: PartialEq + Copy + Clone,
    I: Iterator<Item = K> + Clone,
    P: Parser<'a, K, I, O>,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, ()>, PFail<K, I>> {
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

impl<'a, K, I, O, P> Parser<'a, K, I, ()> for PNot<P, O>
where
    K: PartialEq + Copy + Clone + 'a,
    I: Iterator<Item = K> + Clone + 'a,
    P: Parser<'a, K, I, O>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, I, ((), O2)>
    where
        T: Parser<'a, K, I, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, I, ()>
    where
        T: Parser<'a, K, I, ()>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, I, O2>
    where
        F: Fn(()) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, ()>
    where
        P1: Parser<'a, K, I, A>,
        P2: Parser<'a, K, I, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<()>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, I, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, I, Vec<()>>
    where
        PD: Parser<'a, K, I, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, A>(self, pad: Pad) -> impl Parser<'a, K, I, ()>
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
