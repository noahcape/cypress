use crate::parser::*;

pub struct PPaddedBy<P, PD, A> {
    p: P,
    pad: PD,
    _marker: PhantomData<A>,
}

pub fn ppadded<P, PD, A>(p: P, pad: PD) -> PPaddedBy<P, PD, A> {
    PPaddedBy {
        p,
        pad,
        _marker: PhantomData,
    }
}

impl<'a, K, I, O, P, PD, A> ParserCore<'a, K, I, O> for PPaddedBy<P, PD, A>
where
    K: PartialEq + Copy + 'a,
    I: Iterator<Item = K> + Clone + 'a,
    O: 'a,
    P: Parser<'a, K, I, O>,
    PD: Parser<'a, K, I, A> + Clone,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, O>, PFail<K, I>> {
        match self.pad.clone().many().parse(i) {
            Ok(PSuccess { val: _, rest }) => match self.p.parse(rest) {
                Ok(PSuccess { val, rest }) => match self.pad.clone().many().parse(rest) {
                    Ok(PSuccess { val: _, rest }) => Ok(PSuccess { val, rest }),
                    Err(pfail) => Err(pfail),
                },
                Err(pfail) => Err(pfail),
            },
            Err(pfail) => Err(pfail),
        }
    }
}

impl<'a, K, I, O, P, PD, B> Parser<'a, K, I, O> for PPaddedBy<P, PD, B>
where
    K: PartialEq + Copy + 'a,
    I: Iterator<Item = K> + Clone + 'a,
    O: 'a,
    P: Parser<'a, K, I, O>,
    PD: Parser<'a, K, I, B> + Clone,
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

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, O>
    where
        P1: Parser<'a, K, I, A>,
        P2: Parser<'a, K, I, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<O>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, I, ()> {
        pnot(self)
    }

    fn delimited_by<PD_, A>(self, delim: PD_) -> impl Parser<'a, K, I, Vec<O>>
    where
        PD_: Parser<'a, K, I, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, A>(self, pad: Pad) -> impl Parser<'a, K, I, O>
    where
        Pad: Parser<'a, K, I, A> + Clone,
    {
        ppadded(self, pad)
    }
}
