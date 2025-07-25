use crate::parser::*;

type ParserRef<'a, K, O> = Rc<RefCell<Option<Box<dyn ParserCore<'a, K, O> + 'a>>>>;

pub struct PRecursive<'a, K, O> {
    parser: ParserRef<'a, K, O>,
}

impl<'a, K, O> Clone for PRecursive<'a, K, O> {
    fn clone(&self) -> Self {
        Self {
            parser: Rc::clone(&self.parser),
        }
    }
}

pub fn recursive<'a, K, O, F>(f: F) -> PRecursive<'a, K, O>
where
    K: PartialEq + Copy + 'a,
    O: 'a,
    F: FnOnce(PRecursive<'a, K, O>) -> Box<dyn ParserCore<'a, K, O> + 'a>,
{
    let cell = Rc::new(RefCell::new(None));
    let rec = PRecursive {
        parser: cell.clone(),
    };

    let real_parser = f(rec);
    *cell.borrow_mut() = Some(real_parser);

    PRecursive { parser: cell }
}

impl<'a, K, O> ParserCore<'a, K, O> for PRecursive<'a, K, O>
where
    K: PartialEq + Copy + 'a,
    O: 'a,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>> {
        self.parser
            .borrow()
            .as_ref()
            .expect("Recursive parser not initialized")
            .parse(i)
    }
}

impl<'a, K, O> Parser<'a, K, O> for PRecursive<'a, K, O>
where
    K: PartialEq + Copy + 'a,
    O: 'a,
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

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, O>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<O>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<O>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, O>
    where
        P: Parser<'a, K, A> + Clone,
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
