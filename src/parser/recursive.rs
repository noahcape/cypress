use crate::parser::*;

pub struct PRecursive<'a, K, I, O> {
    parser: Rc<RefCell<Option<Box<dyn ParserCore<'a, K, I, O> + 'a>>>>,
}

impl<'a, K, I, O> Clone for PRecursive<'a, K, I, O> {
    fn clone(&self) -> Self {
        Self {
            parser: Rc::clone(&self.parser),
        }
    }
}

pub fn recursive<'a, K, I, O, F>(f: F) -> PRecursive<'a, K, I, O>
where
    K: PartialEq + Copy + 'a,
    I: Iterator<Item = K> + Clone + 'a,
    O: 'a,
    F: FnOnce(PRecursive<'a, K, I, O>) -> Box<dyn ParserCore<'a, K, I, O> + 'a>,
{
    let cell = Rc::new(RefCell::new(None));
    let rec = PRecursive {
        parser: cell.clone(),
    };

    let real_parser = f(rec);
    *cell.borrow_mut() = Some(real_parser);

    PRecursive { parser: cell }
}

impl<'a, K, I, O> ParserCore<'a, K, I, O> for PRecursive<'a, K, I, O>
where
    K: PartialEq + Copy + 'a,
    I: Iterator<Item = K> + Clone + 'a,
    O: 'a,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, O>, PFail<K, I>> {
        self.parser
            .borrow()
            .as_ref()
            .expect("Recursive parser not initialized")
            .parse(i)
    }
}

impl<'a, K, I, O> Parser<'a, K, I, O> for PRecursive<'a, K, I, O>
where
    K: PartialEq + Copy + 'a,
    I: Iterator<Item = K> + Clone + 'a,
    O: 'a,
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

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, I, Vec<O>>
    where
        PD: Parser<'a, K, I, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, I, O>
    where
        P: Parser<'a, K, I, A> + Clone,
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
