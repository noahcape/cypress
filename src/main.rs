type T<'a> = &'a [u8];
type I = u8;
type Input<'a> = T<'a>;
type Outcome<'a, O> = Result<(O, T<'a>), String>;

fn prepare(input: &'static str) -> Input<'static> {
    input.as_bytes()
}

fn peof<'a>() -> impl Fn(Input<'a>) -> Outcome<'a, bool> {
    |input: Input| {
        if input.is_empty() {
            Ok((true, &[]))
        } else {
            Err(String::from("Not end of file"))
        }
    }
}

fn pnot<'a>(v: u8) -> impl Fn(Input<'a>) -> Outcome<'a, I> {
    psat(Box::new(move |i: I| !i.eq(&v)), format!("is not {}", v))
}

fn pdigit<'a>() -> impl Fn(Input<'a>) -> Outcome<'a, I> {
    psat(Box::new(|i: I| i.is_ascii_digit()), "is digit".into())
}

fn pspace<'a>() -> impl Fn(Input<'a>) -> Outcome<'a, I> {
    psat(
        Box::new(|i: I| i.is_ascii_whitespace()),
        "is whitespace".into(),
    )
}

fn pletter<'a>() -> impl Fn(Input<'a>) -> Outcome<'a, I> {
    psat(Box::new(|i: I| i.is_ascii_alphabetic()), "is letter".into())
}

fn pchar<'a>(c: char) -> impl Fn(Input<'a>) -> Outcome<'a, I> {
    pitem(c as u8)
}

fn isupper<'a>() -> impl Fn(Input<'a>) -> Outcome<'a, I> {
    psat(
        Box::new(|i: I| i.is_ascii_uppercase()),
        "is uppercase".into(),
    )
}

fn islower<'a>() -> impl Fn(Input<'a>) -> Outcome<'a, I> {
    psat(
        Box::new(|i: I| i.is_ascii_lowercase()),
        "is lowercase".into(),
    )
}

fn pstring<'a>(string: &str) -> impl Fn(Input<'a>) -> Outcome<'a, String> {
    let items = string.as_bytes();

    pbind(pitems(items), |xs: Vec<_>| {
        String::from_utf8(xs.to_vec()).unwrap()
    })
}

fn ppadded<'a, A>(p: impl Fn(Input<'a>) -> Outcome<'a, A>) -> impl Fn(Input<'a>) -> Outcome<'a, A> {
    pbetween(pmany(pspace()), p, pmany(pspace()))
}

fn presult<'a, A>(a: A) -> impl Fn(Input<'a>) -> Outcome<'a, A>
where
    A: Copy,
{
    move |input: Input| Ok((a, input))
}

fn psat<'a>(f: impl Fn(I) -> bool, cond: String) -> impl Fn(Input<'a>) -> Outcome<'a, I> {
    move |input: Input| {
        if input.is_empty() {
            return Err(format!("No input to match"));
        }

        let (first, rest) = (input[0], &input[1..]);

        if f(first) {
            presult(first)(rest)
        } else {
            Err(format!("{} did not satisfy {}.", first as char, cond))
        }
    }
}

fn pbind<'a, A, B>(
    p: impl Fn(Input<'a>) -> Outcome<'a, A>,
    f: impl Fn(A) -> B + 'a,
) -> impl Fn(Input<'a>) -> Outcome<'a, B> {
    move |input: Input| match p(input) {
        Ok((val, rest)) => Ok((f(val), rest)),
        Err(e) => Err(e),
    }
}

fn pitem<'a>(item: I) -> impl Fn(Input<'a>) -> Outcome<'a, I> {
    psat(
        Box::new(move |i: I| i.eq(&item)),
        format!("matching {}", item),
    )
}

fn pitems<'a>(items: &[I]) -> impl Fn(Input<'a>) -> Outcome<'a, Vec<I>> {
    move |input: Input| {
        let mut input = input;
        let mut res = vec![];

        for i in items {
            match pitem(*i)(input) {
                Ok((o, rest)) => {
                    res.push(o);
                    input = rest
                }
                Err(e) => return Err(e),
            }
        }

        return Ok((res, input));
    }
}

fn pseq<'a, A, B, C>(
    first: impl Fn(Input<'a>) -> Outcome<'a, A>,
    second: impl Fn(Input<'a>) -> Outcome<'a, B>,
    f: fn(A, B) -> C,
) -> impl Fn(Input<'a>) -> Outcome<'a, C> {
    move |input: Input| {
        return match first(input) {
            Ok((a, rest)) => match second(rest) {
                Ok((b, rest)) => Ok((f(a, b), rest)),
                Err(e) => Err(e),
            },
            Err(err) => Err(err),
        };
    }
}

fn pleft<'a, A, B>(
    first: impl Fn(Input<'a>) -> Outcome<'a, A>,
    second: impl Fn(Input<'a>) -> Outcome<'a, B>,
) -> impl Fn(Input<'a>) -> Outcome<'a, A> {
    move |input: Input| pseq(Box::new(&first), &second, |a, _| a)(input)
}

fn pright<'a, A, B>(
    first: impl Fn(Input<'a>) -> Outcome<'a, A>,
    second: impl Fn(Input<'a>) -> Outcome<'a, B>,
) -> impl Fn(Input<'a>) -> Outcome<'a, B> {
    move |input: Input| pseq(Box::new(&first), &second, |_, a| a)(input)
}

fn por<'a, A>(
    this: impl Fn(Input<'a>) -> Outcome<'a, A>,
    or_that: impl Fn(Input<'a>) -> Outcome<'a, A>,
) -> impl Fn(Input<'a>) -> Outcome<'a, A> {
    move |input: Input| {
        return match this(input) {
            Ok((val, rest)) => Ok((val, rest)),
            Err(_) => or_that(input),
        };
    }
}

fn pmany<'a, A>(
    parser: impl Fn(Input<'a>) -> Outcome<'a, A>,
) -> impl Fn(Input<'a>) -> Outcome<'a, Vec<A>> {
    move |input: Input| {
        let mut input = input;
        let mut res = vec![];
        loop {
            match parser(input) {
                Ok((val, rest)) => {
                    res.push(val);
                    input = rest
                }
                Err(_) => return Ok((res, input)),
            }
        }
    }
}

fn pmany1<'a, A>(
    parser: &impl Fn(Input<'a>) -> Outcome<'a, A>,
) -> impl Fn(Input<'a>) -> Outcome<'a, Vec<A>> {
    move |input: Input| {
        let mut input = input;
        let mut res = vec![];

        match parser(input) {
            Ok((val, rest)) => {
                input = rest;
                res.push(val);
            }
            Err(e) => return Err(e),
        }

        match pmany(&parser)(input) {
            Ok((mut vals, rest)) => {
                res.append(&mut vals);
                Ok((res, rest))
            }
            Err(e) => Err(e),
        }
    }
}

fn pbetween<'a, A, B, C>(
    delim_a: impl Fn(Input<'a>) -> Outcome<'a, B>,
    parser: impl Fn(Input<'a>) -> Outcome<'a, A>,
    delim_b: impl Fn(Input<'a>) -> Outcome<'a, C>,
) -> impl Fn(Input<'a>) -> Outcome<'a, A> {
    move |input: Input| match delim_a(input) {
        Ok((_, rest)) => match parser(rest) {
            Ok((val, rest)) => match delim_b(rest) {
                Ok((_, rest)) => Ok((val, rest)),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        },
        Err(e) => Err(e),
    }
}

fn pdelim<'a, A>(
    delim: impl Fn(Input<'a>) -> Outcome<'a, I>,
    parser: impl Fn(Input<'a>) -> Outcome<'a, A>,
) -> impl Fn(Input<'a>) -> Outcome<'a, Vec<A>> {
    move |input: Input| {
        let mut rest_i = input;
        let mut res = vec![];

        loop {
            match parser(rest_i) {
                Ok((val, rest)) => match delim(rest) {
                    Ok((_, inner_rest)) => {
                        res.push(val);
                        rest_i = inner_rest;
                    }
                    Err(_) => {
                        res.push(val);
                        rest_i = rest;
                        break;
                    }
                },
                Err(err) => {
                    if res.len() > 0 {
                        break;
                    }

                    return Err(err);
                }
            }
        }

        return Ok((res, rest_i));
    }
}

fn map<'a, A, B>(
    p: impl Fn(Input<'a>) -> Outcome<'a, A>,
    f: Box<impl Fn(A) -> B + 'a>,
) -> impl Fn(Input<'a>) -> Outcome<'a, B> {
    move |input: Input| match p(input) {
        Ok((val, rest)) => Ok((f(val), rest)),
        Err(e) => Err(e),
    }
}

fn rec<'a, A, B, C>(
    bc: &impl Fn(Input<'a>) -> Outcome<'a, A>,
    before: &impl Fn(Input<'a>) -> Outcome<'a, B>,
    after: &impl Fn(Input<'a>) -> Outcome<'a, C>,
    f: &impl Fn(A, B, C) -> A,
) -> impl Fn(Input<'a>) -> Outcome<'a, A> {
    |input: Input| match bc(input) {
        Ok((val, rest)) => Ok((val, rest)),
        Err(_) => match before(input) {
            Ok((b, rest)) => match rec(bc, before, after, f)(rest) {
                Ok((val, rest)) => match after(rest) {
                    Ok((a, rest)) => Ok((f(val, b, a), rest)),
                    Err(e) => Err(e),
                },
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        },
    }
}

fn pfold<'a, A, B>(
    p: impl Fn(Input<'a>) -> Outcome<'a, A>,
    init: impl Fn() -> B,
    fold: impl Fn(A, B) -> B,
) -> impl Fn(Input<'a>) -> Outcome<'a, B> {
    move |input: Input| {
        let mut acc = init();
        let mut rest = input;

        loop {
            match p(rest) {
                Ok((val, rest_)) => {
                    acc = fold(val, acc);
                    rest = rest_;
                }
                Err(_) => return Ok((acc, rest)),
            }
        }
    }
}

#[macro_export]
macro_rules! choice {
    ($p:expr, $q:expr$(,)?) => {por($p, $q)};
    ($p:expr, $($rest:expr),*$(,)?) => {por($p, $crate::choice!($($rest),*))}
}

#[macro_export]
macro_rules! pseq {
    ($p:expr$(,)?) => {map($p, Box::new(|a| vec![a]))};
    ($p:expr, $($rest:expr),*$(,)?) => {
        pseq(
            $crate::pseq!($p),
            $crate::pseq!($($rest),*),
            |mut xs, mut ys| {xs.append(&mut ys); return xs})
    }
}

#[macro_export]
macro_rules! pnot {
    ($p:expr) => {pnot(p)};
    ($($p:expr),*$(,)?) => {
        psat(
            Box::new(|v| !vec![$($p),*].contains(&(v as char))),
            format!("is not one of {:?}", vec![$($p),*])
        )
    }
}

#[macro_export]
macro_rules! pany {
    ($($p:expr),*$(,)?) => {
        psat(
            Box::new(|v| vec![$($p),*].contains(&(v as char))),
            format!("is one of {:?}", vec![$($p),*])
        )
    };
}

#[test]
fn t_pseq_macro() {
    let input = prepare("ABCD");

    let parser = pseq!(pchar('A'), pchar('B'), pchar('C'), pchar('D'));

    match parser(input) {
        Ok((val, _)) => assert_eq!(vec![b'A', b'B', b'C', b'D'], val),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pnot_macro() {
    let input = prepare("A");

    let parser = pnot!('B', 'D');

    match parser(input) {
        Ok((val, _)) => assert_eq!(val, b'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pany_macro() {
    let input = prepare("Z");

    let parser = pany!('A', 'B', 'C', 'Z');

    match parser(input) {
        Ok((val, _)) => assert_eq!(val, b'Z'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_ppadded() {
    let input = prepare("[a, b, c, ]");

    let parser = pbetween(
        pchar('['),
        ppadded(pdelim(pchar(','), ppadded(pletter()))),
        pchar(']'),
    );

    match parser(input) {
        Ok((val, _)) => assert_eq!(val, vec![b'a', b'b', b'c']),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pfold() {
    let input = prepare("abcde");

    let parser = pfold(pletter(), String::new, |c, s| format!("{s}{}", c as char));

    match parser(input) {
        Ok((val, _)) => assert_eq!(val, String::from("abcde")),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_tree() {
    let input = prepare("(a,((b,c),d))");

    #[derive(Debug, PartialEq)]
    enum AST {
        Leaf(char),
        Tree(Box<AST>, Box<AST>),
    }

    fn tree<'a>(input: Input<'a>) -> Outcome<'a, AST> {
        let leaf = pseq(
            Box::new(pletter()),
            pright(pchar(','), pletter()),
            |a, b| {
                AST::Tree(
                    Box::new(AST::Leaf(a as char)),
                    Box::new(AST::Leaf(b as char)),
                )
            },
        );
        let ltree = pseq(
            Box::new(pleft(
                map(pletter(), Box::new(|l| AST::Leaf(l as char))),
                pchar(','),
            )),
            tree,
            |a, b| AST::Tree(Box::new(a), Box::new(b)),
        );

        let rtree = pseq(
            Box::new(tree),
            pright(
                pchar(','),
                map(pletter(), Box::new(|l| AST::Leaf(l as char))),
            ),
            |a, b| AST::Tree(Box::new(a), Box::new(b)),
        );

        pbetween(
            pchar('('),
            por(
                Box::new(leaf),
                por(Box::new(ltree), por(Box::new(rtree), tree)),
            ),
            pchar(')'),
        )(input)
    }

    match tree(input) {
        Ok((val, _)) => assert_eq!(
            val,
            AST::Tree(
                Box::new(AST::Leaf('a')),
                Box::new(AST::Tree(
                    Box::new(AST::Tree(
                        Box::new(AST::Leaf('b')),
                        Box::new(AST::Leaf('c'))
                    )),
                    Box::new(AST::Leaf('d'))
                ))
            )
        ),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_cons() {
    let input = prepare("(A,(B,(C,nil)))");

    #[derive(Debug, PartialEq)]
    enum List {
        Nil,
        Cons(char, Box<List>),
    }

    let bc = pbetween(
        pchar('('),
        map(
            pleft(pletter(), pright(pchar(','), pstring("nil"))),
            Box::new(|d| List::Cons(d as char, Box::new(List::Nil))),
        ),
        pchar(')'),
    );

    let before = map(
        pright(pchar('('), pleft(pletter(), pchar(','))),
        Box::new(|d| d as char),
    );
    let after = map(pchar(')'), Box::new(|_| ()));

    let parser = rec(&bc, &before, &after, &|bc, b, _| {
        List::Cons(b, Box::new(bc))
    });

    match parser(input) {
        Ok((val, _)) => {
            assert_eq!(
                val,
                List::Cons(
                    'A',
                    Box::new(List::Cons(
                        'B',
                        Box::new(List::Cons('C', Box::new(List::Nil)))
                    ))
                )
            )
        }
        Err(e) => println!("{:?}", e),
    }
}

#[test]
fn t_rec() {
    let input = prepare("((A))");

    #[derive(Debug, PartialEq)]
    enum Nest {
        Empty,
        A,
        More(Box<Nest>),
    }

    let bc = map(pchar('A'), Box::new(|_| Nest::A));

    match rec(
        &bc,
        &map(pchar('('), Box::new(|_| Nest::Empty)),
        &map(pchar(')'), Box::new(|_| Nest::Empty)),
        &|a, _, _| Nest::More(Box::new(a)),
    )(input)
    {
        Ok((val, _)) => assert_eq!(val, Nest::More(Box::new(Nest::More(Box::new(Nest::A))))),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pitem() {
    let input = prepare("ABCD");

    match pitem('A' as u8)(input) {
        Ok((val, rest)) => {
            assert!(val.eq(&('A' as u8)));
            assert_eq!(String::from_utf8(rest.to_vec()).unwrap(), "BCD")
        }
        Err(e) => {
            println!("{e}");
            assert!(false)
        }
    }
}

#[test]
fn t_pseq() {
    let input = prepare("ABCD");

    let id_t = |i, j| (i, j);

    let athenb = pseq(Box::new(pchar('A')), pchar('B'), id_t);
    let cthend = pseq(Box::new(pchar('C')), pchar('D'), id_t);
    let parser = pseq(Box::new(athenb), cthend, |t1, t2| (t1, t2));

    match parser(input) {
        Ok((val, rest)) => {
            assert_eq!(val, (('A' as u8, 'B' as u8), ('C' as u8, 'D' as u8)));
            assert!(rest.is_empty())
        }
        Err(e) => {
            println!("{e}");
            assert!(false)
        }
    }
}

#[test]
fn t_por() {
    let input = prepare("B");

    let parse_a = pchar('A');
    let parse_b = pchar('B');
    let parse_a_or_b = por(Box::new(parse_a), parse_b);

    match parse_a_or_b(input) {
        Ok((val, rest)) => {
            assert_eq!(val, 'B' as u8);
            assert!(rest.is_empty())
        }
        Err(e) => {
            println!("{e}");
            assert!(false)
        }
    }
}

#[test]
fn t_pmany() {
    let input = prepare("AAAAAB");
    let input2 = prepare("B");

    let parse_a = pchar('A');
    let pmany_a = map(pmany(&parse_a), Box::new(|v: Vec<u8>| v.len()));

    match pmany_a(input) {
        Ok((val, rest)) => {
            assert_eq!(val, 5);
            assert_eq!(String::from_utf8(rest.to_vec()).unwrap(), "B")
        }
        Err(e) => {
            println!("{e}");
            assert!(false);
        }
    }

    match pmany_a(input2) {
        Ok((val, _)) => assert_eq!(val, 0),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pmany1() {
    let input = prepare("12345");
    let input2 = prepare("");

    let binding = pdigit();
    let num = map(
        pmany1(&binding),
        Box::new(|xs| String::from_utf8(xs).unwrap().parse::<u32>().unwrap()),
    );

    match num(input) {
        Ok((val, _)) => assert_eq!(val, 12345),
        Err(_) => assert!(false),
    }

    match num(input2) {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
    }
}

#[test]
fn t_peof() {
    let input = prepare("AAAAA");

    let parse_a = pchar('A');
    let pmany_a = map(pmany(&parse_a), Box::new(|_: Vec<u8>| 'A' as u8));
    let parse_find_end = pseq(Box::new(pmany_a), peof(), |a, _| a);

    match parse_find_end(input) {
        Ok((val, rest)) => {
            assert_eq!(val, 'A' as u8);
            assert!(rest.to_vec().is_empty())
        }
        Err(e) => {
            println!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn t_pbetween() {
    let input = prepare("{abc}");

    let parse_abc = pbind(pitems("abc".as_bytes()), |v| String::from_utf8(v).unwrap());
    let parse_between = pbetween(pchar('{'), parse_abc, pchar('}'));

    match parse_between(input) {
        Ok((val, rest)) => {
            assert_eq!(val, String::from("abc"));
            assert!(rest.to_vec().is_empty())
        }
        Err(e) => {
            println!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn t_pdelim() {
    let input = prepare("A,B,C");

    let p_delim = pchar(',');

    let parse = pdelim(p_delim, pletter());

    match parse(input) {
        Ok((val, _)) => {
            assert_eq!(val.len(), 3);
            assert_eq!(val, vec!['A' as u8, 'B' as u8, 'C' as u8])
        }
        Err(_) => assert!(false),
    }
}

#[test]
fn t_list() {
    #[derive(PartialEq, Debug, Clone, Copy)]
    enum Letter {
        A,
        B,
        C,
    }

    let input = prepare("[A,B,C]");

    let p_delim = pchar(',');
    let p_letter = map(
        por(Box::new(pchar('A')), por(Box::new(pchar('B')), pchar('C'))),
        Box::new(|c| match c as char {
            'A' => Letter::A,
            'B' => Letter::B,
            'C' => Letter::C,
            _ => unreachable!(),
        }),
    );
    let p_inner = pdelim(p_delim, p_letter);

    let parse = pbetween(pchar('['), p_inner, pchar(']'));

    match parse(input) {
        Ok((val, _)) => {
            assert_eq!(val.len(), 3);
            assert_eq!(val, vec![Letter::A, Letter::B, Letter::C])
        }
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pplus() {
    let input = prepare("52+567");

    #[derive(Debug, PartialEq)]
    enum AST {
        Num(u32),
        Add(Box<AST>, Box<AST>),
    }

    let pdigits = map(
        pmany(pdigit()),
        Box::new(|ds| AST::Num(String::from_utf8(ds).unwrap().parse::<u32>().unwrap())),
    );

    let padd = pchar('+');

    let parse = pseq(Box::new(&pdigits), pright(&padd, &pdigits), |a, b| {
        AST::Add(Box::new(a), Box::new(b))
    });

    match parse(input) {
        Ok((val, _)) => assert_eq!(
            val,
            AST::Add(Box::new(AST::Num(52)), Box::new(AST::Num(567)))
        ),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_arithm() {
    #[derive(Debug, PartialEq)]
    enum AST {
        Num(u32),
        Add(Box<AST>, Box<AST>),
        Mult(Box<AST>, Box<AST>),
        Sub(Box<AST>, Box<AST>),
        Div(Box<AST>, Box<AST>),
        Pow(Box<AST>, Box<AST>),
    }

    fn expression<'a>(i: Input<'a>) -> Outcome<'a, AST> {
        let binding = pdigit();
        let num = map(
            pmany1(&binding),
            Box::new(|xs| AST::Num(String::from_utf8(xs).unwrap().parse::<u32>().unwrap())),
        );

        let parens = pbetween(pchar('('), expression, pchar(')'));

        let op = choice!(
            pseq(
                por(&num, &parens),
                pright(pchar('+'), expression),
                |a, b| AST::Add(Box::new(a), Box::new(b)),
            ),
            pseq(
                por(&num, &parens),
                pright(pchar('*'), expression),
                |a, b| AST::Mult(Box::new(a), Box::new(b)),
            ),
            pseq(
                por(&num, &parens),
                pright(pchar('-'), expression),
                |a, b| AST::Sub(Box::new(a), Box::new(b)),
            ),
            pseq(
                por(&num, &parens),
                pright(pchar('/'), expression),
                |a, b| AST::Div(Box::new(a), Box::new(b)),
            ),
            pseq(
                por(&num, &parens),
                pright(pchar('^'), expression),
                |a, b| AST::Pow(Box::new(a), Box::new(b)),
            ),
        );

        por(&op, por(&parens, &num))(i)
    }

    let input = prepare("(1+2^3)");

    match pleft(expression, peof())(input) {
        Ok((val, _)) => assert_eq!(
            val,
            AST::Add(
                Box::new(AST::Num(1)),
                Box::new(AST::Pow(Box::new(AST::Num(2)), Box::new(AST::Num(3))))
            )
        ),
        Err(_) => assert!(false),
    }
}

fn main() {
    println!("Hello, world!");
}
