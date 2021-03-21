#![allow(dead_code)]

#[cfg(not(tarpaulin_include))]
fn main() {
    println!("Hello, world!");
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParserResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParserResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParserResult<'a, Output>,
{
    fn parse(&self, input: &'a str) -> ParserResult<'a, Output> {
        self(input)
    }
}

fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
    let n = input.chars().next();
    match n {
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

#[test]
fn test_the_letter_a() {
    assert_eq!(Ok(("bcde", ())), the_letter_a("abcde"));
    assert_eq!(Err("edcba"), the_letter_a("edcba"));
}

fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

#[test]
fn test_match_literal() {
    let parse_hello = match_literal("Hello");
    assert_eq!(Ok((", world!", ())), parse_hello.parse("Hello, world!"));
    assert_eq!(Ok((" világ!", ())), parse_hello.parse("Hello világ!"));
    let kiskut = "Kiskút, kerekeskút van az udvarunkban";
    assert_eq!(Err(kiskut), parse_hello.parse(kiskut))
}

fn identifier<'a>(input: &'a str) -> ParserResult<'a, String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next)
        } else {
            break;
        }
    }

    Ok((&input[matched.len()..], matched))
}

#[test]
fn test_identifier() {
    assert_eq!(
        Ok((", world!", "Hello".to_string())),
        identifier("Hello, world!")
    );
    assert_eq!(
        Ok(("", "abc-12---q1w".to_string())),
        identifier("abc-12---q1w")
    );
    assert_eq!(
        Ok(("!2---q1w", "abc-1".to_string())),
        identifier("abc-1!2---q1w")
    );
    assert_eq!(Err("!abc"), identifier("!abc"))
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input: &'a str| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

#[test]
fn test_pair() {
    let tag_opener = pair(match_literal("<"), identifier);
    assert_eq!(
        Ok(("/>", ((), "element".to_string()))),
        tag_opener.parse("<element/>")
    );
    assert_eq!(Err("oops"), tag_opener.parse("oops"));
    assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input: &'a str| {
        parser
            .parse(input)
            .map(|(next, result)| (next, map_fn(result)))
    }
}

#[test]
fn test_map() {
    let map_identifier_to_int = |s: String| i64::from_str_radix(s.as_str(), 16).unwrap();
    let int_parser = map(identifier, map_identifier_to_int);
    assert_eq!(Ok((", world!", 0xa123)), int_parser.parse("a123, world!"));
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(result1, _result2)| result1)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_result1, result2)| result2)
}

#[test]
fn test_left_right() {
    assert_eq!(
        Ok(("cd", ("ab12".to_string()))),
        left(identifier, match_literal("!")).parse("ab12!cd")
    );
    assert_eq!(
        Ok(("cd", ())),
        right(identifier, match_literal("!")).parse("ab12!cd")
    );
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input: &'a str| {
        let mut result = Vec::<A>::new();
        match parser.parse(input) {
            Ok((next_input, next_item)) => {
                input = next_input;
                result.push(next_item)
            }
            Err(err) => return Err(err),
        }
        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item)
        }
        Ok((input, result))
    }
    /* It is not working:
    map(pair( parser, zero_or_more(parser)), |(first, mut others)| {
        others.insert(0, first);
        others
    })
    */
}

#[test]
fn test_one_or_more() {
    let parser = one_or_more(match_literal("mi"));
    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("mimimi"));
    assert_eq!(Ok(("AAA", vec![(), (), ()])), parser.parse("mimimiAAA"));
    assert_eq!(Err("nini"), parser.parse("nini"));
    assert_eq!(Err(""), parser.parse(""));
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input: &'a str| {
        let mut result = Vec::<A>::new();
        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item)
        }
        Ok((input, result))
    }
}

#[test]
fn test_zero_or_more() {
    let parser = zero_or_more(match_literal("mi"));
    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("mimimi"));
    assert_eq!(Ok(("AAA", vec![(), (), ()])), parser.parse("mimimiAAA"));
    assert_eq!(Ok(("nini", vec![])), parser.parse("nini"));
    assert_eq!(Ok(("", vec![])), parser.parse(""));
}

fn any_char(input: &str) -> ParserResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

#[test]
fn test_any_char() {
    assert_eq!(Ok(("rvíztűrő", 'Á')), any_char("Árvíztűrő"));
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input: &'a str| match parser.parse(input) {
        Ok((next, result)) => {
            if predicate(&result) {
                Ok((next, result))
            } else {
                Err(input)
            }
        }
        _ => Err(input),
    }
}

#[test]
fn test_pred (){
    let parser = pred( any_char, |&c| c == 'Á');
    assert_eq!(Ok(("rvíztűrő", 'Á')), parser.parse("Árvíztűrő"));
    assert_eq!( Err("árvíztűrő"), parser.parse("árvíztűrő"));
    assert_eq!( Err(""), parser.parse(""));
}

fn whitespace_char<'a>() -> impl Parser<'a, char>
{
    pred( any_char, |&c| c.is_whitespace())
}

#[test]
fn test_whitespace_char() {
    assert_eq!( Ok(("abc",' ')), whitespace_char().parse(" abc"));
    assert_eq!( Ok(("abc",'\n')), whitespace_char().parse("\nabc"));
    assert_eq!( Ok(("abc",'\t')), whitespace_char().parse("\tabc"));
    assert_eq!( Err("abc"), whitespace_char().parse("abc"));
}

fn space0<'a>()-> impl Parser<'a, Vec<char> >
{
    zero_or_more( whitespace_char())
}


fn space1<'a>()-> impl  Parser<'a, Vec<char> >
{
    one_or_more( whitespace_char())
}

#[test]
fn test_space() {
    assert_eq!( Ok(("abc", vec![' ', '\t', ' ', '\n'])), space1().parse(" \t \nabc"));
    assert_eq!( Ok(("abc", vec![' ', '\t', ' ', '\n'])), space0().parse(" \t \nabc"));
    assert_eq!( Err("abc"), space1().parse("abc"));
    assert_eq!( Ok(("abc", vec![])), space0().parse("abc"));
}