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

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        F: Fn(&Output) -> bool + 'a,
        Output: 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParserResult<'a, Output>,
{
    fn parse(&self, input: &'a str) -> ParserResult<'a, Output> {
        self(input)
    }
}

struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParserResult<'a, Output> {
        self.parser.parse(input)
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

fn identifier(input: &str) -> ParserResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    for next in chars {
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
fn test_pred() {
    let parser = any_char.pred(|&c| c == 'Á');
    assert_eq!(Ok(("rvíztűrő", 'Á')), parser.parse("Árvíztűrő"));
    assert_eq!(Err("árvíztűrő"), parser.parse("árvíztűrő"));
    assert_eq!(Err(""), parser.parse(""));
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    any_char.pred(|&c| c.is_whitespace())
}

#[test]
fn test_whitespace_char() {
    assert_eq!(Ok(("abc", ' ')), whitespace_char().parse(" abc"));
    assert_eq!(Ok(("abc", '\n')), whitespace_char().parse("\nabc"));
    assert_eq!(Ok(("abc", '\t')), whitespace_char().parse("\tabc"));
    assert_eq!(Err("abc"), whitespace_char().parse("abc"));
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

#[test]
fn test_space() {
    assert_eq!(
        Ok(("abc", vec![' ', '\t', ' ', '\n'])),
        space1().parse(" \t \nabc")
    );
    assert_eq!(
        Ok(("abc", vec![' ', '\t', ' ', '\n'])),
        space0().parse(" \t \nabc")
    );
    assert_eq!(Err("abc"), space1().parse("abc"));
    assert_eq!(Ok(("abc", vec![])), space0().parse("abc"));
}

fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|&c| c != '"')),
            match_literal("\""),
        ),
    )
    .map(|chars| chars.iter().collect())
}

#[test]
fn test_quoted_string() {
    assert_eq!(
        Ok(("", "Helló".to_string())),
        quoted_string().parse("\"Helló\"")
    );
    assert_eq!(Err("Helló"), quoted_string().parse("Helló"));
    assert_eq!(Err("Helló\""), quoted_string().parse("Helló\""));
    assert_eq!(Err(""), quoted_string().parse("\"Helló"));
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(
        identifier,
        right(
            right(space0(), match_literal("=")),
            right(space0(), quoted_string()),
        ),
    )
    //pair( identifier, right( match_literal("="), quoted_string()))
}

#[test]
fn test_attribute_paid() {
    assert_eq!(
        Ok(("", ("key".to_string(), "value".to_string()))),
        attribute_pair().parse("key  =\n\t\"value\"")
    );
}

fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}

#[test]
fn test_attributes() {
    assert_eq!(
        Ok((
            "",
            vec![
                ("firstname".to_string(), "Lópici".to_string()),
                ("lastname".to_string(), "Gáspár".to_string())
            ]
        )),
        attributes().parse(" firstname=\"Lópici\" lastname=\"Gáspár\"")
    )
}

fn open_element<'a>() -> impl Parser<'a, Element> {
    right(match_literal("<"), pair(identifier, attributes())).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

fn single_element<'a>() -> impl Parser<'a, Element> {
    left(open_element(), right(space0(), match_literal("/>")))
}

#[test]
fn test_single_element() {
    assert_eq!(
        Ok((
            "",
            Element {
                name: "div".to_string(),
                attributes: vec![("class".to_string(), "div1".to_string())],
                children: vec![]
            }
        )),
        single_element().parse("<div class=\"div1\"/>")
    );
}

fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        Ok((next_str, result)) => Ok((next_str, result)),
        Err(_) => parser2.parse(input),
    }
}

#[test]
fn test_either() {
    assert_eq!(
        Ok(("-", ())),
        either(match_literal("+"), match_literal("-")).parse("+-")
    );
    assert_eq!(
        Ok(("+", ())),
        either(match_literal("+"), match_literal("-")).parse("-+")
    );
    assert_eq!(
        Err("!+-"),
        either(match_literal("+"), match_literal("-")).parse("!+-")
    );
}

fn close_element<'a>(expected_name: String) -> impl Parser<'a, String> {
    right(match_literal("</"), left(identifier, match_literal(">")))
        .pred(move |name| name == &expected_name)
}

#[test]
fn test_close_element() {
    assert_eq!(
        Ok(("", "div".to_string())),
        close_element("div".to_string()).parse("</div>")
    )
}

fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
    //F: Fn(A)-> BoxedParser<'a, B>,
{
    move |input: &'a str| match parser.parse(input) {
        Ok((next, result)) => f(result).parse(next),
        Err(err) => Err(err),
    }
}

fn parent_element<'a>() -> impl Parser<'a, Element> {
    left(open_element(),left(space0(),match_literal(">"))).and_then( |e| {
        left(zero_or_more(element()), close_element(e.name.clone())).map(move |children| {
            let mut e = e.clone();
            e.children = children;
            e
        })
    })
}

fn element<'a>() -> impl Parser<'a, Element> {
    right(
        space0(), 
        left( 
            either(single_element(), parent_element()),
            space0()
        
)   )
}


#[test]
fn xml_parser() {
    let doc = r#"
        <top label="Top">
            <semi-bottom label="Bottom"/>
            <middle>
                <bottom label="Another bottom"/>
            </middle>
        </top>"#;
    let parsed_doc = Element {
        name: "top".to_string(),
        attributes: vec![("label".to_string(), "Top".to_string())],
        children: vec![
            Element {
                name: "semi-bottom".to_string(),
                attributes: vec![("label".to_string(), "Bottom".to_string())],
                children: vec![],
            },
            Element {
                name: "middle".to_string(),
                attributes: vec![],
                children: vec![Element {
                    name: "bottom".to_string(),
                    attributes: vec![("label".to_string(), "Another bottom".to_string())],
                    children: vec![],
                }],
            },
        ],
    };
    assert_eq!(Ok(("", parsed_doc)), element().parse(doc));
}


#[test]
fn mismatched_closing_tag() {
    let doc = r#"
        <top>
            <bottom/>
        </middle>"#;
    assert_eq!(Err("</middle>"), element().parse(doc));
}