#![allow(dead_code)]

fn main() {
    println!("Hello, world!");
    println!("{:?}", the_letter_a("Hello, world!"));
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
    let n = input.chars().next();
    match n {
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
    move |input: &str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

#[test]
fn test_match_literal() {
    let parse_hello = match_literal("Hello");
    assert_eq!(Ok((", world!", ())), parse_hello("Hello, world!"));
    assert_eq!(Ok((" világ!", ())), parse_hello("Hello világ!"));
    let kiskut = "Kiskút, kerekeskút van az udvarunkban";
    assert_eq!(Err(kiskut), parse_hello(kiskut))
}

fn identifier(input: &str) -> Result<(&str, String), &str> {
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

fn pair<P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Fn(&str) -> Result<(&str, (R1, R2)), &str>
where
    P1: Fn(&str) -> Result<(&str, R1), &str>,
    P2: Fn(&str) -> Result<(&str, R2), &str>,
{
    move |input: &str| match parser1(input) {
        Ok((next_input, result1)) => match parser2(next_input) {
            Ok((final_input, result2)) => Ok((final_input, (result1, result2))),
            Err(err) => Err(err),
        },
        Err(err) => Err(err),
    }
}

#[test]
fn test_pair() {
    let tag_opener = pair(match_literal("<"), identifier);
    assert_eq!( Ok(("/>", ((),"element".to_string()))), tag_opener("<element/>") );
    assert_eq!( Err("oops"), tag_opener("oops"));
    assert_eq!( Err("!oops"), tag_opener("<!oops"));
}
