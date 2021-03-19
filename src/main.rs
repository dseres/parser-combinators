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
    move |input: &str| {
        match input.get(0..expected.len()) {
            Some(next) if next == expected => Ok((&input[expected.len()..], ())),
            _ => Err(input),
        }
    }
}

#[test]
fn test_match_literal() {
    let parse_hello = match_literal("Hello");
    assert_eq!( Ok((", world!",())), parse_hello("Hello, world!"));
    assert_eq!( Ok((" világ!",())), parse_hello("Hello világ!"));
    let kiskut ="Kiskút, kerekeskút van az udvarunkban";
    assert_eq!( Err(kiskut), parse_hello(kiskut))   
}
