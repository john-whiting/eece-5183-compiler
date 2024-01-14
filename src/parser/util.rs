use crate::scanner::{Scanner, ScannerItem};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub line_count: usize,
    pub column_count: usize,
    pub partial_match: bool,
    pub halting: bool,
}

impl ParseError {
    pub fn new(message: String, partial_match: bool, halting: bool, item: ScannerItem) -> Self {
        Self {
            message,
            line_count: item.line_count,
            column_count: item.column_count,
            partial_match,
            halting,
        }
    }
}

macro_rules! parse_error {
    ($m: tt, $i: ident, $partial_match: expr, $halting: expr) => {
        Err($crate::parser::util::ParseError::new(
            format!($m),
            $partial_match,
            $halting,
            $i,
        ))
    };
    ($m: tt, $i: ident) => {
        $crate::parser::util::parse_error!($m, $i, true, false)
    };
}

pub(crate) use parse_error;

macro_rules! not_partial {
    ($e: expr) => {
        match $e {
            Ok(o) => Ok(o),
            Err(e) => Err($crate::parser::util::ParseError {
                partial_match: false,
                ..e
            }),
        }
    };
}

pub(crate) use not_partial;

pub type ParseInput<'a> = Scanner<'a>;
pub type ParseResult<'a, T> = Result<(ParseInput<'a>, T), ParseError>;

macro_rules! token {
    ($input: ident, $p: pat) => {{
        let mut cloned = $input.clone();
        if let Some(item) = cloned.next() {
            match item.token {
                $p => Ok((cloned, item.token)),
                _ => {
                    let found_token = &item.token;
                    let expected_token = stringify!($p);
                    $crate::parser::util::parse_error!(
                        "Expected token of type {expected_token} but instead found {found_token}.",
                        item
                    )
                }
            }
        } else {
            // Should *never* be able to get here
            panic!("Unable to continue parsing past EOF.");
        }
    }};
    ($input: ident, $p: pat => $x: ident) => {{
        let mut cloned = $input.clone();
        if let Some(item) = cloned.next() {
            match item.token {
                $p => Ok((cloned, $x)),
                _ => {
                    let found_token = &item.token;
                    let expected_token = stringify!($p);
                    $crate::parser::util::parse_error!(
                        "Expected token of type {expected_token} but instead found {found_token}.",
                        item
                    )
                }
            }
        } else {
            // Should *never* be able to get here
            panic!("Unable to continue parsing past EOF.");
        }
    }};
}

pub(crate) use token;

macro_rules! GenericParsable {
    ($a: lifetime, $t: ident) => {
        impl Fn($crate::parser::util::ParseInput<$a>) -> $crate::parser::util::ParseResult<$a, $t>
    };
}

pub(crate) use GenericParsable;

macro_rules! accumulate_match {
    ($f: ident, $acc: ident, $input: ident) => {{
        let result = $f($input.clone());
        match result {
            Err($crate::parser::util::ParseError {
                partial_match: false,
                ..
            }) => return Ok(($input, $acc)),
            Err(e) => return Err(e),
            Ok((i, o)) => {
                $input = i;
                $acc.push(o);
            }
        }
    }};
}

pub(crate) use accumulate_match;

pub fn many0<'a, T>(
    f: GenericParsable!('a, T),
) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, Vec<T>> {
    move |mut input| {
        let mut acc = Vec::new();
        loop {
            accumulate_match!(f, acc, input);
        }
    }
}

pub fn opt<'a, T>(
    f: GenericParsable!('a, T),
) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, Option<T>> {
    move |input| match f(input.clone()) {
        Ok((i, result)) => Ok((i, Some(result))),
        Err(ParseError {
            partial_match: false,
            ..
        }) => Ok((input, None)),
        Err(e) => Err(e),
    }
}

pub fn preceeded<'a, F, T>(
    first: GenericParsable!('a, F),
    second: GenericParsable!('a, T),
) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, T> {
    move |input| {
        let (input, _) = first(input)?;
        second(input)
    }
}

pub fn separated_list0<'a, S, T>(
    separator: GenericParsable!('a, S),
    f: GenericParsable!('a, T),
) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, Vec<T>> {
    move |mut input| {
        let mut acc = Vec::new();

        // Attempt to match one
        accumulate_match!(f, acc, input);

        // Attempt to match more
        let (input, mut acc2) = many0(preceeded(&separator, &f))(input)?;

        acc.append(&mut acc2);

        Ok((input, acc))
    }
}
