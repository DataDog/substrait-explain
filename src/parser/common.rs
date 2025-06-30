use std::fmt;

use pest::Parser as PestParser;
use pest_derive::Parser as PestDeriveParser;
use thiserror::Error;

use crate::extensions::SimpleExtensions;
use crate::extensions::simple::MissingReference;

#[derive(PestDeriveParser)]
#[grammar = "parser/expression_grammar.pest"] // Path relative to src
pub struct ExpressionParser;

/// An error that occurs when parsing a message within a specific line. Contains
/// context pointing at that specific error.
#[derive(Error, Debug, Clone)]
#[error("{kind} Error parsing {message}:\n{error}")]
pub struct MessageParseError {
    pub message: &'static str,
    pub kind: ErrorKind,
    #[source]
    pub error: Box<pest::error::Error<Rule>>,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Syntax,
    InvalidValue,
    Lookup(MissingReference),
}

impl MessageParseError {
    pub fn syntax(message: &'static str, span: pest::Span, description: impl ToString) -> Self {
        let error = pest::error::Error::new_from_span(
            pest::error::ErrorVariant::CustomError {
                message: description.to_string(),
            },
            span,
        );
        Self::new(message, ErrorKind::Syntax, Box::new(error))
    }
    pub fn invalid(message: &'static str, span: pest::Span, description: impl ToString) -> Self {
        let error = pest::error::Error::new_from_span(
            pest::error::ErrorVariant::CustomError {
                message: description.to_string(),
            },
            span,
        );
        Self::new(message, ErrorKind::InvalidValue, Box::new(error))
    }

    pub fn lookup(
        message: &'static str,
        missing: MissingReference,
        span: pest::Span,
        description: impl ToString,
    ) -> Self {
        let error = pest::error::Error::new_from_span(
            pest::error::ErrorVariant::CustomError {
                message: description.to_string(),
            },
            span,
        );
        Self::new(message, ErrorKind::Lookup(missing), Box::new(error))
    }
}

impl MessageParseError {
    pub fn new(
        message: &'static str,
        kind: ErrorKind,
        error: Box<pest::error::Error<Rule>>,
    ) -> Self {
        Self {
            message,
            kind,
            error,
        }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::Syntax => write!(f, "Syntax"),
            ErrorKind::InvalidValue => write!(f, "Invalid value"),
            ErrorKind::Lookup(e) => write!(f, "Invalid reference ({e})"),
        }
    }
}

pub fn unwrap_single_pair(pair: pest::iterators::Pair<Rule>) -> pest::iterators::Pair<Rule> {
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();
    assert_eq!(pairs.next(), None);
    pair
}

/// Unescapes a quoted string literal, handling escape sequences.
///
/// # Arguments
/// * `pair` - The pest pair containing the string to unescape (must be Rule::string_literal or Rule::quoted_name).
///
/// # Returns
/// * `String` with the unescaped contents.
///
/// # Panics
/// Panics if the rule is not `string_literal` or `quoted_name` (this should never happen
/// if the pest grammar is working correctly).
///
pub fn unescape_string(pair: pest::iterators::Pair<Rule>) -> String {
    let s = pair.as_str();

    // Determine opener/closer based on rule type
    let (opener, closer) = match pair.as_rule() {
        Rule::string_literal => ('\'', '\''),
        Rule::quoted_name => ('"', '"'),
        _ => panic!(
            "unescape_string called with unexpected rule: {:?}",
            pair.as_rule()
        ),
    };

    let mut result = String::new();
    let mut chars = s.chars();
    let first = chars.next().expect("Empty string literal");

    assert_eq!(
        first, opener,
        "Expected opening quote '{opener}', got '{first}'"
    );

    // Skip the opening quote
    while let Some(c) = chars.next() {
        match c {
            c if c == closer => {
                // Skip the closing quote, and assert that there are no more characters.
                assert_eq!(
                    chars.next(),
                    None,
                    "Unexpected characters after closing quote"
                );
                break;
            }
            '\\' => {
                let next = chars
                    .next()
                    .expect("Incomplete escape sequence at end of string");
                match next {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    // For all other characters (especially `"`, `'`, and `\`), we just
                    // push the character.
                    _ => result.push(next),
                }
            }
            _ => result.push(c),
        }
    }
    result
}

// A trait for converting a pest::iterators::Pair<Rule> into a Rust type. This
// is used to convert from the uniformly structured nesting
// pest::iterators::Pair<Rule> into more structured types.
pub trait ParsePair: Sized {
    // The rule that this type is parsed from.
    fn rule() -> Rule;

    // The name of the protobuf message type that this type corresponds to.
    fn message() -> &'static str;

    // Parse a single instance of this type from a pest::iterators::Pair<Rule>.
    // The input must match the rule returned by `rule`; otherwise, a panic is
    // expected.
    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self;

    fn parse_str(s: &str) -> Result<Self, MessageParseError> {
        let mut pairs = <ExpressionParser as pest::Parser<Rule>>::parse(Self::rule(), s)
            .map_err(|e| MessageParseError::new(Self::message(), ErrorKind::Syntax, Box::new(e)))?;
        assert_eq!(pairs.as_str(), s);
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        Ok(Self::parse_pair(pair))
    }
}

/// A trait for types that can be directly parsed from a string input,
/// regardless of context.
pub trait Parse {
    fn parse(input: &str) -> Result<Self, MessageParseError>
    where
        Self: Sized;
}

impl<T: ParsePair> Parse for T {
    fn parse(input: &str) -> Result<Self, MessageParseError> {
        T::parse_str(input)
    }
}

/// A trait for types that are parsed from a `pest::iterators::Pair<Rule>` that
/// depends on the context - e.g. extension lookups or other contextual
/// information. This is used for types that are not directly parsed from the
/// grammar, but rather require additional context to parse correctly.
pub trait ScopedParsePair: Sized {
    // The rule that this type is parsed from.
    fn rule() -> Rule;

    // The name of the protobuf message type that this type corresponds to.
    fn message() -> &'static str;

    // Parse a single instance of this type from a `pest::iterators::Pair<Rule>`.
    // The input must match the rule returned by `rule`; otherwise, a panic is
    // expected.
    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<Self, MessageParseError>;
}

pub trait ScopedParse: Sized {
    fn parse(extensions: &SimpleExtensions, input: &str) -> Result<Self, MessageParseError>
    where
        Self: Sized;
}

impl<T: ScopedParsePair> ScopedParse for T {
    fn parse(extensions: &SimpleExtensions, input: &str) -> Result<Self, MessageParseError> {
        let mut pairs = ExpressionParser::parse(Self::rule(), input)
            .map_err(|e| MessageParseError::new(Self::message(), ErrorKind::Syntax, Box::new(e)))?;
        assert_eq!(pairs.as_str(), input);
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        Self::parse_pair(extensions, pair)
    }
}

pub fn iter_pairs(pair: pest::iterators::Pairs<'_, Rule>) -> RuleIter<'_> {
    RuleIter {
        iter: pair,
        done: false,
    }
}

pub struct RuleIter<'a> {
    iter: pest::iterators::Pairs<'a, Rule>,
    // Set to true when done is called, so destructor doesn't panic
    done: bool,
}

impl<'a> From<pest::iterators::Pairs<'a, Rule>> for RuleIter<'a> {
    fn from(iter: pest::iterators::Pairs<'a, Rule>) -> Self {
        RuleIter { iter, done: false }
    }
}

impl<'a> RuleIter<'a> {
    pub fn peek(&self) -> Option<pest::iterators::Pair<'a, Rule>> {
        self.iter.peek()
    }

    // Pop the next pair if it matches the rule. Returns None if not.
    pub fn try_pop(&mut self, rule: Rule) -> Option<pest::iterators::Pair<'a, Rule>> {
        match self.peek() {
            Some(pair) if pair.as_rule() == rule => {
                self.iter.next();
                Some(pair)
            }
            _ => None,
        }
    }

    // Pop the next pair, asserting it matches the given rule. Panics if not.
    pub fn pop(&mut self, rule: Rule) -> pest::iterators::Pair<'a, Rule> {
        let pair = self.iter.next().expect("expected another pair");
        assert_eq!(
            pair.as_rule(),
            rule,
            "expected rule {:?}, got {:?}",
            rule,
            pair.as_rule()
        );
        pair
    }

    // Parse the next pair if it matches the rule. Returns None if not.
    pub fn parse_if_next<T: ParsePair>(&mut self) -> Option<T> {
        match self.peek() {
            Some(pair) if pair.as_rule() == T::rule() => {
                self.iter.next();
                Some(T::parse_pair(pair))
            }
            _ => None,
        }
    }

    // Parse the next pair if it matches the rule. Returns None if not.
    pub fn parse_if_next_scoped<T: ScopedParsePair>(
        &mut self,
        extensions: &SimpleExtensions,
    ) -> Option<Result<T, MessageParseError>> {
        match self.peek() {
            Some(pair) if pair.as_rule() == T::rule() => {
                self.iter.next();
                Some(T::parse_pair(extensions, pair))
            }
            _ => None,
        }
    }

    // Parse the next pair, assuming it matches the rule. Panics if not.
    pub fn parse_next<T: ParsePair>(&mut self) -> T {
        let pair = self.iter.next().unwrap();
        T::parse_pair(pair)
    }

    // Parse the next pair, assuming it matches the rule. Panics if not.
    pub fn parse_next_scoped<T: ScopedParsePair>(
        &mut self,
        extensions: &SimpleExtensions,
    ) -> Result<T, MessageParseError> {
        let pair = self.iter.next().unwrap();
        T::parse_pair(extensions, pair)
    }

    pub fn done(mut self) {
        self.done = true;
        assert_eq!(self.iter.next(), None);
    }
}

/// Make sure that the iterator was completely consumed when the iterator is
/// dropped - that we didn't leave any partially-parsed tokens.
///
/// This is not strictly necessary, but it's a good way to catch bugs.
impl Drop for RuleIter<'_> {
    fn drop(&mut self) {
        if self.done || std::thread::panicking() {
            return;
        }
        // If the iterator is not done, something probably went wrong.
        assert_eq!(self.iter.next(), None);
    }
}
