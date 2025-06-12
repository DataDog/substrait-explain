use pest::Parser as PestParser;
use pest_derive::Parser as PestParser;

use crate::textify::Scope;

#[derive(PestParser)]
#[grammar = "parser/expression_grammar.pest"] // Path relative to src
pub struct ExpressionParser;

pub type Error = Box<pest::error::Error<Rule>>;

pub fn unwrap_single_pair(pair: pest::iterators::Pair<Rule>) -> pest::iterators::Pair<Rule> {
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();
    assert_eq!(pairs.next(), None);
    pair
}

pub fn unescape_string(s: &str, opener: char, closer: char) -> String {
    let mut result = String::new();
    let mut chars = s.chars();
    let first = chars.next().unwrap();
    assert_eq!(first, opener);
    // Skip the opening quote
    while let Some(c) = chars.next() {
        match c {
            c if c == closer => {
                // Skip the closing quote, and assert that there are no more characters.
                assert_eq!(chars.next(), None);
                break;
            }
            '\\' => {
                let next = chars.next().unwrap();
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

    // Parse a single instance of this type from a pest::iterators::Pair<Rule>.
    // The input must match the rule returned by `rule`; otherwise, a panic is
    // expected.
    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self;

    fn parse_str(s: &str) -> Result<Self, Error> {
        let mut pairs = ExpressionParser::parse(Self::rule(), s)?;
        assert_eq!(pairs.as_str(), s);
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        Ok(Self::parse_pair(pair))
    }
}

/// A trait for types that can be directly parsed from a string input,
/// regardless of context.
pub trait Parse {
    fn parse(input: &str) -> Result<Self, Error>
    where
        Self: Sized;
}

impl<T: ParsePair> Parse for T {
    fn parse(input: &str) -> Result<Self, Error> {
        T::parse_str(input)
    }
}

/// A trait for types that are parsed from a pest::iterators::Pair<Rule> that
/// depends on the context - e.g. extension lookups or other contextual
/// information. This is used for types that are not directly parsed from the
/// grammar, but rather require additional context to parse correctly.
pub trait ScopedParsePair: Sized {
    // The rule that this type is parsed from.
    fn rule() -> Rule;

    // Parse a single instance of this type from a pest::iterators::Pair<Rule>.
    // The input must match the rule returned by `rule`; otherwise, a panic is
    // expected.
    fn parse_pair<S: Scope>(scope: &mut S, pair: pest::iterators::Pair<Rule>) -> Self;
}

pub trait ScopedParse: Sized {
    fn parse<S: Scope>(scope: &mut S, input: &str) -> Result<Self, Error>
    where
        Self: Sized;
}

impl<T: ScopedParsePair> ScopedParse for T {
    fn parse<S: Scope>(scope: &mut S, input: &str) -> Result<Self, Error> {
        let mut pairs = ExpressionParser::parse(Self::rule(), input)?;
        assert_eq!(pairs.as_str(), input);
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        Ok(Self::parse_pair(scope, pair))
    }
}

pub fn iter_pairs(pair: pest::iterators::Pairs<Rule>) -> RuleIter<'_> {
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
    pub fn pop_if(&mut self, rule: Rule) -> Option<pest::iterators::Pair<'a, Rule>> {
        match self.peek() {
            Some(pair) if pair.as_rule() == rule => {
                self.iter.next();
                Some(pair)
            }
            _ => None,
        }
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
    pub fn parse_if_next_scoped<T: ScopedParsePair, S: Scope>(
        &mut self,
        scope: &mut S,
    ) -> Option<T> {
        match self.peek() {
            Some(pair) if pair.as_rule() == T::rule() => {
                self.iter.next();
                Some(T::parse_pair(scope, pair))
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
    pub fn parse_next_scoped<S: Scope, T: ScopedParsePair>(&mut self, scope: &mut S) -> T {
        let pair = self.iter.next().unwrap();
        T::parse_pair(scope, pair)
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
