//! Thin wrappers around LALRPOP entry points for single-line parsing.

use lalrpop_util::ParseError as LalrpopError;
use lalrpop_util::lexer::Token;

use super::errors::{SyntaxErrorDetail, SyntaxErrorKind, TextSpan};
use crate::parser::ast;

lalrpop_util::lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(warnings)]
    line_grammar,
    "/parser/line_grammar.rs"
);

pub fn parse_relation(input: &str) -> Result<ast::Relation, SyntaxErrorDetail> {
    line_grammar::RelationParser::new()
        .parse(input)
        .map_err(|e| map_syntax_error(input, e))
}

/// Parse only root names (`Root[...]`) for top-level lines.
///
/// Callers can fall back to [`parse_relation`] if this fails.
pub fn parse_root_names(input: &str) -> Result<Vec<String>, SyntaxErrorDetail> {
    line_grammar::RootNamesParser::new()
        .parse(input)
        .map_err(|e| map_syntax_error(input, e))
}

fn map_syntax_error(
    input: &str,
    error: LalrpopError<usize, Token<'_>, &'static str>,
) -> SyntaxErrorDetail {
    match error {
        LalrpopError::InvalidToken { location } => SyntaxErrorDetail::new(
            SyntaxErrorKind::InvalidToken,
            Some(single_char_span(input, location)),
            Vec::new(),
        ),
        LalrpopError::UnrecognizedEof { location, expected } => SyntaxErrorDetail::new(
            SyntaxErrorKind::UnexpectedEof,
            Some(TextSpan::new(location, location)),
            normalize_expected(expected),
        ),
        LalrpopError::UnrecognizedToken {
            token: (start, token, end),
            expected,
        } => SyntaxErrorDetail::new(
            SyntaxErrorKind::UnexpectedToken {
                found: token.1.to_string(),
            },
            Some(TextSpan::new(start, end)),
            normalize_expected(expected),
        ),
        LalrpopError::ExtraToken {
            token: (start, token, end),
        } => SyntaxErrorDetail::new(
            SyntaxErrorKind::ExtraToken {
                found: token.1.to_string(),
            },
            Some(TextSpan::new(start, end)),
            Vec::new(),
        ),
        LalrpopError::User { error } => SyntaxErrorDetail::new(
            SyntaxErrorKind::User {
                message: error.to_string(),
            },
            None,
            Vec::new(),
        ),
    }
}

fn normalize_expected(mut expected: Vec<String>) -> Vec<String> {
    expected.sort();
    expected.dedup();
    expected
}

fn single_char_span(input: &str, start: usize) -> TextSpan {
    let end = input
        .get(start..)
        .and_then(|suffix| suffix.chars().next().map(|ch| start + ch.len_utf8()))
        .unwrap_or(start);
    TextSpan::new(start, end)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::{Arg, Expr, RelationName};

    #[test]
    fn parses_generic_relation_with_named_args() {
        let relation = parse_relation("Filter[$0, keep=true => out:i64]").unwrap();
        assert_eq!(relation.name, RelationName::Standard("Filter".to_string()));
        assert_eq!(relation.args.positional.len(), 1);
        assert_eq!(relation.args.named.len(), 1);
        assert_eq!(relation.outputs.len(), 1);
    }

    #[test]
    fn parses_extension_relation() {
        let relation = parse_relation("ExtensionLeaf:ParquetScan[path='x']").unwrap();
        assert!(matches!(relation.name, RelationName::Extension(_, _)));
    }

    #[test]
    fn parses_root_names() {
        let names = parse_root_names("Root[a, b, c]").unwrap();
        assert_eq!(names, vec!["a", "b", "c"]);
    }

    #[test]
    fn named_args_must_follow_positional_args() {
        let relation = parse_relation("Project[$0, keep=true]").unwrap();
        assert!(matches!(
            relation.args.positional[0],
            Arg::Expr(Expr::FieldRef(0))
        ));
    }
}
