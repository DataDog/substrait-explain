//! Thin wrappers around LALRPOP entry points for single-line parsing.

use super::errors::ParseError;
use crate::parser::ast;

lalrpop_util::lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(warnings)]
    line_grammar,
    "/parser/line_grammar.rs"
);

pub fn parse_relation(input: &str) -> Result<ast::Relation, ParseError> {
    line_grammar::RelationParser::new()
        .parse(input)
        .map_err(|e| ParseError::ValidationError(format!("failed to parse relation: {e}")))
}

/// Parse only root names (`Root[...]`) for top-level lines.
///
/// Callers can fall back to [`parse_relation`] if this fails.
pub fn parse_root_names(input: &str) -> Result<Vec<String>, ParseError> {
    line_grammar::RootNamesParser::new()
        .parse(input)
        .map_err(|e| ParseError::ValidationError(format!("failed to parse root relation: {e}")))
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
