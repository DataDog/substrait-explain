//! Parser for the `=== Header` section of the Substrait text format.
//!
//! The header section carries plan-level metadata that describes how to
//! interpret the plan as a whole. Today the only supported field is
//! `Plan.version`, but the section is structured as a general framework so
//! that other top-level fields (e.g. `expected_type_urls`, `execution_behavior`)
//! can be added later without changing the surrounding grammar.
//!
//! The version uses a compact, semantic-versioning style on a single line:
//!
//! ```text
//! === Header
//! Version: 0.64.0
//!   producer: "datafusion-substrait 46.0.0"
//!   git_hash: f455ecbe8b581c9b93abd6fbf4a360ec54b196d9
//! ```
//!
//! The numeric `Version:` line is optional. A plan that only carries a
//! `producer` and/or `git_hash` (with all version numbers zero) is written
//! without a `Version:` line:
//!
//! ```text
//! === Header
//! producer: "datafusion-substrait 46.0.0"
//! ```
//!
//! Like [`crate::parser::extensions::ExtensionParser`], this parser does not
//! parse the `=== Header` line itself; that transition is handled by the
//! top-level [`crate::parser::structural::Parser`] state machine.

use substrait::proto::Version;
use thiserror::Error;

use crate::parser::structural::IndentedLine;

/// The `=== Header` section header line.
pub const HEADER_HEADER: &str = "=== Header";

/// The `Version` field key.
pub const VERSION_FIELD: &str = "Version";
/// The `producer` field key.
pub const PRODUCER_FIELD: &str = "producer";
/// The `git_hash` field key.
pub const GIT_HASH_FIELD: &str = "git_hash";

#[derive(Debug, Clone, Error)]
pub enum HeaderParseError {
    #[error("Unknown header field: {0:?}")]
    UnknownField(String),
    #[error("Invalid version {0:?}: expected MAJOR.MINOR.PATCH")]
    InvalidVersion(String),
    #[error("Unexpected indentation in header line: {0:?}")]
    UnexpectedIndent(String),
}

/// Parses the lines of the `=== Header` section into a [`Version`].
///
/// This is symmetric with `PlanWriter::write_header`: the writer adds the
/// `=== Header` line and emits each field, while this parser consumes the
/// fields (the header line is consumed by the top-level parser).
#[derive(Debug, Default)]
pub struct HeaderParser {
    version: Option<Version>,
}

impl HeaderParser {
    pub fn parse_line(&mut self, line: IndentedLine<'_>) -> Result<(), HeaderParseError> {
        let IndentedLine(indent, s) = line;
        if indent > 1 {
            return Err(HeaderParseError::UnexpectedIndent(s.to_string()));
        }

        let (key, value) = split_field(s)?;
        match (indent, key) {
            (0, VERSION_FIELD) => {
                let (major, minor, patch) = parse_semver(value)?;
                let version = self.version_mut();
                version.major_number = major;
                version.minor_number = minor;
                version.patch_number = patch;
            }
            // `producer` and `git_hash` are accepted both nested under a
            // `Version:` line (indent 1) and on their own (indent 0).
            (_, PRODUCER_FIELD) => {
                self.version_mut().producer = unquote(value);
            }
            (_, GIT_HASH_FIELD) => {
                self.version_mut().git_hash = value.to_string();
            }
            (_, other) => return Err(HeaderParseError::UnknownField(other.to_string())),
        }

        Ok(())
    }

    fn version_mut(&mut self) -> &mut Version {
        self.version.get_or_insert_with(Version::default)
    }

    /// The parsed version, or `None` if the header had no version fields.
    pub fn version(self) -> Option<Version> {
        self.version
    }
}

/// Split a `key: value` line into its trimmed key and value.
fn split_field(s: &str) -> Result<(&str, &str), HeaderParseError> {
    match s.split_once(':') {
        Some((key, value)) => Ok((key.trim(), value.trim())),
        None => Err(HeaderParseError::UnknownField(s.to_string())),
    }
}

/// Strip a single pair of surrounding double quotes, if present.
fn unquote(s: &str) -> String {
    if s.len() >= 2 && s.starts_with('"') && s.ends_with('"') {
        s[1..s.len() - 1].to_string()
    } else {
        s.to_string()
    }
}

/// Parse a `MAJOR.MINOR.PATCH` semantic version string into three numbers.
fn parse_semver(s: &str) -> Result<(u32, u32, u32), HeaderParseError> {
    let parts: Vec<&str> = s.split('.').collect();
    if parts.len() != 3 {
        return Err(HeaderParseError::InvalidVersion(s.to_string()));
    }
    let parse = |p: &str| {
        p.parse::<u32>()
            .map_err(|_| HeaderParseError::InvalidVersion(s.to_string()))
    };
    Ok((parse(parts[0])?, parse(parts[1])?, parse(parts[2])?))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(lines: &[&str]) -> Option<Version> {
        let mut parser = HeaderParser::default();
        for line in lines {
            parser
                .parse_line(IndentedLine::from(*line))
                .unwrap_or_else(|e| panic!("Failed to parse {line:?}: {e}"));
        }
        parser.version()
    }

    #[test]
    fn test_full_version() {
        let v = parse(&[
            "Version: 0.64.0",
            "  producer: \"datafusion-substrait 46.0.0\"",
            "  git_hash: abc123",
        ])
        .unwrap();
        assert_eq!(v.major_number, 0);
        assert_eq!(v.minor_number, 64);
        assert_eq!(v.patch_number, 0);
        assert_eq!(v.producer, "datafusion-substrait 46.0.0");
        assert_eq!(v.git_hash, "abc123");
    }

    #[test]
    fn test_version_only() {
        let v = parse(&["Version: 1.2.3"]).unwrap();
        assert_eq!((v.major_number, v.minor_number, v.patch_number), (1, 2, 3));
        assert!(v.producer.is_empty());
        assert!(v.git_hash.is_empty());
    }

    #[test]
    fn test_metadata_without_version_line() {
        let v = parse(&["producer: \"my-producer\""]).unwrap();
        assert_eq!((v.major_number, v.minor_number, v.patch_number), (0, 0, 0));
        assert_eq!(v.producer, "my-producer");
    }

    #[test]
    fn test_empty_header() {
        assert!(parse(&[]).is_none());
    }

    #[test]
    fn test_unknown_field() {
        let mut parser = HeaderParser::default();
        let err = parser
            .parse_line(IndentedLine::from("Bogus: 1"))
            .unwrap_err();
        assert!(matches!(err, HeaderParseError::UnknownField(_)));
    }

    #[test]
    fn test_invalid_version() {
        let mut parser = HeaderParser::default();
        let err = parser
            .parse_line(IndentedLine::from("Version: 1.2"))
            .unwrap_err();
        assert!(matches!(err, HeaderParseError::InvalidVersion(_)));
    }
}
