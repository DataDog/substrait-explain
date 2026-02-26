//! Text → Substrait protobuf parsing.
//!
//! Parsing proceeds in two phases:
//!
//! 1. **Syntax** — each line is parsed into an [`ast`] node by the LALRPOP
//!    grammar (invoked through `lalrpop_line`). The [`structural`] module
//!    drives this, handling indentation, section headers, and the extensions
//!    preamble.
//! 2. **Lowering** — the AST is converted to a [`substrait::proto::Plan`] by
//!    the [`lower`] module, which performs semantic validation (anchor
//!    resolution, column counts, type checking).
//!
//! The main entry point is [`Parser::parse`].

pub mod ast;
pub mod errors;
pub mod extensions;
pub(crate) mod lalrpop_line;
pub mod lower;
pub mod structural;

pub use errors::{ErrorKind, MessageParseError, ParseContext, ParseError, ParseResult};
pub use structural::{PLAN_HEADER, Parser};
