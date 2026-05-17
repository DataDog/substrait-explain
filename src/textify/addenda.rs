//! Addendum line textification support.
//!
//! Addenda are `+`-prefixed lines emitted between a relation header and its
//! child relations. This module owns their textifier-side shape and canonical
//! ordering.

use std::fmt;

use substrait::proto::extensions::AdvancedExtension;

use crate::FormatError;
use crate::extensions::any::AnyRef;
use crate::extensions::registry::ExtensionError;
use crate::extensions::{AddendumKind, ExtensionArgs};
use crate::textify::foundation::{PlanError, Scope, Textify};

/// All addenda associated with a relation, in canonical text-format order.
#[derive(Default)]
pub(super) struct AddendumLines {
    // Addenda are in textification order: extension table first (if it exists),
    // then enhancements, then optimizations.
    lines: Vec<AddendumLine>,
}

impl AddendumLines {
    pub(super) fn standard<S: Scope>(
        ctx: &S,
        advanced_extension: Option<&AdvancedExtension>,
    ) -> Self {
        let mut lines = Self::default();
        if let Some(advanced_extension) = advanced_extension {
            lines.extend_advanced_extension(ctx, advanced_extension);
        }
        lines
    }

    pub(super) fn extension_table<S: Scope>(
        ctx: &S,
        extension_table: Result<(String, ExtensionArgs), ExtensionError>,
        advanced_extension: Option<&AdvancedExtension>,
    ) -> Self {
        let mut lines = Self {
            lines: vec![AddendumLine::extension_table(extension_table)],
        };
        if let Some(advanced_extension) = advanced_extension {
            lines.extend_advanced_extension(ctx, advanced_extension);
        }
        lines
    }

    pub(super) fn none() -> Self {
        Self::default()
    }

    fn extend_advanced_extension<S: Scope>(
        &mut self,
        ctx: &S,
        advanced_extension: &AdvancedExtension,
    ) {
        if let Some(enhancement) = &advanced_extension.enhancement {
            self.lines
                .push(AddendumLine::enhancement(ctx, AnyRef::from(enhancement)));
        }
        self.lines.extend(
            advanced_extension
                .optimization
                .iter()
                .map(|optimization| AddendumLine::optimization(ctx, AnyRef::from(optimization))),
        );
    }
}

impl Textify for AddendumLines {
    fn name() -> &'static str {
        "AddendumLines"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        for line in &self.lines {
            writeln!(w)?;
            line.textify(ctx, w)?;
        }
        Ok(())
    }
}

enum AddendumLine {
    Decoded {
        kind: AddendumKind,
        name: String,
        args: ExtensionArgs,
    },
    DecodeError {
        kind: AddendumKind,
        error: ExtensionError,
    },
}

impl AddendumLine {
    fn extension_table(result: Result<(String, ExtensionArgs), ExtensionError>) -> Self {
        Self::from_decode_result(AddendumKind::ExtensionTable, result)
    }

    fn enhancement<S: Scope>(ctx: &S, detail: AnyRef<'_>) -> Self {
        Self::from_decode_result(
            AddendumKind::Enhancement,
            ctx.extension_registry().decode_enhancement(detail),
        )
    }

    fn optimization<S: Scope>(ctx: &S, detail: AnyRef<'_>) -> Self {
        Self::from_decode_result(
            AddendumKind::Optimization,
            ctx.extension_registry().decode_optimization(detail),
        )
    }

    fn from_decode_result(
        kind: AddendumKind,
        result: Result<(String, ExtensionArgs), ExtensionError>,
    ) -> Self {
        match result {
            Ok((name, args)) => Self::Decoded { kind, name, args },
            Err(error) => Self::DecodeError { kind, error },
        }
    }
}

impl Textify for AddendumLine {
    fn name() -> &'static str {
        "AddendumLine"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            AddendumLine::Decoded { kind, name, args } => {
                let indent = ctx.indent();
                let prefix = kind.prefix();

                if !args.output_columns.is_empty() {
                    let (message, description) = match kind {
                        AddendumKind::Enhancement | AddendumKind::Optimization => (
                            "addendum",
                            "output_columns cannot be represented in addendum syntax",
                        ),
                        AddendumKind::ExtensionTable => (
                            "addendum",
                            "output_columns cannot be represented in extension table addendum syntax",
                        ),
                    };
                    write!(
                        w,
                        "{indent}+ {prefix}[{}]",
                        ctx.failure(FormatError::Format(PlanError::invalid(
                            message,
                            Some(name.clone()),
                            description,
                        )))
                    )
                } else {
                    write!(w, "{indent}+ {prefix}:{name}[{}]", ctx.display(args))
                }
            }
            AddendumLine::DecodeError { kind, error } => {
                let indent = ctx.indent();
                let prefix = kind.prefix();
                write!(w, "{indent}+ {prefix}[{}]", ctx.failure(error.clone()))
            }
        }
    }
}
