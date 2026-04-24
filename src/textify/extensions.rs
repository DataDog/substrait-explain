//! Extension textification support
//!
//! This module provides [`Textify`] implementations for extension-related
//! types, including [`ExtensionValue`], [`ExtensionColumn`], [`ExtensionArgs`],
//! and the various extension relation types ([`substrait::proto::ExtensionLeafRel`],
//! [`substrait::proto::ExtensionSingleRel`], [`substrait::proto::ExtensionMultiRel`]).

use std::fmt;

use substrait::proto::extensions::AdvancedExtension;

use crate::FormatError;
use crate::extensions::any::AnyRef;
use crate::extensions::registry::ExtensionType;
use crate::extensions::{ExtensionArgs, ExtensionColumn, ExtensionValue};
use crate::textify::foundation::{PlanError, Scope, Textify};
use crate::textify::types::escaped;

impl Textify for ExtensionValue {
    fn name() -> &'static str {
        "ExtensionValue"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            ExtensionValue::String(s) => write!(w, "'{}'", escaped(s)),
            ExtensionValue::Integer(i) => write!(w, "{i}"),
            ExtensionValue::Float(f) => write!(w, "{f}"),
            ExtensionValue::Boolean(b) => write!(w, "{b}"),
            ExtensionValue::Reference(r) => write!(w, "${r}"),
            ExtensionValue::Enum(e) => write!(w, "&{e}"),
            ExtensionValue::Tuple(items) => {
                write!(w, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(w, ", ")?;
                    }
                    item.textify(ctx, w)?;
                }
                write!(w, ")")
            }
            ExtensionValue::Expression(e) => write!(w, "{e}"),
        }
    }
}

impl Textify for ExtensionColumn {
    fn name() -> &'static str {
        "ExtensionColumn"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, _ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            ExtensionColumn::Named { name, type_spec } => write!(w, "{name}:{type_spec}"),
            ExtensionColumn::Reference(r) => write!(w, "${r}"),
            ExtensionColumn::Expression(e) => write!(w, "{e}"),
        }
    }
}

impl Textify for ExtensionArgs {
    fn name() -> &'static str {
        "ExtensionArgs"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let mut has_args = false;

        // Add positional arguments
        for (i, value) in self.positional.iter().enumerate() {
            if i > 0 || has_args {
                write!(w, ", ")?;
            }
            value.textify(ctx, w)?;
            has_args = true;
        }

        // Add named arguments in display order (IndexMap preserves insertion order)
        for (name, value) in &self.named {
            if has_args {
                write!(w, ", ")?;
            }
            write!(w, "{name}=")?;
            value.textify(ctx, w)?;
            has_args = true;
        }

        if !has_args {
            write!(w, "_")?;
        }

        // Add output columns if present
        if !self.output_columns.is_empty() {
            write!(w, " => {}", ctx.separated(self.output_columns.iter(), ", "))?;
        }

        Ok(())
    }
}

/// Textify a single enhancement or optimization line.
///
/// Emits one of:
/// - `{indent}+ Enh:Name[args]`
/// - `{indent}+ Opt:Name[args]`
fn format_adv_ext_line<S: Scope, W: fmt::Write>(
    ctx: &S,
    w: &mut W,
    ext_type: ExtensionType,
    detail: AnyRef<'_>,
) -> fmt::Result {
    let indent = ctx.indent();
    let registry = ctx.extension_registry();
    let (prefix, decode_result) = match ext_type {
        ExtensionType::Enhancement => ("Enh", registry.decode_enhancement(detail)),
        ExtensionType::Optimization => ("Opt", registry.decode_optimization(detail)),
        ExtensionType::Relation => unreachable!("Relation extensions don't use adv_ext lines"),
    };
    match decode_result {
        Ok((name, args)) => {
            if !args.output_columns.is_empty() {
                write!(
                    w,
                    "{indent}+ {prefix}[{}]",
                    ctx.failure(FormatError::Format(PlanError::invalid(
                        "adv_extension",
                        Some(name),
                        "output_columns cannot be represented in adv_extension syntax",
                    )))
                )
            } else {
                write!(w, "{indent}+ {prefix}:{name}[{}]", ctx.display(&args))
            }
        }
        Err(error) => {
            write!(w, "{indent}+ {prefix}[{}]", ctx.failure(error))
        }
    }
}

impl Textify for AdvancedExtension {
    fn name() -> &'static str {
        "AdvancedExtension"
    }

    /// Textify all enhancement and optimization lines for an [`AdvancedExtension`].
    ///
    /// Writes one `+ Enh:` line (if an enhancement is present) followed by zero
    /// or more `+ Opt:` lines, each preceded by a newline.
    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        if let Some(enhancement) = &self.enhancement {
            writeln!(w)?;
            format_adv_ext_line(
                ctx,
                w,
                ExtensionType::Enhancement,
                AnyRef::from(enhancement),
            )?;
        }
        for optimization in &self.optimization {
            writeln!(w)?;
            format_adv_ext_line(
                ctx,
                w,
                ExtensionType::Optimization,
                AnyRef::from(optimization),
            )?;
        }
        Ok(())
    }
}
