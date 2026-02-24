//! Extension textification support
//!
//! This module provides [`Textify`] implementations for extension-related
//! types, including [`ExtensionValue`], [`ExtensionColumn`], [`ExtensionArgs`],
//! and the various extension relation types ([`ExtensionLeafRel`],
//! [`ExtensionSingleRel`], [`ExtensionMultiRel`]).

use std::fmt;

use substrait::proto::{ExtensionLeafRel, ExtensionMultiRel, ExtensionSingleRel};

use crate::extensions::any::AnyRef;
use crate::extensions::registry::ExtensionError;
use crate::extensions::{ExtensionArgs, ExtensionColumn, ExtensionValue};
use crate::textify::foundation::{Scope, Textify};
use crate::textify::types::escaped;

/// Decode an extension from an [`AnyRef`], and format it as text
fn format_extension<S: Scope, W: fmt::Write>(
    ctx: &S,
    w: &mut W,
    extension_type: &str,
    detail: Option<AnyRef<'_>>,
) -> fmt::Result {
    let indent = ctx.indent();

    match detail {
        Some(detail) => {
            // Decode the extension using the registry
            let registry = ctx.extension_registry();
            match registry.decode(detail) {
                Ok((name, args)) => {
                    // Success: format with extension name and args (with built-in ordering)
                    write!(
                        w,
                        "{}{}:{}[{}]",
                        indent,
                        extension_type,
                        name,
                        ctx.display(&args)
                    )?;
                }
                Err(error) => {
                    // Error decoding: format with error token
                    write!(w, "{}{}[{}]", indent, extension_type, ctx.failure(error))?;
                }
            }
        }
        None => {
            // No detail provided: format with error token
            let error = ExtensionError::ParseError("Extension detail is missing".to_string());
            write!(w, "{}{}[{}]", indent, extension_type, ctx.failure(error))?;
        }
    }

    Ok(())
}

impl Textify for ExtensionValue {
    fn name() -> &'static str {
        "ExtensionValue"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, _ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            ExtensionValue::String(s) => write!(w, "'{}'", escaped(s)),
            ExtensionValue::Integer(i) => write!(w, "{i}"),
            ExtensionValue::Float(f) => write!(w, "{f}"),
            ExtensionValue::Boolean(b) => write!(w, "{b}"),
            ExtensionValue::Reference(r) => write!(w, "${r}"),
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

        // Add named arguments using the extension's preferred order
        for (name, value) in self.ordered_named_args() {
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

/// Textify children relations with proper indentation
fn textify_children<S: Scope, W: fmt::Write>(
    ctx: &S,
    w: &mut W,
    children: &[substrait::proto::Rel],
) -> fmt::Result {
    let child_scope = ctx.push_indent();
    for child in children {
        writeln!(w)?;
        child.textify(&child_scope, w)?;
    }
    Ok(())
}

/// Textify a single child relation with proper indentation
fn textify_child<S: Scope, W: fmt::Write>(
    ctx: &S,
    w: &mut W,
    child: Option<&substrait::proto::Rel>,
) -> fmt::Result {
    if let Some(input) = child {
        let child_scope = ctx.push_indent();
        writeln!(w)?;
        input.textify(&child_scope, w)?;
    }
    Ok(())
}

impl Textify for ExtensionLeafRel {
    fn name() -> &'static str {
        "ExtensionLeafRel"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        // Convert prost_types::Any to AnyRef at the boundary
        let detail_ref = self.detail.as_ref().map(AnyRef::from);
        format_extension(ctx, w, "ExtensionLeaf", detail_ref)
    }
}

impl Textify for ExtensionSingleRel {
    fn name() -> &'static str {
        "ExtensionSingleRel"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        // Convert prost_types::Any to AnyRef at the boundary
        let detail_ref = self.detail.as_ref().map(AnyRef::from);
        format_extension(ctx, w, "ExtensionSingle", detail_ref)?;

        // Add child input regardless of whether extension was resolved
        textify_child(ctx, w, self.input.as_deref())?;
        Ok(())
    }
}

impl Textify for ExtensionMultiRel {
    fn name() -> &'static str {
        "ExtensionMultiRel"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        // Convert prost_types::Any to AnyRef at the boundary
        let detail_ref = self.detail.as_ref().map(AnyRef::from);
        format_extension(ctx, w, "ExtensionMulti", detail_ref)?;

        // Add child inputs regardless of whether extension was resolved
        textify_children(ctx, w, &self.inputs)?;
        Ok(())
    }
}
