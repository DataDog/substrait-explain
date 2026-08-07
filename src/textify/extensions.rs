//! Extension textification support
//!
//! This module provides [`Textify`] implementations for extension-related
//! types, including [`ExtensionValue`], [`ExtensionColumn`], [`ExtensionArgs`],
//! and the various extension relation types ([`substrait::proto::ExtensionLeafRel`],
//! [`substrait::proto::ExtensionSingleRel`], [`substrait::proto::ExtensionMultiRel`]).

use std::fmt;

use crate::FormatError;
use crate::extensions::{Expr, ExtensionArgs, ExtensionColumn, ExtensionValue, TupleValue};
use crate::textify::foundation::{Scope, Textify};
use crate::textify::types::{Name, escaped};

impl Textify for TupleValue {
    fn name() -> &'static str {
        "TupleValue"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "(")?;
        if self.len() == 1 {
            self.iter().next().unwrap().textify(ctx, w)?;
            write!(w, ",")?;
        } else {
            write!(w, "{}", ctx.separated(self, ", "))?;
        }
        write!(w, ")")
    }
}

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
            ExtensionValue::Null => write!(w, "null"),
            ExtensionValue::Error(error) => {
                write!(w, "{}", ctx.failure(FormatError::Extension(error.clone())))
            }
            ExtensionValue::Expr(expr) => expr.textify(ctx, w),
            ExtensionValue::Enum(e) => write!(w, "&{e}"),
            ExtensionValue::Tuple(tv) => tv.textify(ctx, w),
        }
    }
}

impl Textify for Expr {
    fn name() -> &'static str {
        "Expr"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}", ctx.display(self.as_proto()))
    }
}

impl Textify for ExtensionColumn {
    fn name() -> &'static str {
        "ExtensionColumn"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            ExtensionColumn::Named { name, r#type: ty } => {
                write!(w, "{}:{}", Name(name), ctx.display(ty))
            }
            ExtensionColumn::Expr(expr) => expr.textify(ctx, w),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::extensions::ExtensionError;
    use crate::fixtures::TestContext;

    #[test]
    fn error_value_renders_failure_and_continues() {
        let mut args = ExtensionArgs::default();
        args.insert("bad", ExtensionError::Custom("malformed field".to_string()));
        args.insert("good", "continues");

        let (rendered, errors) = TestContext::new().textify(&args);

        assert_eq!(rendered, "bad=!{extension}, good='continues'");
        assert!(matches!(
            errors.0.as_slice(),
            [FormatError::Extension(ExtensionError::Custom(message))]
                if message == "malformed field"
        ));
    }
}
