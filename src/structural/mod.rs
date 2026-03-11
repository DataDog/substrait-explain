//! Structural types for unified value representation
//!
//! This module provides a unified type system that bridges the gap between
//! extension values and textify values, using proper lifetime management
//! with `Cow<'a, str>` for flexible string handling.

use std::borrow::Cow;
use std::fmt;

use crate::extensions::simple::{ExtensionFunction, SimpleExtensions};

/// A unified value type that can represent both extension arguments and textify values
/// with proper lifetime management
#[derive(Debug, Clone)]
pub enum StructuralValue<'a> {
    /// String value with flexible ownership (borrowed or owned)
    String(Cow<'a, str>),
    /// Integer literal value
    Integer(i64),
    /// Float literal value
    Float(f64),
    /// Boolean literal value
    Boolean(bool),
    /// Field reference ($0, $1, etc.)
    Reference(i32),
    /// Function call or extension reference
    Function(FunctionRef<'a>),
    /// Table name reference
    TableName(Vec<Cow<'a, str>>),
    /// Tuple of multiple values
    Tuple(Vec<StructuralValue<'a>>),
    /// List of values
    List(Vec<StructuralValue<'a>>),
    /// Missing or invalid value with error description
    Missing(Cow<'a, str>),
    /// Enum value representation
    Enum(Cow<'a, str>),
}

/// Represents a function call or extension reference with resolution state
#[derive(Debug, Clone)]
pub enum FunctionRef<'a> {
    /// Unresolved reference by name only
    Unresolved(Cow<'a, str>),
    /// Unresolved reference with anchor notation (e.g., "add#10")
    UnresolvedWithAnchor { name: Cow<'a, str>, anchor: u32 },
    /// Unresolved reference with URI anchor (e.g., "add@1")
    UnresolvedWithUri { name: Cow<'a, str>, uri_anchor: u32 },
    /// Unresolved reference with both anchors (e.g., "add#10@1")
    UnresolvedWithBoth {
        name: Cow<'a, str>,
        anchor: u32,
        uri_anchor: u32,
    },
    /// Resolved to a simple extension function
    Resolved {
        name: Cow<'a, str>,
        extension: &'a ExtensionFunction,
    },
}

/// Named argument pair for function calls and relations
#[derive(Debug, Clone)]
pub struct NamedArg<'a> {
    pub name: Cow<'a, str>,
    pub value: StructuralValue<'a>,
}

/// Arguments collection for relations and function calls
#[derive(Debug, Clone)]
pub struct Arguments<'a> {
    /// Positional arguments
    pub positional: Vec<StructuralValue<'a>>,
    /// Named arguments
    pub named: Vec<NamedArg<'a>>,
}

/// Column specification with type information
#[derive(Debug, Clone)]
pub enum ColumnSpec<'a> {
    /// Named column with optional type (name or name:type)
    Named {
        name: Cow<'a, str>,
        type_spec: Option<Cow<'a, str>>,
    },
    /// Field reference ($0, $1, etc.)
    Reference(i32),
    /// Expression column
    Expression(StructuralValue<'a>),
}

impl<'a> StructuralValue<'a> {
    /// Create a new string value from any string-like type
    pub fn string(s: impl Into<Cow<'a, str>>) -> Self {
        Self::String(s.into())
    }

    /// Create a new owned string value
    pub fn owned_string(s: String) -> Self {
        Self::String(Cow::Owned(s))
    }

    /// Create a new borrowed string value
    pub fn borrowed_string(s: &'a str) -> Self {
        Self::String(Cow::Borrowed(s))
    }

    /// Create a function reference from a name
    pub fn function(name: impl Into<Cow<'a, str>>) -> Self {
        Self::Function(FunctionRef::Unresolved(name.into()))
    }

    /// Create a function reference with anchor
    pub fn function_with_anchor(name: impl Into<Cow<'a, str>>, anchor: u32) -> Self {
        Self::Function(FunctionRef::UnresolvedWithAnchor {
            name: name.into(),
            anchor,
        })
    }

    /// Create a function reference with URI anchor
    pub fn function_with_uri(name: impl Into<Cow<'a, str>>, uri_anchor: u32) -> Self {
        Self::Function(FunctionRef::UnresolvedWithUri {
            name: name.into(),
            uri_anchor,
        })
    }

    /// Create a function reference with both anchors
    pub fn function_with_both(name: impl Into<Cow<'a, str>>, anchor: u32, uri_anchor: u32) -> Self {
        Self::Function(FunctionRef::UnresolvedWithBoth {
            name: name.into(),
            anchor,
            uri_anchor,
        })
    }

    /// Try to resolve function references using simple extensions
    pub fn resolve_functions(&mut self, extensions: &'a SimpleExtensions) -> Result<(), String> {
        match self {
            StructuralValue::Function(func_ref) => {
                *func_ref = func_ref.try_resolve(extensions)?;
            }
            StructuralValue::Tuple(values) | StructuralValue::List(values) => {
                for value in values {
                    value.resolve_functions(extensions)?;
                }
            }
            _ => {} // Other types don't need resolution
        }
        Ok(())
    }

    /// Convert to an owned version (removes all borrowed references)
    pub fn into_owned(self) -> StructuralValue<'static> {
        match self {
            StructuralValue::String(s) => StructuralValue::String(Cow::Owned(s.into_owned())),
            StructuralValue::Integer(i) => StructuralValue::Integer(i),
            StructuralValue::Float(f) => StructuralValue::Float(f),
            StructuralValue::Boolean(b) => StructuralValue::Boolean(b),
            StructuralValue::Reference(r) => StructuralValue::Reference(r),
            StructuralValue::Function(f) => StructuralValue::Function(f.into_owned()),
            StructuralValue::TableName(names) => StructuralValue::TableName(
                names
                    .into_iter()
                    .map(|n| Cow::Owned(n.into_owned()))
                    .collect(),
            ),
            StructuralValue::Tuple(values) => {
                StructuralValue::Tuple(values.into_iter().map(|v| v.into_owned()).collect())
            }
            StructuralValue::List(values) => {
                StructuralValue::List(values.into_iter().map(|v| v.into_owned()).collect())
            }
            StructuralValue::Missing(msg) => StructuralValue::Missing(Cow::Owned(msg.into_owned())),
            StructuralValue::Enum(e) => StructuralValue::Enum(Cow::Owned(e.into_owned())),
        }
    }
}

impl<'a> FunctionRef<'a> {
    /// Try to resolve this function reference using simple extensions
    pub fn try_resolve(self, extensions: &'a SimpleExtensions) -> Result<Self, String> {
        match self {
            FunctionRef::Unresolved(name) => {
                // Try to find by name in extensions
                if let Some(ext_fn) = extensions.functions().find_by_name(&name) {
                    Ok(FunctionRef::Resolved {
                        name,
                        extension: ext_fn,
                    })
                } else {
                    // Keep unresolved
                    Ok(FunctionRef::Unresolved(name))
                }
            }
            FunctionRef::UnresolvedWithAnchor { name, anchor } => {
                // Try to resolve by anchor
                if let Some(ext_fn) = extensions.functions().get(anchor) {
                    Ok(FunctionRef::Resolved {
                        name,
                        extension: ext_fn,
                    })
                } else {
                    Ok(FunctionRef::UnresolvedWithAnchor { name, anchor })
                }
            }
            // TODO: Add resolution logic for URI anchors and combined anchors
            other => Ok(other),
        }
    }

    /// Convert to owned version
    pub fn into_owned(self) -> FunctionRef<'static> {
        match self {
            FunctionRef::Unresolved(name) => FunctionRef::Unresolved(Cow::Owned(name.into_owned())),
            FunctionRef::UnresolvedWithAnchor { name, anchor } => {
                FunctionRef::UnresolvedWithAnchor {
                    name: Cow::Owned(name.into_owned()),
                    anchor,
                }
            }
            FunctionRef::UnresolvedWithUri { name, uri_anchor } => FunctionRef::UnresolvedWithUri {
                name: Cow::Owned(name.into_owned()),
                uri_anchor,
            },
            FunctionRef::UnresolvedWithBoth {
                name,
                anchor,
                uri_anchor,
            } => FunctionRef::UnresolvedWithBoth {
                name: Cow::Owned(name.into_owned()),
                anchor,
                uri_anchor,
            },
            FunctionRef::Resolved { name, extension: _ } => {
                // Can't preserve the reference in owned version, convert to unresolved
                FunctionRef::Unresolved(Cow::Owned(name.into_owned()))
            }
        }
    }

    /// Get the function name regardless of resolution state
    pub fn name(&self) -> &str {
        match self {
            FunctionRef::Unresolved(name) => name,
            FunctionRef::UnresolvedWithAnchor { name, .. } => name,
            FunctionRef::UnresolvedWithUri { name, .. } => name,
            FunctionRef::UnresolvedWithBoth { name, .. } => name,
            FunctionRef::Resolved { name, .. } => name,
        }
    }
}

impl<'a> Arguments<'a> {
    /// Create new empty arguments
    pub fn new() -> Self {
        Self {
            positional: Vec::new(),
            named: Vec::new(),
        }
    }

    /// Add a positional argument
    pub fn add_positional(&mut self, value: StructuralValue<'a>) {
        self.positional.push(value);
    }

    /// Add a named argument
    pub fn add_named(&mut self, name: impl Into<Cow<'a, str>>, value: StructuralValue<'a>) {
        self.named.push(NamedArg {
            name: name.into(),
            value,
        });
    }

    /// Check if arguments are empty
    pub fn is_empty(&self) -> bool {
        self.positional.is_empty() && self.named.is_empty()
    }

    /// Resolve all function references in arguments
    pub fn resolve_functions(&mut self, extensions: &'a SimpleExtensions) -> Result<(), String> {
        for value in &mut self.positional {
            value.resolve_functions(extensions)?;
        }
        for arg in &mut self.named {
            arg.value.resolve_functions(extensions)?;
        }
        Ok(())
    }
}

impl<'a> Default for Arguments<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> ColumnSpec<'a> {
    /// Create a named column
    pub fn named(name: impl Into<Cow<'a, str>>) -> Self {
        Self::Named {
            name: name.into(),
            type_spec: None,
        }
    }

    /// Create a named column with type
    pub fn named_with_type(
        name: impl Into<Cow<'a, str>>,
        type_spec: impl Into<Cow<'a, str>>,
    ) -> Self {
        Self::Named {
            name: name.into(),
            type_spec: Some(type_spec.into()),
        }
    }

    /// Create a reference column
    pub fn reference(index: i32) -> Self {
        Self::Reference(index)
    }

    /// Create an expression column
    pub fn expression(expr: StructuralValue<'a>) -> Self {
        Self::Expression(expr)
    }
}

// Display implementations for debugging and textification
impl<'a> fmt::Display for StructuralValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StructuralValue::String(s) => write!(f, "'{}'", s),
            StructuralValue::Integer(i) => write!(f, "{}", i),
            StructuralValue::Float(fl) => write!(f, "{}", fl),
            StructuralValue::Boolean(b) => write!(f, "{}", b),
            StructuralValue::Reference(r) => write!(f, "${}", r),
            StructuralValue::Function(func) => write!(f, "{}", func),
            StructuralValue::TableName(names) => {
                write!(
                    f,
                    "{}",
                    names
                        .iter()
                        .map(|n| n.as_ref())
                        .collect::<Vec<_>>()
                        .join(".")
                )
            }
            StructuralValue::Tuple(values) => {
                write!(
                    f,
                    "({})",
                    values
                        .iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            StructuralValue::List(values) => {
                write!(
                    f,
                    "[{}]",
                    values
                        .iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            StructuralValue::Missing(msg) => write!(f, "!{{{}}}", msg),
            StructuralValue::Enum(e) => write!(f, "&{}", e),
        }
    }
}

impl<'a> fmt::Display for FunctionRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionRef::Unresolved(name) => write!(f, "{}", name),
            FunctionRef::UnresolvedWithAnchor { name, anchor } => write!(f, "{}#{}", name, anchor),
            FunctionRef::UnresolvedWithUri { name, uri_anchor } => {
                write!(f, "{}@{}", name, uri_anchor)
            }
            FunctionRef::UnresolvedWithBoth {
                name,
                anchor,
                uri_anchor,
            } => {
                write!(f, "{}#{}@{}", name, anchor, uri_anchor)
            }
            FunctionRef::Resolved { name, .. } => write!(f, "{}", name),
        }
    }
}

impl<'a> fmt::Display for NamedArg<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}={}", self.name, self.value)
    }
}

impl<'a> fmt::Display for Arguments<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();

        // Add positional arguments
        for value in &self.positional {
            parts.push(value.to_string());
        }

        // Add named arguments
        for arg in &self.named {
            parts.push(arg.to_string());
        }

        write!(f, "{}", parts.join(", "))
    }
}

impl<'a> fmt::Display for ColumnSpec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ColumnSpec::Named { name, type_spec } => {
                if let Some(type_spec) = type_spec {
                    write!(f, "{}:{}", name, type_spec)
                } else {
                    write!(f, "{}", name)
                }
            }
            ColumnSpec::Reference(r) => write!(f, "${}", r),
            ColumnSpec::Expression(expr) => write!(f, "{}", expr),
        }
    }
}
