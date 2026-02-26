//! AST for the text representation of `substrait-explain`, used by the
//! [`crate::parser`] module.
//!
//! Parsing happens in two steps:
//!
//! 1. Conversion from text to the AST in the [`crate::parser`] module via
//!    LALRPOP, with syntactical validation.
//! 2. Conversion from the AST to a Protobuf [`substrait::proto::Plan`], via the
//!    [`crate::parser::lower`] module, with semantic validation, e.g. function
//!    references, anchors, column references, argument types/shapes match.
//!
//! This AST is intentionally close to the text syntax and does not encode all
//! semantic constraints; semantic validation happens in lowering.

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtensionUrnDeclaration {
    pub anchor: u32,
    pub urn: String,
}

impl ExtensionUrnDeclaration {
    pub fn new(anchor: u32, urn: impl Into<String>) -> Self {
        Self {
            anchor,
            urn: urn.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtensionDeclaration {
    pub anchor: u32,
    pub urn_anchor: u32,
    pub name: String,
}

impl ExtensionDeclaration {
    pub fn new(anchor: u32, urn_anchor: u32, name: impl Into<String>) -> Self {
        Self {
            anchor,
            urn_anchor,
            name: name.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Relation {
    pub name: RelationName,
    pub args: ArgList,
    pub outputs: Vec<Arg>,
}

impl Relation {
    pub fn new(name: RelationName, args: ArgList, outputs: Vec<Arg>) -> Self {
        Self {
            name,
            args,
            outputs,
        }
    }

    pub fn with_outputs(name: RelationName, outputs: Vec<Arg>) -> Self {
        Self::new(name, ArgList::default(), outputs)
    }

    pub fn with_entries(name: RelationName, entries: Vec<ArgEntry>, outputs: Vec<Arg>) -> Self {
        Self::new(name, ArgList::from_entries(entries), outputs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelationName {
    Standard(String),
    Extension(ExtensionType, String),
}

impl RelationName {
    pub fn standard(name: impl Into<String>) -> Self {
        Self::Standard(name.into())
    }

    pub fn extension(kind: ExtensionType, name: impl Into<String>) -> Self {
        Self::Extension(kind, name.into())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtensionType {
    Leaf,
    Single,
    Multi,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ArgList {
    /// Positional arguments in source order.
    pub positional: Vec<Arg>,
    /// Named arguments in source order.
    pub named: Vec<NamedArg>,
    /// True when positional arguments appear after named arguments.
    ///
    /// The parser records this and lowering turns it into a user-facing error.
    pub invalid_order: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arg {
    Expr(Expr),
    Enum(String),
    NamedColumn(String, TypeExpr),
    Tuple(Vec<Arg>),
    Wildcard,
}

impl Arg {
    pub fn expr(value: Expr) -> Self {
        Self::Expr(value)
    }

    pub fn enum_variant(value: impl Into<String>) -> Self {
        Self::Enum(value.into())
    }

    pub fn named_column(name: impl Into<String>, typ: TypeExpr) -> Self {
        Self::NamedColumn(name.into(), typ)
    }

    pub fn tuple(values: Vec<Arg>) -> Self {
        Self::Tuple(values)
    }

    pub fn wildcard() -> Self {
        Self::Wildcard
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedArg {
    pub name: String,
    pub value: Arg,
}

impl NamedArg {
    pub fn new(name: impl Into<String>, value: Arg) -> Self {
        Self {
            name: name.into(),
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    FieldRef(i32),
    Literal(Literal),
    FunctionCall(FunctionCall),
    IfThen(IfThenExpr),
    Identifier(String),
}

impl Expr {
    pub fn field_ref(index: i32) -> Self {
        Self::FieldRef(index)
    }

    pub fn literal(value: Literal) -> Self {
        Self::Literal(value)
    }

    pub fn function_call(value: FunctionCall) -> Self {
        Self::FunctionCall(value)
    }

    pub fn if_then(value: IfThenExpr) -> Self {
        Self::IfThen(value)
    }

    pub fn identifier(name: impl Into<String>) -> Self {
        Self::Identifier(name.into())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub anchor: Option<u32>,
    pub urn_anchor: Option<u32>,
    pub args: Vec<Expr>,
    pub output_type: Option<TypeExpr>,
}

impl FunctionCall {
    pub fn new(
        name: impl Into<String>,
        anchor: Option<u32>,
        urn_anchor: Option<u32>,
        args: Vec<Expr>,
        output_type: Option<TypeExpr>,
    ) -> Self {
        Self {
            name: name.into(),
            anchor,
            urn_anchor,
            args,
            output_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfThenExpr {
    pub clauses: Vec<(Expr, Expr)>,
    pub else_expr: Box<Expr>,
}

impl IfThenExpr {
    pub fn new(clauses: Vec<(Expr, Expr)>, else_expr: Expr) -> Self {
        Self {
            clauses,
            else_expr: Box::new(else_expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub value: LiteralValue,
    pub typ: Option<TypeExpr>,
}

impl Literal {
    pub fn new(value: LiteralValue, typ: Option<TypeExpr>) -> Self {
        Self { value, typ }
    }

    pub fn integer(value: i64, typ: Option<TypeExpr>) -> Self {
        Self::new(LiteralValue::Integer(value), typ)
    }

    pub fn float(value: f64, typ: Option<TypeExpr>) -> Self {
        Self::new(LiteralValue::Float(value), typ)
    }

    pub fn boolean(value: bool, typ: Option<TypeExpr>) -> Self {
        Self::new(LiteralValue::Boolean(value), typ)
    }

    pub fn string(value: impl Into<String>, typ: Option<TypeExpr>) -> Self {
        Self::new(LiteralValue::String(value.into()), typ)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    Simple {
        name: String,
        nullability: Nullability,
    },
    List {
        nullability: Nullability,
        inner: Box<TypeExpr>,
    },
    UserDefined {
        name: String,
        anchor: Option<u32>,
        urn_anchor: Option<u32>,
        nullability: Nullability,
        parameters: Vec<TypeExpr>,
    },
}

impl TypeExpr {
    pub fn simple(name: impl Into<String>, nullability: Nullability) -> Self {
        Self::Simple {
            name: name.into(),
            nullability,
        }
    }

    pub fn list(nullability: Nullability, inner: TypeExpr) -> Self {
        Self::List {
            nullability,
            inner: Box::new(inner),
        }
    }

    pub fn user_defined(
        name: impl Into<String>,
        anchor: Option<u32>,
        urn_anchor: Option<u32>,
        nullability: Nullability,
        parameters: Vec<TypeExpr>,
    ) -> Self {
        Self::UserDefined {
            name: name.into(),
            anchor,
            urn_anchor,
            nullability,
            parameters,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Nullability {
    #[default]
    Required,
    Nullable,
    Unspecified,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArgEntry {
    Positional(Arg),
    Named(NamedArg),
}

impl ArgEntry {
    pub fn positional(value: Arg) -> Self {
        Self::Positional(value)
    }

    pub fn named(name: impl Into<String>, value: Arg) -> Self {
        Self::Named(NamedArg::new(name, value))
    }
}

impl ArgList {
    /// Build an argument list while preserving order constraints.
    pub fn from_entries(entries: Vec<ArgEntry>) -> Self {
        let mut positional = Vec::new();
        let mut named = Vec::new();
        let mut seen_named = false;
        let mut invalid_order = false;

        for entry in entries {
            match entry {
                ArgEntry::Positional(arg) => {
                    if seen_named {
                        invalid_order = true;
                    }
                    positional.push(arg);
                }
                ArgEntry::Named(arg) => {
                    seen_named = true;
                    named.push(arg);
                }
            }
        }

        Self {
            positional,
            named,
            invalid_order,
        }
    }
}

/// Unescape a quoted identifier or string literal.
///
/// The input is expected to include surrounding quotes.
pub(crate) fn unescape_quoted(input: &str) -> String {
    if input.len() < 2 {
        return input.to_string();
    }

    let mut out = String::with_capacity(input.len().saturating_sub(2));
    let mut chars = input[1..input.len() - 1].chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(next) = chars.next() {
                match next {
                    'n' => out.push('\n'),
                    'r' => out.push('\r'),
                    't' => out.push('\t'),
                    _ => out.push(next),
                }
            }
        } else {
            out.push(c);
        }
    }
    out
}

impl fmt::Display for Nullability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Nullability::Required => Ok(()),
            Nullability::Nullable => write!(f, "?"),
            // Internal sentinel for "present but unspecified".
            Nullability::Unspecified => write!(f, "⁉"),
        }
    }
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExpr::Simple { name, nullability } => write!(f, "{name}{nullability}"),
            TypeExpr::List { nullability, inner } => write!(f, "list{nullability}<{inner}>"),
            TypeExpr::UserDefined {
                name,
                anchor,
                urn_anchor,
                nullability,
                parameters,
            } => {
                write!(f, "{name}")?;
                if let Some(anchor) = anchor {
                    write!(f, "#{anchor}")?;
                }
                if let Some(urn_anchor) = urn_anchor {
                    write!(f, "@{urn_anchor}")?;
                }
                write!(f, "{nullability}")?;
                if !parameters.is_empty() {
                    write!(f, "<")?;
                    for (idx, parameter) in parameters.iter().enumerate() {
                        if idx > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{parameter}")?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            LiteralValue::Integer(v) => write!(f, "{v}")?,
            LiteralValue::Float(v) => write!(f, "{v}")?,
            LiteralValue::Boolean(v) => write!(f, "{v}")?,
            LiteralValue::String(v) => write!(f, "'{}'", v.replace('\'', "\\'"))?,
        }
        if let Some(typ) = &self.typ {
            write!(f, ":{typ}")?;
        }
        Ok(())
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::FieldRef(idx) => write!(f, "${idx}"),
            Expr::Literal(lit) => write!(f, "{lit}"),
            Expr::Identifier(name) => write!(f, "{name}"),
            Expr::FunctionCall(call) => {
                write!(f, "{}", call.name)?;
                if let Some(anchor) = call.anchor {
                    write!(f, "#{anchor}")?;
                }
                if let Some(urn_anchor) = call.urn_anchor {
                    write!(f, "@{urn_anchor}")?;
                }
                write!(f, "(")?;
                for (idx, arg) in call.args.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")?;
                if let Some(typ) = &call.output_type {
                    write!(f, ":{typ}")?;
                }
                Ok(())
            }
            Expr::IfThen(if_then) => {
                write!(f, "if_then(")?;
                for (idx, (cond, result)) in if_then.clauses.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{cond} -> {result}")?;
                }
                write!(f, ", _ -> {})", if_then.else_expr)
            }
        }
    }
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arg::Expr(expr) => write!(f, "{expr}"),
            Arg::Enum(value) => write!(f, "&{value}"),
            Arg::NamedColumn(name, typ) => write!(f, "{name}:{typ}"),
            Arg::Tuple(args) => {
                write!(f, "(")?;
                for (idx, arg) in args.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            Arg::Wildcard => write!(f, "_"),
        }
    }
}
