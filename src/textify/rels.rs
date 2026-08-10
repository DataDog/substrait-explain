use std::borrow::Cow;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::fmt;

use prost::Message;
use substrait::proto::fetch_rel::{CountMode, OffsetMode};
use substrait::proto::plan_rel::RelType as PlanRelType;
use substrait::proto::read_rel::ReadType;
use substrait::proto::rel::RelType;
use substrait::proto::rel_common::EmitKind;
use substrait::proto::{
    AggregateRel, CrossRel, ExtensionLeafRel, ExtensionMultiRel, ExtensionSingleRel, FetchRel,
    FilterRel, JoinRel, NamedStruct, PlanRel, ProjectRel, ReadRel, Rel, RelCommon, RelRoot, SetRel,
    SortRel, join_rel, set_rel,
};

use super::addenda::AddendumLines;
use super::types::Name;
use super::values::{Arguments, NamedArg, Value, ValueEnum, decode_enum_field};
use super::{PlanError, Scope, Textify};
use crate::FormatError;
use crate::extensions::any::AnyRef;
use crate::extensions::{ExtensionContext, ExtensionError, ExtensionInput};

pub trait NamedRelation {
    fn name(&self) -> &'static str;
}

impl NamedRelation for Rel {
    fn name(&self) -> &'static str {
        match self.rel_type.as_ref() {
            None => "UnknownRel",
            Some(RelType::Read(_)) => "Read",
            Some(RelType::Filter(_)) => "Filter",
            Some(RelType::Project(_)) => "Project",
            Some(RelType::Fetch(_)) => "Fetch",
            Some(RelType::Aggregate(_)) => "Aggregate",
            Some(RelType::Sort(_)) => "Sort",
            Some(RelType::HashJoin(_)) => "HashJoin",
            Some(RelType::Exchange(_)) => "Exchange",
            Some(RelType::Join(_)) => "Join",
            Some(RelType::Set(_)) => "Set",
            Some(RelType::ExtensionLeaf(_)) => "ExtensionLeaf",
            Some(RelType::Cross(_)) => "Cross",
            Some(RelType::Reference(_)) => "Reference",
            Some(RelType::ExtensionSingle(_)) => "ExtensionSingle",
            Some(RelType::ExtensionMulti(_)) => "ExtensionMulti",
            Some(RelType::Write(_)) => "Write",
            Some(RelType::Ddl(_)) => "Ddl",
            Some(RelType::Update(_)) => "Update",
            Some(RelType::MergeJoin(_)) => "MergeJoin",
            Some(RelType::NestedLoopJoin(_)) => "NestedLoopJoin",
            Some(RelType::Window(_)) => "Window",
            Some(RelType::Expand(_)) => "Expand",
        }
    }
}

impl Textify for Rel {
    fn name() -> &'static str {
        "Rel"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        // delegates to `Relation` which carries `advanced_extension`, so the full
        // header → enhancement → children sequence is handled uniformly there.
        Relation::from_rel(self, ctx).textify(ctx, w)
    }
}

fn schema_to_values<'a>(schema: &'a NamedStruct) -> Vec<Value<'a>> {
    let mut fields = schema
        .r#struct
        .as_ref()
        .map(|s| s.types.iter())
        .into_iter()
        .flatten();
    let mut names = schema.names.iter();

    // let field_count = schema.r#struct.as_ref().map(|s| s.types.len()).unwrap_or(0);
    // let name_count = schema.names.len();

    let mut values = Vec::new();
    loop {
        let field = fields.next();
        let name = names.next().map(|n| Name(n));
        if field.is_none() && name.is_none() {
            break;
        }

        values.push(Value::Field(name, field));
    }

    values
}

/// How a relation header renders its output.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
enum OutputSyntax {
    /// Output columns are rendered as final visible output: `=> output_columns`.
    #[default]
    Implicit,
    /// Output columns are rendered as the direct output domain: `+> columns`,
    /// with `|> order` appended when an explicit emit mapping is present.
    Explicit,
}

struct Emitted<'a> {
    values: &'a [Value<'a>],
    emit: Option<&'a EmitKind>,
    output_syntax: Option<OutputSyntax>,
}

impl<'a> Emitted<'a> {
    pub fn columns(values: &'a [Value<'a>], emit: Option<&'a EmitKind>) -> Self {
        Self {
            values,
            emit,
            output_syntax: None,
        }
    }

    pub fn output_clause(
        values: &'a [Value<'a>],
        emit: Option<&'a EmitKind>,
        output_syntax: OutputSyntax,
    ) -> Self {
        Self {
            values,
            emit,
            output_syntax: Some(output_syntax),
        }
    }

    fn write_output_clause<S: Scope, W: fmt::Write>(
        &self,
        ctx: &S,
        w: &mut W,
        output_syntax: OutputSyntax,
    ) -> fmt::Result {
        match output_syntax {
            OutputSyntax::Implicit => {
                write!(w, "=> ")?;
                self.write_implicit_columns(ctx, w)
            }
            OutputSyntax::Explicit => {
                write!(w, "+> ")?;
                self.write_direct_columns(ctx, w)?;
                self.write_emit_suffix(ctx, w)
            }
        }
    }

    fn write_direct_columns<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}", ctx.separated(self.values.iter(), ", "))
    }

    fn write_implicit_columns<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        if ctx.options().show_emit {
            return self.write_direct_columns(ctx, w);
        }

        let indices = match &self.emit {
            Some(EmitKind::Emit(e)) => &e.output_mapping,
            Some(EmitKind::Direct(_)) => return self.write_direct_columns(ctx, w),
            None => return self.write_direct_columns(ctx, w),
        };

        for (i, &index) in indices.iter().enumerate() {
            if i > 0 {
                write!(w, ", ")?;
            }

            match self.values.get(index as usize) {
                Some(value) => write!(w, "{}", ctx.display(value))?,
                None => write!(w, "{}", ctx.failure(PlanError::invalid(
                    "Emitted",
                    Some("output_mapping"),
                    format!(
                        "Output mapping index {} is out of bounds for values collection of size {}",
                        index, self.values.len()
                    )
                )))?,
            }
        }

        Ok(())
    }

    fn write_emit_suffix<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let Some(EmitKind::Emit(emit)) = self.emit else {
            return Ok(());
        };
        let mapping = emit
            .output_mapping
            .iter()
            .copied()
            .map(Value::Reference)
            .collect::<Vec<_>>();
        write!(w, " |> {}", ctx.separated(mapping.iter(), ", "))
    }
}

impl<'a> Textify for Emitted<'a> {
    fn name() -> &'static str {
        "Emitted"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self.output_syntax {
            Some(output_syntax) => self.write_output_clause(ctx, w, output_syntax),
            None => self.write_implicit_columns(ctx, w),
        }
    }
}

/// The argument section of a relation header.
#[derive(Debug, Clone)]
pub enum RelationArgs<'a> {
    /// `arg, arg, name=arg` inline inside the header's `[...]`.
    Inline(Arguments<'a>),
    /// One `- row` per line, used by `Read:Virtual` once it has enough rows to
    /// be worth spreading out. Any named arguments follow the rows, one per
    /// line, in the same `- name=value` form.
    Rows {
        rows: Vec<Value<'a>>,
        named: Vec<NamedArg<'a>>,
    },
}

impl<'a> RelationArgs<'a> {
    /// An inline argument list, the layout used by every relation but a
    /// multi-row `Read:Virtual`.
    pub fn inline(positional: Vec<Value<'a>>, named: Vec<NamedArg<'a>>) -> Self {
        RelationArgs::Inline(Arguments::new(positional, named))
    }

    /// A row-per-line argument list (`- arg` per line) used for `Read:Virtual`
    /// with many rows. Named arguments, if any, follow the rows.
    pub fn rows(rows: Vec<Value<'a>>, named: Vec<NamedArg<'a>>) -> Self {
        RelationArgs::Rows { rows, named }
    }
}

pub struct Relation<'a> {
    pub name: Cow<'a, str>,
    /// Arguments to the relation, if any.
    ///
    /// - `None` means this relation does not take arguments, and the argument
    ///   section is omitted entirely.
    /// - `Some(RelationArgs::Inline(args))` with both vectors empty means the
    ///   relation takes arguments, but none are provided; this will print as
    ///   `_ => ...`.
    /// - `Some(RelationArgs::Inline(args))` with non-empty vectors will print
    ///   with positional arguments first, then named arguments, separated by commas.
    /// - `Some(RelationArgs::Rows { .. })` prints one row per line, followed by
    ///   any named arguments, one per line.
    pub arguments: Option<RelationArgs<'a>>,
    /// The columns emitted by this relation, pre-emit - the 'direct' column
    /// output.
    pub columns: Vec<Value<'a>>,
    /// The emit kind, if any. If none, use the columns directly.
    pub emit: Option<&'a EmitKind>,
    /// Whether output columns are rendered as visible output or as a direct
    /// output domain plus optional explicit emit mapping.
    output_syntax: OutputSyntax,
    /// `+`-prefixed addendum lines to emit between this relation's header and
    /// children.  This owns the canonical ordering for `+ Ext`, `+ Enh`, and
    /// `+ Opt` lines rather than making the generic relation shape grow one
    /// field per addendum kind.
    addenda: AddendumLines,
    /// The input relations.
    pub children: Vec<Option<Relation<'a>>>,
}

impl Textify for Relation<'_> {
    fn name() -> &'static str {
        "Relation"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        self.write_header(ctx, w)?;
        let child_scope = ctx.push_indent();
        self.addenda.textify(&child_scope, w)?;
        self.write_children(ctx, w)?;
        Ok(())
    }
}

impl Relation<'_> {
    /// Write the header for this relation, e.g. `Filter[$0 => $0]`.
    ///
    /// Usually a single line, but an argument list of [`RelationArgs::Rows`]
    /// (used by `Read:Virtual` with many rows) spans several lines:
    ///
    /// ```text
    /// Read:Virtual[
    ///   - (1, 'alice'),
    ///   - (2, 'bob')
    ///   - => id:i64, name:string]
    /// ```
    ///
    /// Does not write a trailing newline; callers are responsible for any
    /// newline that follows (either from an addendum or from the next child).
    pub fn write_header<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let indent = ctx.indent();
        let name = &self.name;
        match &self.arguments {
            None => {
                let cols = Emitted::columns(&self.columns, self.emit);
                let cols = ctx.display(&cols);
                write!(w, "{indent}{name}[{cols}]")
            }
            Some(RelationArgs::Rows { rows, named }) => {
                // One `- row` per line, one indent level deeper, with a
                // trailing comma when another row or named argument follows,
                // then `- <output> cols]`.
                let child = ctx.push_indent();
                let child_indent = child.indent();
                writeln!(w, "{indent}{name}[")?;
                let last = rows.len().saturating_sub(1);
                for (i, row) in rows.iter().enumerate() {
                    let row = ctx.display(row);
                    let comma = if i == last && named.is_empty() {
                        ""
                    } else {
                        ","
                    };
                    writeln!(w, "{child_indent}- {row}{comma}")?;
                }
                let last = named.len().saturating_sub(1);
                for (i, named_arg) in named.iter().enumerate() {
                    let named_arg = ctx.display(named_arg);
                    let comma = if i == last { "" } else { "," };
                    writeln!(w, "{child_indent}- {named_arg}{comma}")?;
                }
                let output = Emitted::output_clause(&self.columns, self.emit, self.output_syntax);
                let output = ctx.display(&output);
                write!(w, "{child_indent}- {output}]")
            }
            Some(RelationArgs::Inline(args)) => {
                let args = ctx.display(args);
                let output = Emitted::output_clause(&self.columns, self.emit, self.output_syntax);
                let output = ctx.display(&output);
                write!(w, "{indent}{name}[{args} {output}]")
            }
        }
    }

    /// Write each child relation at one indent level deeper than `ctx`.
    /// Each child is preceded by a newline.
    pub fn write_children<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let child_scope = ctx.push_indent();
        for child in self.children.iter().flatten() {
            writeln!(w)?;
            child.textify(&child_scope, w)?;
        }
        Ok(())
    }
}

impl<'a> Relation<'a> {
    pub fn emitted(&self) -> usize {
        match self.emit {
            Some(EmitKind::Emit(e)) => e.output_mapping.len(),
            Some(EmitKind::Direct(_)) => self.columns.len(),
            None => self.columns.len(),
        }
    }
}

impl<'a> Relation<'a> {
    fn from_read<S: Scope>(rel: &'a ReadRel, ctx: &S) -> Self {
        let columns = read_columns(rel);
        let emit = rel.common.as_ref().and_then(|c| c.emit_kind.as_ref());

        match &rel.read_type {
            Some(ReadType::NamedTable(table)) => {
                let table_name = Value::TableName(table.names.iter().map(|n| Name(n)).collect());
                // XXX: For `ReadRel`s, we use `=>` if emit is None, `+>` if
                // `emit` is `Some(Direct)`, and `+> … |>` if emit is
                // `Some(Remap(…))`. However, we ignore the
                // `OutputOptions::show_emit` option, and we haven't yet
                // supported other operators; at some point, we should figure
                // out our policy here and clean this up.
                let output_syntax = if emit.is_some() {
                    OutputSyntax::Explicit
                } else {
                    OutputSyntax::Implicit
                };
                Relation {
                    name: Cow::Borrowed("Read"),
                    arguments: Some(RelationArgs::inline(vec![table_name], vec![])),
                    columns,
                    emit,
                    output_syntax,
                    addenda: AddendumLines::from_advanced_extension(
                        ctx,
                        rel.advanced_extension.as_ref(),
                    ),
                    children: vec![],
                }
            }
            Some(ReadType::VirtualTable(vt)) => {
                let row_count = vt.expressions.len();
                let mut positional: Vec<Value> = vt
                    .expressions
                    .iter()
                    .map(|row| Value::Tuple(row.fields.iter().map(Value::Expression).collect()))
                    .collect();
                let mut named = vec![];
                if let Some(filter) = rel.filter.as_ref() {
                    named.push(NamedArg {
                        name: Cow::Borrowed("filter"),
                        value: Value::Expression(filter.as_ref()),
                    });
                }
                if positional.is_empty() && !named.is_empty() {
                    positional.push(Value::EmptyGroup);
                }

                // Emit many rows across multiple lines for readability, based on
                // a configurable threshold (default = 3). An empty table has no
                // rows to spread out and is written `_`, so it stays inline
                // regardless of the threshold — the row layout has no `_` form.
                let multiline =
                    row_count > 0 && row_count >= ctx.options().virtual_table_multiline_threshold;
                let arguments = if multiline {
                    RelationArgs::rows(positional, named)
                } else {
                    RelationArgs::inline(positional, named)
                };

                Relation {
                    name: Cow::Borrowed("Read:Virtual"),
                    arguments: Some(arguments),
                    columns,
                    emit,
                    output_syntax: OutputSyntax::Implicit,
                    addenda: AddendumLines::from_advanced_extension(
                        ctx,
                        rel.advanced_extension.as_ref(),
                    ),
                    children: vec![],
                }
            }
            Some(ReadType::ExtensionTable(table)) => {
                let decoded = match table.detail.as_ref().map(AnyRef::from) {
                    Some(detail) => ctx.extension_registry().decode_extension_table(detail),
                    None => Err(ExtensionError::MissingDetail),
                };

                Relation {
                    name: Cow::Borrowed("Read:Extension"),
                    arguments: None,
                    columns,
                    emit,
                    output_syntax: OutputSyntax::Implicit,
                    addenda: AddendumLines::extension_table(
                        ctx,
                        decoded,
                        rel.advanced_extension.as_ref(),
                    ),
                    children: vec![],
                }
            }
            other => {
                let err = PlanError::unimplemented(
                    "ReadRel",
                    Some("read_type"),
                    format!("Unsupported read type {other:?}"),
                );
                Relation {
                    name: Cow::Borrowed("Read"),
                    arguments: Some(RelationArgs::inline(vec![Value::Missing(err)], vec![])),
                    columns,
                    emit,
                    output_syntax: OutputSyntax::Implicit,
                    addenda: AddendumLines::from_advanced_extension(
                        ctx,
                        rel.advanced_extension.as_ref(),
                    ),
                    children: vec![],
                }
            }
        }
    }
}

fn read_columns<'a>(rel: &'a ReadRel) -> Vec<Value<'a>> {
    match rel.base_schema {
        Some(ref schema) => schema_to_values(schema),
        None => {
            let err =
                PlanError::unimplemented("ReadRel", Some("base_schema"), "Base schema is required");
            vec![Value::Missing(err)]
        }
    }
}

pub fn get_emit(rel: Option<&RelCommon>) -> Option<&EmitKind> {
    rel.as_ref().and_then(|c| c.emit_kind.as_ref())
}

impl<'a> Relation<'a> {
    /// Convert a vector of relation references into their structured form.
    ///
    /// Returns a list of children (with None for ones missing), and a count of input columns.
    pub fn convert_children<S: Scope>(
        refs: Vec<Option<&'a Rel>>,
        ctx: &S,
    ) -> (Vec<Option<Relation<'a>>>, usize) {
        let mut children = vec![];
        let mut inputs = 0;

        for maybe_rel in refs {
            match maybe_rel {
                Some(rel) => {
                    let child = Relation::from_rel(rel, ctx);
                    inputs += child.emitted();
                    children.push(Some(child));
                }
                None => children.push(None),
            }
        }

        (children, inputs)
    }
}

impl<'a> Relation<'a> {
    fn from_filter<S: Scope>(rel: &'a FilterRel, ctx: &S) -> Self {
        let condition = rel
            .condition
            .as_ref()
            .map(|c| Value::Expression(c.as_ref()));
        let condition = Value::expect(condition, || {
            PlanError::unimplemented("FilterRel", Some("condition"), "Condition is None")
        });
        let positional = vec![condition];
        let arguments = Some(RelationArgs::inline(positional, vec![]));
        let emit = get_emit(rel.common.as_ref());
        let (children, columns) = Relation::convert_children(vec![rel.input.as_deref()], ctx);
        let columns = (0..columns).map(|i| Value::Reference(i as i32)).collect();

        Relation {
            name: Cow::Borrowed("Filter"),
            arguments,
            columns,
            emit,
            output_syntax: OutputSyntax::Implicit,
            addenda: AddendumLines::from_advanced_extension(ctx, rel.advanced_extension.as_ref()),
            children,
        }
    }

    fn from_project<S: Scope>(rel: &'a ProjectRel, ctx: &S) -> Self {
        let (children, input_columns) = Relation::convert_children(vec![rel.input.as_deref()], ctx);
        let mut columns: Vec<Value> = vec![];
        for i in 0..input_columns {
            columns.push(Value::Reference(i as i32));
        }
        for expr in &rel.expressions {
            columns.push(Value::Expression(expr));
        }

        Relation {
            name: Cow::Borrowed("Project"),
            arguments: None,
            columns,
            emit: get_emit(rel.common.as_ref()),
            output_syntax: OutputSyntax::Implicit,
            addenda: AddendumLines::from_advanced_extension(ctx, rel.advanced_extension.as_ref()),
            children,
        }
    }

    pub fn from_rel<S: Scope>(rel: &'a Rel, ctx: &S) -> Self {
        match rel.rel_type.as_ref() {
            Some(RelType::Read(r)) => Relation::from_read(r, ctx),
            Some(RelType::Filter(r)) => Relation::from_filter(r, ctx),
            Some(RelType::Project(r)) => Relation::from_project(r, ctx),
            Some(RelType::Aggregate(r)) => Relation::from_aggregate(r, ctx),
            Some(RelType::Sort(r)) => Relation::from_sort(r, ctx),
            Some(RelType::Fetch(r)) => Relation::from_fetch(r, ctx),
            Some(RelType::Join(r)) => Relation::from_join(r, ctx),
            Some(RelType::Set(r)) => Relation::from_set(r, ctx),
            Some(RelType::Cross(r)) => Relation::from_cross(r, ctx),
            Some(RelType::ExtensionLeaf(r)) => Relation::from_extension_leaf(r, ctx),
            Some(RelType::ExtensionSingle(r)) => Relation::from_extension_single(r, ctx),
            Some(RelType::ExtensionMulti(r)) => Relation::from_extension_multi(r, ctx),
            _ => {
                let name = rel.name();
                let token = ctx.failure(FormatError::Format(PlanError::unimplemented(
                    "Rel",
                    Some(name),
                    format!("{name} is not yet supported in the text format"),
                )));
                Relation {
                    name: Cow::Owned(format!("{token}")),
                    arguments: None,
                    columns: vec![],
                    emit: None,
                    output_syntax: OutputSyntax::Implicit,
                    addenda: AddendumLines::none(),
                    children: vec![],
                }
            }
        }
    }

    fn from_extension_leaf<S: Scope>(rel: &'a ExtensionLeafRel, ctx: &S) -> Self {
        Relation::from_extension(
            "ExtensionLeaf",
            rel.detail.as_ref().map(AnyRef::from),
            vec![],
            ctx,
        )
    }

    fn from_extension_single<S: Scope>(rel: &'a ExtensionSingleRel, ctx: &S) -> Self {
        Relation::from_extension(
            "ExtensionSingle",
            rel.detail.as_ref().map(AnyRef::from),
            vec![rel.input.as_deref()],
            ctx,
        )
    }

    fn from_extension_multi<S: Scope>(rel: &'a ExtensionMultiRel, ctx: &S) -> Self {
        let mut child_refs: Vec<Option<&'a Rel>> = vec![];
        for input in &rel.inputs {
            child_refs.push(Some(input));
        }
        Relation::from_extension(
            "ExtensionMulti",
            rel.detail.as_ref().map(AnyRef::from),
            child_refs,
            ctx,
        )
    }

    fn from_extension<S: Scope>(
        ext_type: &'static str,
        detail: Option<AnyRef<'a>>,
        child_refs: Vec<Option<&'a Rel>>,
        ctx: &S,
    ) -> Self {
        let (children, _) = Relation::convert_children(child_refs, ctx);
        let inputs = children
            .iter()
            .filter_map(|child| {
                child
                    .as_ref()
                    .map(|child| ExtensionInput::new(child.emitted()))
            })
            .collect::<Vec<_>>();
        let context = ExtensionContext::new(&inputs);
        let decoded = match detail {
            Some(detail) => ctx
                .extension_registry()
                .decode_with_context(detail, &context),
            None => Err(ExtensionError::MissingDetail),
        };

        match decoded {
            Ok((name, args)) => {
                let mut positional = vec![];
                for value in args.positional {
                    positional.push(Value::ExtensionArgument(value));
                }
                let mut named = vec![];
                for (key, value) in args.named {
                    named.push(NamedArg {
                        name: Cow::Owned(key),
                        value: Value::ExtensionArgument(value),
                    });
                }
                let columns = args
                    .output_columns
                    .into_iter()
                    .map(Value::ExtColumn)
                    .collect();
                Relation {
                    name: Cow::Owned(format!("{}:{}", ext_type, name)),
                    arguments: Some(RelationArgs::inline(positional, named)),
                    columns,
                    emit: None,
                    output_syntax: OutputSyntax::Implicit,
                    // Extension relations use `detail` rather than
                    // `advanced_extension`; the field does not exist on these
                    // proto types.
                    addenda: AddendumLines::none(),
                    children,
                }
            }
            Err(error) => Relation {
                name: Cow::Borrowed(ext_type),
                arguments: None,
                columns: vec![Value::Missing(PlanError::invalid(
                    "extension",
                    None::<&str>,
                    error.to_string(),
                ))],
                emit: None,
                output_syntax: OutputSyntax::Implicit,
                addenda: AddendumLines::none(),
                children,
            },
        }
    }

    /// Convert an AggregateRel to a Relation for textification.
    ///
    /// The conversion follows this logic:
    /// 1. Arguments: Group-by expressions (as Value::Expression)
    /// 2. Columns: All possible outputs in order:
    ///    - First: Group-by field references (Value::Reference)
    ///    - Then: Aggregate function measures (Value::AggregateFunction)
    /// 3. Emit: Uses the relation's emit mapping to select which outputs to display
    /// 4. Children: The input relation
    fn from_aggregate<S: Scope>(rel: &'a AggregateRel, ctx: &S) -> Self {
        let mut grouping_sets: Vec<Vec<Value>> = vec![]; // the Groupings in the Aggregate
        let expression_list: Vec<Value>; // grouping_expressions defined on Aggregate

        // if rel.grouping_expressions is empty, the deprecated rel.groupings.grouping_expressions might be set
        // If *both* the deprecated `rel.groupings.grouping_expressions` and `rel.grouping_expressions` are
        // set, then we silently ignore the deprecated one.
        #[allow(deprecated)]
        if rel.grouping_expressions.is_empty()
            && !rel.groupings.is_empty()
            && !rel.groupings[0].grouping_expressions.is_empty()
        {
            (expression_list, grouping_sets) = Relation::get_grouping_sets(rel);
        } else {
            expression_list = rel
                .grouping_expressions
                .iter()
                .map(Value::Expression)
                .collect::<Vec<_>>(); // already a list of the unique expressions
            for group in &rel.groupings {
                let mut grouping_set: Vec<Value> = vec![];
                for i in &group.expression_references {
                    let value = match rel.grouping_expressions.get(*i as usize) {
                        Some(expr) => Value::Expression(expr),
                        None => Value::Missing(PlanError::invalid(
                            "AggregateRel",
                            Some("groupings.expression_references"),
                            format!(
                                "expression_reference {i} is out of bounds for grouping_expressions of length {}",
                                rel.grouping_expressions.len()
                            ),
                        )),
                    };
                    grouping_set.push(value);
                }
                grouping_sets.push(grouping_set);
            }
            // no defined groupings means there is global group by
            if rel.groupings.is_empty() {
                grouping_sets.push(vec![]);
            }
        }

        let is_single = grouping_sets.len() == 1;
        let mut positional: Vec<Value> = vec![];
        for g in grouping_sets {
            if g.is_empty() {
                positional.push(Value::EmptyGroup);
            } else if is_single {
                // Single non-empty grouping set: spread expressions directly without parens
                positional.extend(g);
            } else {
                positional.push(Value::Tuple(g));
            }
        }

        // adding the grouping_sets as a list of Arguments to Aggregate Rel
        let arguments = Some(RelationArgs::inline(positional, vec![]));

        // The columns are the direct outputs of this relation (before emit)
        let mut all_outputs: Vec<Value> = expression_list;

        // Then, add all measures (aggregate functions)
        // These are indexed after the group-by fields
        for m in &rel.measures {
            if let Some(agg_fn) = m.measure.as_ref() {
                all_outputs.push(Value::AggregateFunction(agg_fn));
            }
        }
        let emit = get_emit(rel.common.as_ref());
        let (children, _) = Relation::convert_children(vec![rel.input.as_deref()], ctx);

        Relation {
            name: Cow::Borrowed("Aggregate"),
            arguments,
            columns: all_outputs,
            emit,
            output_syntax: OutputSyntax::Implicit,
            addenda: AddendumLines::from_advanced_extension(ctx, rel.advanced_extension.as_ref()),
            children,
        }
    }

    fn get_grouping_sets(rel: &'a AggregateRel) -> (Vec<Value<'a>>, Vec<Vec<Value<'a>>>) {
        let mut grouping_sets: Vec<Vec<Value>> = vec![];
        let mut expression_list: Vec<Value> = Vec::new();

        // groupings might have the same expressions in their set, so we track
        // which byte-encoded expressions have already been added to
        // `expression_list` to keep it deduplicated.
        let mut seen_expressions = HashSet::new();

        for group in &rel.groupings {
            let mut grouping_set: Vec<Value> = vec![];
            #[allow(deprecated)]
            for exp in &group.grouping_expressions {
                // TODO: use a better key here than encoding to bytes.
                // Ideally, substrait-rs would support `PartialEq` and `Hash`,
                // but as there isn't an easy way to do that now, we'll skip.
                if seen_expressions.insert(exp.encode_to_vec()) {
                    expression_list.push(Value::Expression(exp)); // new unique expression found
                }
                grouping_set.push(Value::Expression(exp));
            }
            grouping_sets.push(grouping_set);
        }
        (expression_list, grouping_sets)
    }
}

impl Textify for RelRoot {
    fn name() -> &'static str {
        "RelRoot"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let names = self.names.iter().map(|n| Name(n)).collect::<Vec<_>>();

        write!(
            w,
            "{}Root[{}]",
            ctx.indent(),
            ctx.separated(names.iter(), ", ")
        )?;
        let child_scope = ctx.push_indent();
        for child in self.input.iter() {
            writeln!(w)?;
            child.textify(&child_scope, w)?;
        }

        Ok(())
    }
}

impl Textify for PlanRelType {
    fn name() -> &'static str {
        "PlanRelType"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            PlanRelType::Rel(rel) => rel.textify(ctx, w),
            PlanRelType::Root(root) => root.textify(ctx, w),
        }
    }
}

impl Textify for PlanRel {
    fn name() -> &'static str {
        "PlanRel"
    }

    /// Write the relation as a string. Inputs are ignored - those are handled
    /// separately.
    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}", ctx.expect(self.rel_type.as_ref()))
    }
}

impl<'a> Relation<'a> {
    fn from_sort<S: Scope>(rel: &'a SortRel, ctx: &S) -> Self {
        let (children, input_columns) = Relation::convert_children(vec![rel.input.as_deref()], ctx);
        let mut positional = vec![];
        for sort_field in &rel.sorts {
            positional.push(Value::from(sort_field));
        }
        let arguments = Some(RelationArgs::inline(positional, vec![]));
        // The columns are the direct outputs of this relation (before emit)
        let mut col_values = vec![];
        for i in 0..input_columns {
            col_values.push(Value::Reference(i as i32));
        }
        let emit = get_emit(rel.common.as_ref());
        Relation {
            name: Cow::Borrowed("Sort"),
            arguments,
            columns: col_values,
            emit,
            output_syntax: OutputSyntax::Implicit,
            addenda: AddendumLines::from_advanced_extension(ctx, rel.advanced_extension.as_ref()),
            children,
        }
    }

    fn from_fetch<S: Scope>(rel: &'a FetchRel, ctx: &S) -> Self {
        let (children, input_columns) = Relation::convert_children(vec![rel.input.as_deref()], ctx);
        let mut named_args: Vec<NamedArg> = vec![];
        match &rel.count_mode {
            Some(CountMode::CountExpr(expr)) => {
                named_args.push(NamedArg {
                    name: Cow::Borrowed("limit"),
                    value: Value::Expression(expr),
                });
            }
            #[allow(deprecated)]
            Some(CountMode::Count(val)) => {
                named_args.push(NamedArg {
                    name: Cow::Borrowed("limit"),
                    value: Value::Integer(*val),
                });
            }
            None => {}
        }
        if let Some(offset) = &rel.offset_mode {
            match offset {
                OffsetMode::OffsetExpr(expr) => {
                    named_args.push(NamedArg {
                        name: Cow::Borrowed("offset"),
                        value: Value::Expression(expr),
                    });
                }
                #[allow(deprecated)]
                OffsetMode::Offset(val) => {
                    named_args.push(NamedArg {
                        name: Cow::Borrowed("offset"),
                        value: Value::Integer(*val),
                    });
                }
            }
        }

        let emit = get_emit(rel.common.as_ref());
        // Fetch is passthrough — direct output is all input columns.
        let columns: Vec<Value> = (0..input_columns)
            .map(|i| Value::Reference(i as i32))
            .collect();
        Relation {
            name: Cow::Borrowed("Fetch"),
            arguments: Some(RelationArgs::inline(vec![], named_args)),
            columns,
            emit,
            output_syntax: OutputSyntax::Implicit,
            addenda: AddendumLines::from_advanced_extension(ctx, rel.advanced_extension.as_ref()),
            children,
        }
    }
}

fn join_output_columns(
    join_type: join_rel::JoinType,
    left_columns: usize,
    right_columns: usize,
) -> Vec<Value<'static>> {
    let total_columns = match join_type {
        // Inner, Left, Right, Outer joins output columns from both sides
        join_rel::JoinType::Inner
        | join_rel::JoinType::Left
        | join_rel::JoinType::Right
        | join_rel::JoinType::Outer => left_columns + right_columns,

        // Left semi/anti joins only output columns from the left side
        join_rel::JoinType::LeftSemi | join_rel::JoinType::LeftAnti => left_columns,

        // Right semi/anti joins output columns from the right side
        join_rel::JoinType::RightSemi | join_rel::JoinType::RightAnti => right_columns,

        // Single joins behave like semi joins
        join_rel::JoinType::LeftSingle => left_columns,
        join_rel::JoinType::RightSingle => right_columns,

        // Mark joins output base columns plus one mark column
        join_rel::JoinType::LeftMark => left_columns + 1,
        join_rel::JoinType::RightMark => right_columns + 1,

        // Unspecified - fallback to all columns
        join_rel::JoinType::Unspecified => left_columns + right_columns,
    };

    // Output is always a contiguous range starting from $0
    (0..total_columns)
        .map(|i| Value::Reference(i as i32))
        .collect()
}

impl<'a> Relation<'a> {
    fn from_join<S: Scope>(rel: &'a JoinRel, ctx: &S) -> Self {
        let (children, _total_columns) =
            Relation::convert_children(vec![rel.left.as_deref(), rel.right.as_deref()], ctx);

        // convert_children should preserve input vector length
        assert_eq!(
            children.len(),
            2,
            "convert_children should return same number of elements as input"
        );

        // Calculate left and right column counts separately
        let left_columns = match &children[0] {
            Some(child) => child.emitted(),
            None => 0,
        };
        let right_columns = match &children[1] {
            Some(child) => child.emitted(),
            None => 0,
        };

        // Convert join type from protobuf i32 to enum value
        // JoinType is stored as i32 in protobuf, convert to typed enum for processing
        let (join_type, join_type_value) = match join_rel::JoinType::try_from(rel.r#type) {
            Ok(join_type) => {
                let join_type_value = match join_type.as_enum_str() {
                    Ok(s) => Value::Enum(s),
                    Err(e) => Value::Missing(e),
                };
                (join_type, join_type_value)
            }
            Err(_) => {
                // Use Unspecified for the join_type but create an error for the join_type_value
                let join_type_error = Value::Missing(PlanError::invalid(
                    "JoinRel",
                    Some("type"),
                    format!("Unknown join type: {}", rel.r#type),
                ));
                (join_rel::JoinType::Unspecified, join_type_error)
            }
        };

        // Join condition
        let condition = rel
            .expression
            .as_ref()
            .map(|c| Value::Expression(c.as_ref()));
        let condition = Value::expect(condition, || {
            PlanError::unimplemented("JoinRel", Some("expression"), "Join condition is None")
        });

        let positional = vec![join_type_value, condition];
        let mut named = vec![];
        if let Some(post_join_filter) = rel.post_join_filter.as_ref() {
            named.push(NamedArg {
                name: Cow::Borrowed("post_filter"),
                value: Value::Expression(post_join_filter.as_ref()),
            });
        }
        let arguments = Some(RelationArgs::inline(positional, named));

        let emit = get_emit(rel.common.as_ref());
        let columns = join_output_columns(join_type, left_columns, right_columns);

        Relation {
            name: Cow::Borrowed("Join"),
            arguments,
            columns,
            emit,
            output_syntax: OutputSyntax::Implicit,
            addenda: AddendumLines::from_advanced_extension(ctx, rel.advanced_extension.as_ref()),
            children,
        }
    }

    fn from_set<S: Scope>(rel: &'a SetRel, ctx: &S) -> Self {
        let child_refs: Vec<Option<&'a Rel>> = rel.inputs.iter().map(Some).collect();
        let (children, total_columns) = Relation::convert_children(child_refs, ctx);

        // Set relation output has the same width as any one of its inputs
        // (it's a pass-through, not a concatenation like Join).
        // TODO: we may want to validate that all inputs have the same width
        // (and schema, if possible...), and provide a warning if they do not.
        let width = if children.is_empty() {
            0
        } else {
            total_columns / children.len()
        };

        let op_value = decode_enum_field::<set_rel::SetOp>(rel.op, "SetRel", "op");

        let arguments = Some(RelationArgs::inline(vec![op_value], vec![]));
        let emit = get_emit(rel.common.as_ref());
        let columns = (0..width).map(|i| Value::Reference(i as i32)).collect();

        Relation {
            name: Cow::Borrowed("Set"),
            arguments,
            columns,
            emit,
            output_syntax: OutputSyntax::Implicit,
            addenda: AddendumLines::from_advanced_extension(ctx, rel.advanced_extension.as_ref()),
            children,
        }
    }

    fn from_cross<S: Scope>(rel: &'a CrossRel, ctx: &S) -> Self {
        let (children, total_columns) =
            Relation::convert_children(vec![rel.left.as_deref(), rel.right.as_deref()], ctx);

        // Output columns concatenate the left and right inputs; there is no
        // join-type column dropping, since CrossRel has none.
        let columns = (0..total_columns)
            .map(|i| Value::Reference(i as i32))
            .collect();

        Relation {
            name: Cow::Borrowed("Cross"),
            arguments: None,
            columns,
            emit: get_emit(rel.common.as_ref()),
            output_syntax: OutputSyntax::Implicit,
            addenda: AddendumLines::from_advanced_extension(ctx, rel.advanced_extension.as_ref()),
            children,
        }
    }
}

#[cfg(test)]
mod tests {
    use substrait::proto::aggregate_rel::Grouping;
    use substrait::proto::expression::literal::LiteralType;
    use substrait::proto::expression::{Literal, RexType, ScalarFunction};
    use substrait::proto::function_argument::ArgType;
    use substrait::proto::read_rel::{NamedTable, ReadType};
    use substrait::proto::rel_common::{Direct, Emit};
    use substrait::proto::r#type::{self as ptype, Boolean, I64, Kind, Nullability, Struct};
    use substrait::proto::{
        AggregateFunction, Expression, FunctionArgument, NamedStruct, ReadRel, ReferenceRel, Type,
        aggregate_rel,
    };

    use super::*;
    use crate::fixtures::TestContext;
    use crate::parser::expressions::FieldIndex;
    use crate::textify::expressions::Reference;
    use crate::textify::foundation::FormatErrorType;

    #[test]
    fn test_read_rel() {
        let ctx = TestContext::new();

        // Create a simple ReadRel with a NamedStruct schema
        let read_rel = ReadRel {
            common: None,
            base_schema: Some(NamedStruct {
                names: vec!["col1".into(), "column 2".into()],
                r#struct: Some(Struct {
                    type_variation_reference: 0,
                    types: vec![
                        Type {
                            kind: Some(Kind::I32(ptype::I32 {
                                type_variation_reference: 0,
                                nullability: Nullability::Nullable as i32,
                            })),
                        },
                        Type {
                            kind: Some(Kind::String(ptype::String {
                                type_variation_reference: 0,
                                nullability: Nullability::Nullable as i32,
                            })),
                        },
                    ],
                    nullability: Nullability::Nullable as i32,
                }),
            }),
            filter: None,
            best_effort_filter: None,
            projection: None,
            advanced_extension: None,
            read_type: Some(ReadType::NamedTable(NamedTable {
                names: vec!["some_db".into(), "test_table".into()],
                advanced_extension: None,
            })),
        };

        let rel = Rel {
            rel_type: Some(RelType::Read(Box::new(read_rel))),
        };
        let (result, errors) = ctx.textify(&rel);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert_eq!(
            result,
            "Read[some_db.test_table => col1:i32?, \"column 2\":string?]"
        );
    }

    #[test]
    fn test_filter_rel() {
        let ctx = TestContext::new()
            .with_urn(1, "test_urn")
            .with_function(1, 10, "gt");

        // Create a simple FilterRel with a ReadRel input and a filter expression
        let read_rel = ReadRel {
            common: None,
            base_schema: Some(NamedStruct {
                names: vec!["col1".into(), "col2".into()],
                r#struct: Some(Struct {
                    type_variation_reference: 0,
                    types: vec![
                        Type {
                            kind: Some(Kind::I32(ptype::I32 {
                                type_variation_reference: 0,
                                nullability: Nullability::Nullable as i32,
                            })),
                        },
                        Type {
                            kind: Some(Kind::I32(ptype::I32 {
                                type_variation_reference: 0,
                                nullability: Nullability::Nullable as i32,
                            })),
                        },
                    ],
                    nullability: Nullability::Nullable as i32,
                }),
            }),
            filter: None,
            best_effort_filter: None,
            projection: None,
            advanced_extension: None,
            read_type: Some(ReadType::NamedTable(NamedTable {
                names: vec!["test_table".into()],
                advanced_extension: None,
            })),
        };

        // Create a filter expression: col1 > 10
        let filter_expr = Expression {
            rex_type: Some(RexType::ScalarFunction(ScalarFunction {
                function_reference: 10, // gt function
                arguments: vec![
                    FunctionArgument {
                        arg_type: Some(ArgType::Value(Reference(0).into())),
                    },
                    FunctionArgument {
                        arg_type: Some(ArgType::Value(Expression {
                            rex_type: Some(RexType::Literal(Literal {
                                literal_type: Some(LiteralType::I32(10)),
                                nullable: false,
                                type_variation_reference: 0,
                            })),
                        })),
                    },
                ],
                options: vec![],
                output_type: Some(Type {
                    kind: Some(Kind::Bool(Boolean {
                        nullability: Nullability::Required as i32,
                        type_variation_reference: 0,
                    })),
                }),
                #[allow(deprecated)]
                args: vec![],
            })),
        };

        let filter_rel = FilterRel {
            common: None,
            input: Some(Box::new(Rel {
                rel_type: Some(RelType::Read(Box::new(read_rel))),
            })),
            condition: Some(Box::new(filter_expr)),
            advanced_extension: None,
        };

        let rel = Rel {
            rel_type: Some(RelType::Filter(Box::new(filter_rel))),
        };

        let (result, errors) = ctx.textify(&rel);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        let expected = r#"
Filter[gt($0, 10:i32):boolean => $0, $1]
  Read[test_table => col1:i32?, col2:i32?]"#
            .trim_start();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_aggregate_function_textify() {
        let ctx = TestContext::new()
        .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
        .with_function(1, 10, "sum")
        .with_function(1, 11, "count");

        // Create a simple AggregateFunction
        let agg_fn = get_aggregate_func(10, 1);

        let value = Value::AggregateFunction(&agg_fn);
        let (result, errors) = ctx.textify(&value);

        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert_eq!(result, "sum($1):i64");
    }

    #[test]
    fn test_aggregate_relation_textify() {
        let ctx = TestContext::new()
        .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
        .with_function(1, 10, "sum")
        .with_function(1, 11, "count");

        // Create a simple AggregateRel
        let agg_fn1 = get_aggregate_func(10, 1);
        let agg_fn2 = get_aggregate_func(11, 1);

        let grouping_expressions = vec![Expression {
            rex_type: Some(RexType::Selection(Box::new(
                FieldIndex(0).to_field_reference(),
            ))),
        }];

        let measures = vec![
            aggregate_rel::Measure {
                measure: Some(agg_fn1),
                filter: None,
            },
            aggregate_rel::Measure {
                measure: Some(agg_fn2),
                filter: None,
            },
        ];

        let common = Some(RelCommon {
            emit_kind: Some(EmitKind::Emit(Emit {
                output_mapping: vec![1, 2], // measures only
            })),
            ..Default::default()
        });

        let aggregate_rel = create_aggregate_rel(grouping_expressions, vec![], measures, common);

        let rel = Rel {
            rel_type: Some(RelType::Aggregate(Box::new(aggregate_rel))),
        };
        let (result, errors) = ctx.textify(&rel);

        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        // Expected: Aggregate[_ => sum($1):i64, count($1):i64] we chose to emit only measures
        assert!(result.contains("Aggregate[_ => sum($1):i64, count($1):i64]"));
    }

    #[test]
    fn test_multiple_groupings_on_aggregate_deprecated() {
        // Protobuf plan that uses AggregateRel.groupings with deprecated
        // grouping_expressions, leaving AggregateRel.grouping_expressions empty.
        let ctx = TestContext::new()
        .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
        .with_function(1, 11, "count");

        let grouping_expr_0 = create_exp(0);
        let grouping_expr_1 = create_exp(1);

        let grouping_sets = vec![
            aggregate_rel::Grouping {
                #[allow(deprecated)]
                grouping_expressions: vec![grouping_expr_0.clone()],
                expression_references: vec![],
            },
            aggregate_rel::Grouping {
                #[allow(deprecated)]
                grouping_expressions: vec![grouping_expr_0.clone(), grouping_expr_1.clone()],
                expression_references: vec![],
            },
        ];

        let aggregate_rel = create_aggregate_rel(vec![], grouping_sets, vec![], None);

        let rel = Rel {
            rel_type: Some(RelType::Aggregate(Box::new(aggregate_rel))),
        };
        let (result, errors) = ctx.textify(&rel);

        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert!(result.contains("Aggregate[($0), ($0, $1) => $0, $1]"));
    }

    #[test]
    fn test_multiple_groupings_with_measure_deprecated() {
        // Protobuf plan that uses AggregateRel.groupings with deprecated
        // grouping_expressions, leaving AggregateRel.grouping_expressions empty.
        let ctx = TestContext::new()
        .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
        .with_function(1, 11, "count");

        let agg_fn1 = get_aggregate_func(11, 2);

        let grouping_expr_0 = create_exp(0);
        let grouping_expr_1 = create_exp(1);

        let grouping_sets = vec![
            aggregate_rel::Grouping {
                #[allow(deprecated)]
                grouping_expressions: vec![grouping_expr_0.clone()],
                expression_references: vec![],
            },
            aggregate_rel::Grouping {
                #[allow(deprecated)]
                grouping_expressions: vec![grouping_expr_0.clone(), grouping_expr_1.clone()],
                expression_references: vec![],
            },
        ];

        let measures = vec![aggregate_rel::Measure {
            measure: Some(agg_fn1),
            filter: None,
        }];

        let aggregate_rel = create_aggregate_rel(vec![], grouping_sets, measures, None);

        let rel = Rel {
            rel_type: Some(RelType::Aggregate(Box::new(aggregate_rel))),
        };
        let (result, errors) = ctx.textify(&rel);

        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert!(result.contains("($0), ($0, $1) => $0, $1, count($2):i64"));
    }

    #[test]
    fn test_multiple_groupings_on_aggregate() {
        let ctx = TestContext::new()
        .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
        .with_function(1, 11, "count");

        let agg_fn2 = get_aggregate_func(11, 2);

        let grouping_expressions = vec![
            Expression {
                rex_type: Some(RexType::Selection(Box::new(
                    FieldIndex(0).to_field_reference(),
                ))),
            },
            Expression {
                rex_type: Some(RexType::Selection(Box::new(
                    FieldIndex(1).to_field_reference(),
                ))),
            },
        ];

        let grouping_sets = vec![
            Grouping {
                #[allow(deprecated)]
                grouping_expressions: vec![],
                expression_references: vec![0, 1],
            },
            Grouping {
                #[allow(deprecated)]
                grouping_expressions: vec![],
                expression_references: vec![0, 1],
            },
            Grouping {
                #[allow(deprecated)]
                grouping_expressions: vec![],
                expression_references: vec![1],
            },
            Grouping {
                #[allow(deprecated)]
                grouping_expressions: vec![],
                expression_references: vec![1, 1],
            },
            Grouping {
                #[allow(deprecated)]
                grouping_expressions: vec![],
                expression_references: vec![],
            },
        ];

        let measures = vec![aggregate_rel::Measure {
            measure: Some(agg_fn2),
            filter: None,
        }];

        let aggregate_rel =
            create_aggregate_rel(grouping_expressions, grouping_sets, measures, None);

        let rel = Rel {
            rel_type: Some(RelType::Aggregate(Box::new(aggregate_rel))),
        };
        let (result, errors) = ctx.textify(&rel);

        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert!(
            result.contains(
                "Aggregate[($0, $1), ($0, $1), ($1), ($1, $1), _ => $0, $1, count($2):i64]"
            )
        );
    }

    #[test]
    fn test_deprecated_reordered_grouping() {
        // Protobuf plan that uses the deprecated per-Grouping
        // `grouping_expressions`, leaving `AggregateRel.grouping_expressions`
        // empty. The lone unique expression here is $5, but it is the first
        // (index 0) expression discovered during deduplication - so if the
        // grouping set were rendered from that dedup index rather than from
        // the expression itself, it would wrongly print as `$0` instead of
        // `$5`.
        let ctx = TestContext::new();
        let grouping_expr_5 = create_exp(5);

        let grouping_sets = vec![aggregate_rel::Grouping {
            #[allow(deprecated)]
            grouping_expressions: vec![grouping_expr_5],
            expression_references: vec![],
        }];

        let aggregate_rel = create_aggregate_rel(vec![], grouping_sets, vec![], None);
        let rel = Rel {
            rel_type: Some(RelType::Aggregate(Box::new(aggregate_rel))),
        };
        let (result, errors) = ctx.textify(&rel);

        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert!(result.contains("Aggregate[$5 => $5]"));
    }

    #[test]
    fn test_reordered_grouping_textifies_expression_not_raw_index() {
        let ctx = TestContext::new()
            .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
            .with_function(1, 11, "count");

        let agg_fn2 = get_aggregate_func(11, 1);

        // grouping_expressions is [$2, $0] (textual order); the single
        // grouping set references both by index into grouping_expressions:
        // [0, 1]. Those indexes must resolve back through
        // grouping_expressions ([$2, $0]), not be printed directly as `$0,
        // $1`.
        let grouping_expressions = vec![
            Expression {
                rex_type: Some(RexType::Selection(Box::new(
                    FieldIndex(2).to_field_reference(),
                ))),
            },
            Expression {
                rex_type: Some(RexType::Selection(Box::new(
                    FieldIndex(0).to_field_reference(),
                ))),
            },
        ];

        let grouping_sets = vec![Grouping {
            #[allow(deprecated)]
            grouping_expressions: vec![],
            expression_references: vec![0, 1],
        }];

        let measures = vec![aggregate_rel::Measure {
            measure: Some(agg_fn2),
            filter: None,
        }];

        let aggregate_rel =
            create_aggregate_rel(grouping_expressions, grouping_sets, measures, None);

        let rel = Rel {
            rel_type: Some(RelType::Aggregate(Box::new(aggregate_rel))),
        };
        let (result, errors) = ctx.textify(&rel);

        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert!(result.contains("Aggregate[$2, $0 => $2, $0, count($1):i64]"));
    }

    #[test]
    fn test_join_relation_unknown_type() {
        let ctx = TestContext::new();

        // Create a join with an unknown/invalid type
        let join_rel = JoinRel {
            left: Some(Box::new(Rel {
                rel_type: Some(RelType::Read(Box::default())),
            })),
            right: Some(Box::new(Rel {
                rel_type: Some(RelType::Read(Box::default())),
            })),
            expression: Some(Box::new(Expression::default())),
            r#type: 999, // Invalid join type
            common: None,
            post_join_filter: None,
            advanced_extension: None,
        };

        let rel = Rel {
            rel_type: Some(RelType::Join(Box::new(join_rel))),
        };
        let (result, errors) = ctx.textify(&rel);

        // Should contain error for unknown join type but still show condition and columns
        assert!(!errors.is_empty(), "Expected errors for unknown join type");
        assert!(
            result.contains("!{JoinRel}"),
            "Expected error token for unknown join type"
        );
        assert!(
            result.contains("Join["),
            "Expected Join relation to be formatted"
        );
    }

    #[test]
    fn test_set_relation_unknown_op() {
        let ctx = TestContext::new();

        let set_rel = SetRel {
            common: None,
            inputs: vec![
                Rel {
                    rel_type: Some(RelType::Read(Box::default())),
                },
                Rel {
                    rel_type: Some(RelType::Read(Box::default())),
                },
            ],
            op: 999, // Invalid set op
            advanced_extension: None,
        };
        let rel = Rel {
            rel_type: Some(RelType::Set(set_rel)),
        };

        let (result, errors) = ctx.textify(&rel);
        assert!(!errors.is_empty(), "Expected errors for unknown set op");
        assert!(
            result.contains("!{SetRel}"),
            "Expected error token for unknown set op, got: {result}"
        );
        assert!(
            result.contains("Set["),
            "Expected Set relation to be formatted"
        );
    }

    fn basic_read(table: &str) -> Rel {
        Rel {
            rel_type: Some(RelType::Read(Box::new(ReadRel {
                common: None,
                base_schema: Some(get_basic_schema()),
                filter: None,
                best_effort_filter: None,
                projection: None,
                advanced_extension: None,
                read_type: Some(ReadType::NamedTable(NamedTable {
                    names: vec![table.into()],
                    advanced_extension: None,
                })),
            }))),
        }
    }

    #[test]
    fn test_cross_relation() {
        let ctx = TestContext::new();

        // Two 3-column reads: the cross output concatenates both, giving 6
        // columns ($0..$5), with no arguments.
        let cross = CrossRel {
            common: Some(RelCommon {
                emit_kind: Some(EmitKind::Direct(Direct {})),
                ..Default::default()
            }),
            left: Some(Box::new(basic_read("left_tbl"))),
            right: Some(Box::new(basic_read("right_tbl"))),
            advanced_extension: None,
        };
        let rel = Rel {
            rel_type: Some(RelType::Cross(Box::new(cross))),
        };

        let (result, errors) = ctx.textify(&rel);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        let expected = r#"
Cross[$0, $1, $2, $3, $4, $5]
  Read[left_tbl => category:string?, amount:fp64?, value:i32?]
  Read[right_tbl => category:string?, amount:fp64?, value:i32?]"#
            .trim_start();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_cross_relation_prunes_columns() {
        let ctx = TestContext::new();

        // A non-identity emit selects only two of the six columns.
        let cross = CrossRel {
            common: Some(RelCommon {
                emit_kind: Some(EmitKind::Emit(Emit {
                    output_mapping: vec![0, 3],
                })),
                ..Default::default()
            }),
            left: Some(Box::new(basic_read("left_tbl"))),
            right: Some(Box::new(basic_read("right_tbl"))),
            advanced_extension: None,
        };
        let rel = Rel {
            rel_type: Some(RelType::Cross(Box::new(cross))),
        };

        let (result, errors) = ctx.textify(&rel);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        let expected = r#"
Cross[$0, $3]
  Read[left_tbl => category:string?, amount:fp64?, value:i32?]
  Read[right_tbl => category:string?, amount:fp64?, value:i32?]"#
            .trim_start();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_join_type_enum_textify() {
        // Test that JoinType enum values convert correctly to their string representation
        assert_eq!(join_rel::JoinType::Inner.as_enum_str().unwrap(), "Inner");
        assert_eq!(join_rel::JoinType::Left.as_enum_str().unwrap(), "Left");
        assert_eq!(
            join_rel::JoinType::LeftSemi.as_enum_str().unwrap(),
            "LeftSemi"
        );
        assert_eq!(
            join_rel::JoinType::LeftAnti.as_enum_str().unwrap(),
            "LeftAnti"
        );
    }

    #[test]
    fn test_join_output_columns() {
        // Test Inner join - outputs all columns from both sides
        let inner_cols = super::join_output_columns(join_rel::JoinType::Inner, 2, 3);
        assert_eq!(inner_cols.len(), 5); // 2 + 3 = 5 columns
        assert!(matches!(inner_cols[0], Value::Reference(0)));
        assert!(matches!(inner_cols[4], Value::Reference(4)));

        // Test LeftSemi join - outputs only left columns
        let left_semi_cols = super::join_output_columns(join_rel::JoinType::LeftSemi, 2, 3);
        assert_eq!(left_semi_cols.len(), 2); // Only left columns
        assert!(matches!(left_semi_cols[0], Value::Reference(0)));
        assert!(matches!(left_semi_cols[1], Value::Reference(1)));

        // Test RightSemi join - outputs right columns as contiguous range starting from $0
        let right_semi_cols = super::join_output_columns(join_rel::JoinType::RightSemi, 2, 3);
        assert_eq!(right_semi_cols.len(), 3); // Only right columns
        assert!(matches!(right_semi_cols[0], Value::Reference(0))); // Contiguous range starts at $0
        assert!(matches!(right_semi_cols[1], Value::Reference(1)));
        assert!(matches!(right_semi_cols[2], Value::Reference(2))); // Last right column

        // Test LeftMark join - outputs left columns plus a mark column as contiguous range
        let left_mark_cols = super::join_output_columns(join_rel::JoinType::LeftMark, 2, 3);
        assert_eq!(left_mark_cols.len(), 3); // 2 left + 1 mark
        assert!(matches!(left_mark_cols[0], Value::Reference(0)));
        assert!(matches!(left_mark_cols[1], Value::Reference(1)));
        assert!(matches!(left_mark_cols[2], Value::Reference(2))); // Mark column at contiguous position

        // Test RightMark join - outputs right columns plus a mark column as contiguous range
        let right_mark_cols = super::join_output_columns(join_rel::JoinType::RightMark, 2, 3);
        assert_eq!(right_mark_cols.len(), 4); // 3 right + 1 mark
        assert!(matches!(right_mark_cols[0], Value::Reference(0))); // Contiguous range starts at $0
        assert!(matches!(right_mark_cols[1], Value::Reference(1)));
        assert!(matches!(right_mark_cols[2], Value::Reference(2))); // Last right column
        assert!(matches!(right_mark_cols[3], Value::Reference(3))); // Mark column at contiguous position
    }

    fn get_aggregate_func(func_ref: u32, column_ind: i32) -> AggregateFunction {
        AggregateFunction {
            function_reference: func_ref,
            arguments: vec![FunctionArgument {
                arg_type: Some(ArgType::Value(Expression {
                    rex_type: Some(RexType::Selection(Box::new(
                        FieldIndex(column_ind).to_field_reference(),
                    ))),
                })),
            }],
            options: vec![],
            output_type: Some(Type {
                kind: Some(Kind::I64(I64 {
                    nullability: Nullability::Required as i32,
                    type_variation_reference: 0,
                })),
            }),
            invocation: 0,
            phase: 0,
            sorts: vec![],
            #[allow(deprecated)]
            args: vec![],
        }
    }

    fn create_aggregate_rel(
        grouping_expressions: Vec<Expression>,
        grouping_sets: Vec<Grouping>,
        measures: Vec<aggregate_rel::Measure>,
        common: Option<RelCommon>,
    ) -> AggregateRel {
        let common = common.or_else(|| {
            Some(RelCommon {
                emit_kind: Some(EmitKind::Direct(Direct {})),
                ..Default::default()
            })
        });
        AggregateRel {
            input: Some(Box::new(Rel {
                rel_type: Some(RelType::Read(Box::new(ReadRel {
                    common: None,
                    base_schema: Some(get_basic_schema()),
                    filter: None,
                    best_effort_filter: None,
                    projection: None,
                    advanced_extension: None,
                    read_type: Some(ReadType::NamedTable(NamedTable {
                        names: vec!["orders".into()],
                        advanced_extension: None,
                    })),
                }))),
            })),
            grouping_expressions,
            groupings: grouping_sets,
            measures,
            common,
            advanced_extension: None,
        }
    }

    fn get_basic_schema() -> NamedStruct {
        NamedStruct {
            names: vec!["category".into(), "amount".into(), "value".into()],
            r#struct: Some(Struct {
                type_variation_reference: 0,
                types: vec![
                    Type {
                        kind: Some(Kind::String(ptype::String {
                            type_variation_reference: 0,
                            nullability: Nullability::Nullable as i32,
                        })),
                    },
                    Type {
                        kind: Some(Kind::Fp64(ptype::Fp64 {
                            type_variation_reference: 0,
                            nullability: Nullability::Nullable as i32,
                        })),
                    },
                    Type {
                        kind: Some(Kind::I32(ptype::I32 {
                            type_variation_reference: 0,
                            nullability: Nullability::Nullable as i32,
                        })),
                    },
                ],
                nullability: Nullability::Nullable as i32,
            }),
        }
    }

    fn create_exp(column_ind: i32) -> Expression {
        Expression {
            rex_type: Some(RexType::Selection(Box::new(
                FieldIndex(column_ind).to_field_reference(),
            ))),
        }
    }

    #[test]
    fn test_unsupported_rel_type_produces_failure_token() {
        let ctx = TestContext::new();

        // ReferenceRel is a valid Substrait relation type that the textifier
        // does not yet support.  Wrapping it in a Rel and textifying should
        // produce a `!{Rel}` failure token rather than panicking.
        let rel = Rel {
            rel_type: Some(RelType::Reference(ReferenceRel { subtree_ordinal: 0 })),
        };

        let (result, errors) = ctx.textify(&rel);

        // The output should contain the failure token, not an empty string.
        assert!(
            result.contains("!{Rel}"),
            "Expected '!{{Rel}}' in output, got: {result}"
        );

        // Exactly one error should have been collected.
        assert_eq!(errors.0.len(), 1, "Expected exactly one error: {errors:?}");

        // The error should be a Format / Unimplemented error mentioning ReferenceRel.
        match &errors.0[0] {
            FormatError::Format(plan_err) => {
                assert_eq!(plan_err.message, "Rel");
                assert_eq!(plan_err.error_type, FormatErrorType::Unimplemented);
                assert!(
                    plan_err
                        .lookup
                        .as_deref()
                        .unwrap_or("")
                        .contains("Reference"),
                    "Expected lookup to mention 'Reference', got: {:?}",
                    plan_err.lookup
                );
            }
            other => panic!("Expected FormatError::Format, got: {other:?}"),
        }
    }
}
