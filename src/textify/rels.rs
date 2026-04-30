use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::Debug;

use prost::{Message, UnknownEnumValue};
use substrait::proto::extensions::AdvancedExtension;
use substrait::proto::fetch_rel::CountMode;
use substrait::proto::plan_rel::RelType as PlanRelType;
use substrait::proto::read_rel::ReadType;
use substrait::proto::rel::RelType;
use substrait::proto::rel_common::EmitKind;
use substrait::proto::sort_field::{SortDirection, SortKind};
use substrait::proto::{
    AggregateFunction, AggregateRel, Expression, ExtensionLeafRel, ExtensionMultiRel,
    ExtensionSingleRel, FetchRel, FilterRel, JoinRel, NamedStruct, PlanRel, ProjectRel, ReadRel,
    Rel, RelCommon, RelRoot, SortField, SortRel, Type, join_rel,
};

use super::expressions::Reference;
use super::types::Name;
use super::{PlanError, Scope, Textify};
use crate::FormatError;
use crate::extensions::any::AnyRef;
use crate::extensions::{ExtensionColumn, ExtensionValue};

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

/// Trait for enums that can be converted to a string representation for
/// textification.
///
/// Returns Ok(str) for valid enum values, or Err([PlanError]) for invalid or
/// unknown values.
pub trait ValueEnum {
    fn as_enum_str(&self) -> Result<Cow<'static, str>, PlanError>;
}

#[derive(Debug, Clone)]
pub struct NamedArg<'a> {
    pub name: Cow<'a, str>,
    pub value: Value<'a>,
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Name(Name<'a>),
    TableName(Vec<Name<'a>>),
    Field(Option<Name<'a>>, Option<&'a Type>),
    Tuple(Vec<Value<'a>>),
    List(Vec<Value<'a>>),
    Reference(i32),
    Expression(&'a Expression),
    AggregateFunction(&'a AggregateFunction),
    /// Represents a missing, invalid, or unspecified value.
    Missing(PlanError),
    /// Represents a valid enum value as a string for textification.
    Enum(Cow<'a, str>),
    EmptyGroup,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    /// A decoded extension argument value.
    ExtValue(ExtensionValue),
    /// A decoded extension output column.
    ExtColumn(ExtensionColumn),
}

impl<'a> Value<'a> {
    pub fn expect(maybe_value: Option<Self>, f: impl FnOnce() -> PlanError) -> Self {
        match maybe_value {
            Some(s) => s,
            None => Value::Missing(f()),
        }
    }
}

impl<'a> From<Result<Vec<Name<'a>>, PlanError>> for Value<'a> {
    fn from(token: Result<Vec<Name<'a>>, PlanError>) -> Self {
        match token {
            Ok(value) => Value::TableName(value),
            Err(err) => Value::Missing(err),
        }
    }
}

impl<'a> Textify for Value<'a> {
    fn name() -> &'static str {
        "Value"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            Value::Name(name) => write!(w, "{}", ctx.display(name)),
            Value::TableName(names) => write!(w, "{}", ctx.separated(names, ".")),
            Value::Field(name, typ) => {
                write!(w, "{}:{}", ctx.expect(name.as_ref()), ctx.expect(*typ))
            }
            Value::Tuple(values) => write!(w, "({})", ctx.separated(values, ", ")),
            Value::List(values) => write!(w, "[{}]", ctx.separated(values, ", ")),
            Value::Reference(i) => write!(w, "{}", Reference(*i)),
            Value::Expression(e) => write!(w, "{}", ctx.display(*e)),
            Value::AggregateFunction(agg_fn) => agg_fn.textify(ctx, w),
            Value::Missing(err) => write!(w, "{}", ctx.failure(err.clone())),
            Value::Enum(res) => write!(w, "&{res}"),
            Value::Integer(i) => write!(w, "{i}"),
            Value::EmptyGroup => write!(w, "_"),
            Value::Float(f) => write!(w, "{f}"),
            Value::Boolean(b) => write!(w, "{b}"),
            Value::ExtValue(ev) => ev.textify(ctx, w),
            Value::ExtColumn(ec) => ec.textify(ctx, w),
        }
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

struct Emitted<'a> {
    pub values: &'a [Value<'a>],
    pub emit: Option<&'a EmitKind>,
}

impl<'a> Emitted<'a> {
    pub fn new(values: &'a [Value<'a>], emit: Option<&'a EmitKind>) -> Self {
        Self { values, emit }
    }

    pub fn write_direct<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}", ctx.separated(self.values.iter(), ", "))
    }
}

impl<'a> Textify for Emitted<'a> {
    fn name() -> &'static str {
        "Emitted"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        if ctx.options().show_emit {
            return self.write_direct(ctx, w);
        }

        let indices = match &self.emit {
            Some(EmitKind::Emit(e)) => &e.output_mapping,
            Some(EmitKind::Direct(_)) => return self.write_direct(ctx, w),
            None => return self.write_direct(ctx, w),
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
}

#[derive(Debug, Clone)]
pub struct Arguments<'a> {
    /// Positional arguments (e.g., a filter condition, group-bys, etc.)
    pub positional: Vec<Value<'a>>,
    /// Named arguments (e.g., limit=10, offset=5)
    pub named: Vec<NamedArg<'a>>,
}

impl<'a> Textify for Arguments<'a> {
    fn name() -> &'static str {
        "Arguments"
    }
    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        if self.positional.is_empty() && self.named.is_empty() {
            return write!(w, "_");
        }

        write!(w, "{}", ctx.separated(self.positional.iter(), ", "))?;
        if !self.positional.is_empty() && !self.named.is_empty() {
            write!(w, ", ")?;
        }
        write!(w, "{}", ctx.separated(self.named.iter(), ", "))
    }
}

pub struct Relation<'a> {
    pub name: Cow<'a, str>,
    /// Arguments to the relation, if any.
    ///
    /// - `None` means this relation does not take arguments, and the argument
    ///   section is omitted entirely.
    /// - `Some(args)` with both vectors empty means the relation takes
    ///   arguments, but none are provided; this will print as `_ => ...`.
    /// - `Some(args)` with non-empty vectors will print as usual, with
    ///   positional arguments first, then named arguments, separated by commas.
    pub arguments: Option<Arguments<'a>>,
    /// The columns emitted by this relation, pre-emit - the 'direct' column
    /// output.
    pub columns: Vec<Value<'a>>,
    /// The emit kind, if any. If none, use the columns directly.
    pub emit: Option<&'a EmitKind>,
    /// The advanced extension (enhancement and/or optimizations) attached to
    /// this relation, if any.  Mirrors the `advanced_extension` field carried
    /// by standard relation types in the protobuf (Read, Filter, Project, etc...)
    ///  Extension relations (`ExtensionLeaf`, `ExtensionSingle`, `ExtensionMulti`)
    /// do not carry this field and always set it to `None`.
    pub advanced_extension: Option<&'a AdvancedExtension>,
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
        // Emit any enhancement / optimizations between the header line and the
        // child relations, indented one level deeper than this relation — the
        // same position they occupy in the text format when parsed.
        if let Some(adv_ext) = self.advanced_extension {
            adv_ext.textify(&child_scope, w)?;
        }
        self.write_children(ctx, w)?;
        Ok(())
    }
}

impl Relation<'_> {
    /// Write the single header line for this relation, e.g. `Filter[$0 => $0]`.
    /// Does not write a trailing newline; callers are responsible for any
    /// newline that follows (either from adv_ext or from the next child).
    pub fn write_header<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let cols = Emitted::new(&self.columns, self.emit);
        let indent = ctx.indent();
        let name = &self.name;
        let cols = ctx.display(&cols);
        match &self.arguments {
            None => {
                write!(w, "{indent}{name}[{cols}]")
            }
            Some(args) => {
                let args = ctx.display(args);
                write!(w, "{indent}{name}[{args} => {cols}]")
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

#[derive(Debug, Copy, Clone)]
pub struct TableName<'a>(&'a [String]);

impl<'a> Textify for TableName<'a> {
    fn name() -> &'static str {
        "TableName"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let names = self.0.iter().map(|n| Name(n)).collect::<Vec<_>>();
        write!(w, "{}", ctx.separated(names.iter(), "."))
    }
}

impl<'a> Relation<'a> {
    fn from_read<S: Scope>(rel: &'a ReadRel, _ctx: &S) -> Self {
        let columns = read_columns(rel);
        let emit = rel.common.as_ref().and_then(|c| c.emit_kind.as_ref());

        match &rel.read_type {
            Some(ReadType::NamedTable(table)) => {
                let table_name = Value::TableName(table.names.iter().map(|n| Name(n)).collect());
                Relation {
                    name: Cow::Borrowed("Read"),
                    arguments: Some(Arguments {
                        positional: vec![table_name],
                        named: vec![],
                    }),
                    columns,
                    emit,
                    advanced_extension: rel.advanced_extension.as_ref(),
                    children: vec![],
                }
            }
            Some(ReadType::VirtualTable(vt)) => {
                let positional = vt
                    .expressions
                    .iter()
                    .map(|row| Value::Tuple(row.fields.iter().map(Value::Expression).collect()))
                    .collect();

                Relation {
                    name: Cow::Borrowed("Read:Virtual"),
                    arguments: Some(Arguments {
                        positional,
                        named: vec![],
                    }),
                    columns,
                    emit,
                    advanced_extension: rel.advanced_extension.as_ref(),
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
                    arguments: Some(Arguments {
                        positional: vec![Value::Missing(err)],
                        named: vec![],
                    }),
                    columns,
                    emit,
                    advanced_extension: rel.advanced_extension.as_ref(),
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
    /// Create a vector of values that are references to the emitted outputs of
    /// this relation. "Emitted" here meaning the outputs of this relation after
    /// the emit kind has been applied.
    ///
    /// This is useful for relations like Filter and Limit whose direct outputs
    /// are primarily those of its children (direct here meaning before the emit
    /// has been applied).
    pub fn input_refs(&self) -> Vec<Value<'a>> {
        let len = self.emitted();
        (0..len).map(|i| Value::Reference(i as i32)).collect()
    }

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
        let arguments = Some(Arguments {
            positional,
            named: vec![],
        });
        let emit = get_emit(rel.common.as_ref());
        let (children, columns) = Relation::convert_children(vec![rel.input.as_deref()], ctx);
        let columns = (0..columns).map(|i| Value::Reference(i as i32)).collect();

        Relation {
            name: Cow::Borrowed("Filter"),
            arguments,
            columns,
            emit,
            advanced_extension: rel.advanced_extension.as_ref(),
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
            advanced_extension: rel.advanced_extension.as_ref(),
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
                    advanced_extension: None,
                    children: vec![],
                }
            }
        }
    }

    fn from_extension_leaf<S: Scope>(rel: &'a ExtensionLeafRel, ctx: &S) -> Self {
        let detail_ref = rel.detail.as_ref().map(AnyRef::from);
        let decoded = match detail_ref {
            Some(d) => ctx.extension_registry().decode(d, ctx.extensions()),
            None => Err(crate::extensions::registry::ExtensionError::MissingDetail),
        };
        Relation::from_extension("ExtensionLeaf", decoded, vec![], ctx)
    }

    fn from_extension_single<S: Scope>(rel: &'a ExtensionSingleRel, ctx: &S) -> Self {
        let detail_ref = rel.detail.as_ref().map(AnyRef::from);
        let decoded = match detail_ref {
            Some(d) => ctx.extension_registry().decode(d, ctx.extensions()),
            None => Err(crate::extensions::registry::ExtensionError::MissingDetail),
        };
        Relation::from_extension("ExtensionSingle", decoded, vec![rel.input.as_deref()], ctx)
    }

    fn from_extension_multi<S: Scope>(rel: &'a ExtensionMultiRel, ctx: &S) -> Self {
        let detail_ref = rel.detail.as_ref().map(AnyRef::from);
        let decoded = match detail_ref {
            Some(d) => ctx.extension_registry().decode(d, ctx.extensions()),
            None => Err(crate::extensions::registry::ExtensionError::MissingDetail),
        };
        let mut child_refs: Vec<Option<&'a Rel>> = vec![];
        for input in &rel.inputs {
            child_refs.push(Some(input));
        }
        Relation::from_extension("ExtensionMulti", decoded, child_refs, ctx)
    }

    fn from_extension<S: Scope>(
        ext_type: &'static str,
        decoded: Result<
            (String, crate::extensions::ExtensionArgs),
            crate::extensions::registry::ExtensionError,
        >,
        child_refs: Vec<Option<&'a Rel>>,
        ctx: &S,
    ) -> Self {
        match decoded {
            Ok((name, args)) => {
                let (children, _) = Relation::convert_children(child_refs, ctx);
                let mut positional = vec![];
                for value in args.positional {
                    positional.push(Value::ExtValue(value));
                }
                let mut named = vec![];
                for (key, value) in args.named {
                    named.push(NamedArg {
                        name: Cow::Owned(key),
                        value: Value::ExtValue(value),
                    });
                }
                let mut columns = vec![];
                for col in args.output_columns {
                    columns.push(Value::ExtColumn(col));
                }
                Relation {
                    name: Cow::Owned(format!("{}:{}", ext_type, name)),
                    arguments: Some(Arguments { positional, named }),
                    columns,
                    emit: None,
                    // Extension relations use `detail` rather than
                    // `advanced_extension`; the field does not exist on these
                    // proto types.
                    advanced_extension: None,
                    children,
                }
            }
            Err(error) => {
                let (children, _) = Relation::convert_children(child_refs, ctx);
                Relation {
                    name: Cow::Borrowed(ext_type),
                    arguments: None,
                    columns: vec![Value::Missing(PlanError::invalid(
                        "extension",
                        None::<&str>,
                        error.to_string(),
                    ))],
                    emit: None,
                    advanced_extension: None,
                    children,
                }
            }
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
                    grouping_set.push(Value::Reference(*i as i32));
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
        let arguments = Some(Arguments {
            positional,
            named: vec![],
        });

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
            advanced_extension: rel.advanced_extension.as_ref(),
            children,
        }
    }

    fn get_grouping_sets(rel: &'a AggregateRel) -> (Vec<Value<'a>>, Vec<Vec<Value<'a>>>) {
        let mut grouping_sets: Vec<Vec<Value>> = vec![];
        let mut expression_list: Vec<Value> = Vec::new();

        // groupings might have the same expressions in their set so we use a map to get unique expressions
        let mut expression_index_map = HashMap::new();
        let mut i: i32 = 0; // index for the unique expression in the grouping_expressions list

        for group in &rel.groupings {
            let mut grouping_set: Vec<Value> = vec![];
            #[allow(deprecated)]
            for exp in &group.grouping_expressions {
                // TODO: use a better key here than encoding to bytes.
                // Ideally, substrait-rs would support `PartialEq` and `Hash`,
                // but as there isn't an easy way to do that now, we'll skip.
                let key = exp.encode_to_vec();
                expression_index_map.entry(key.clone()).or_insert_with(|| {
                    let value = Value::Expression(exp);
                    expression_list.push(value); // new unique expression found
                    // mapping the byte encoded expression to its index in the group_expression list
                    let index = i;
                    i += 1;
                    index // is expression returned by this closure and inserted into map
                });
                grouping_set.push(Value::Reference(expression_index_map[&key]));
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
        let arguments = Some(Arguments {
            positional,
            named: vec![],
        });
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
            advanced_extension: rel.advanced_extension.as_ref(),
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
                substrait::proto::fetch_rel::OffsetMode::OffsetExpr(expr) => {
                    named_args.push(NamedArg {
                        name: Cow::Borrowed("offset"),
                        value: Value::Expression(expr),
                    });
                }
                #[allow(deprecated)]
                substrait::proto::fetch_rel::OffsetMode::Offset(val) => {
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
            arguments: Some(Arguments {
                positional: vec![],
                named: named_args,
            }),
            columns,
            emit,
            advanced_extension: rel.advanced_extension.as_ref(),
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

        // TODO: Add support for post_join_filter when grammar is extended
        // Currently post_join_filter is not supported in the text format
        // grammar
        let positional = vec![join_type_value, condition];
        let arguments = Some(Arguments {
            positional,
            named: vec![],
        });

        let emit = get_emit(rel.common.as_ref());
        let columns = join_output_columns(join_type, left_columns, right_columns);

        Relation {
            name: Cow::Borrowed("Join"),
            arguments,
            columns,
            emit,
            advanced_extension: rel.advanced_extension.as_ref(),
            children,
        }
    }
}

impl<'a> From<&'a SortField> for Value<'a> {
    fn from(sf: &'a SortField) -> Self {
        let field = match &sf.expr {
            Some(expr) => match &expr.rex_type {
                Some(substrait::proto::expression::RexType::Selection(fref)) => {
                    if let Some(substrait::proto::expression::field_reference::ReferenceType::DirectReference(seg)) = &fref.reference_type {
                        if let Some(substrait::proto::expression::reference_segment::ReferenceType::StructField(sf)) = &seg.reference_type {
                            Value::Reference(sf.field)
                        } else { Value::Missing(PlanError::unimplemented("SortField", Some("expr"), "Not a struct field")) }
                    } else { Value::Missing(PlanError::unimplemented("SortField", Some("expr"), "Not a direct reference")) }
                }
                _ => Value::Missing(PlanError::unimplemented(
                    "SortField",
                    Some("expr"),
                    "Not a selection",
                )),
            },
            None => Value::Missing(PlanError::unimplemented(
                "SortField",
                Some("expr"),
                "Missing expr",
            )),
        };
        let direction = match &sf.sort_kind {
            Some(kind) => Value::from(kind),
            None => Value::Missing(PlanError::invalid(
                "SortKind",
                Some(Cow::Borrowed("sort_kind")),
                "Missing sort_kind",
            )),
        };
        Value::Tuple(vec![field, direction])
    }
}

impl<'a, T: ValueEnum + ?Sized> From<&'a T> for Value<'a> {
    fn from(enum_val: &'a T) -> Self {
        match enum_val.as_enum_str() {
            Ok(s) => Value::Enum(s),
            Err(e) => Value::Missing(e),
        }
    }
}

impl ValueEnum for SortKind {
    fn as_enum_str(&self) -> Result<Cow<'static, str>, PlanError> {
        let d = match self {
            &SortKind::Direction(d) => SortDirection::try_from(d),
            SortKind::ComparisonFunctionReference(f) => {
                return Err(PlanError::invalid(
                    "SortKind",
                    Some(Cow::Owned(format!("function reference{f}"))),
                    "SortKind::ComparisonFunctionReference unimplemented",
                ));
            }
        };
        let s = match d {
            Err(UnknownEnumValue(d)) => {
                return Err(PlanError::invalid(
                    "SortKind",
                    Some(Cow::Owned(format!("unknown variant: {d:?}"))),
                    "Unknown SortDirection",
                ));
            }
            Ok(SortDirection::AscNullsFirst) => "AscNullsFirst",
            Ok(SortDirection::AscNullsLast) => "AscNullsLast",
            Ok(SortDirection::DescNullsFirst) => "DescNullsFirst",
            Ok(SortDirection::DescNullsLast) => "DescNullsLast",
            Ok(SortDirection::Clustered) => "Clustered",
            Ok(SortDirection::Unspecified) => {
                return Err(PlanError::invalid(
                    "SortKind",
                    Option::<Cow<str>>::None,
                    "Unspecified SortDirection",
                ));
            }
        };
        Ok(Cow::Borrowed(s))
    }
}

impl ValueEnum for join_rel::JoinType {
    fn as_enum_str(&self) -> Result<Cow<'static, str>, PlanError> {
        let s = match self {
            join_rel::JoinType::Unspecified => {
                return Err(PlanError::invalid(
                    "JoinType",
                    Option::<Cow<str>>::None,
                    "Unspecified JoinType",
                ));
            }
            join_rel::JoinType::Inner => "Inner",
            join_rel::JoinType::Outer => "Outer",
            join_rel::JoinType::Left => "Left",
            join_rel::JoinType::Right => "Right",
            join_rel::JoinType::LeftSemi => "LeftSemi",
            join_rel::JoinType::RightSemi => "RightSemi",
            join_rel::JoinType::LeftAnti => "LeftAnti",
            join_rel::JoinType::RightAnti => "RightAnti",
            join_rel::JoinType::LeftSingle => "LeftSingle",
            join_rel::JoinType::RightSingle => "RightSingle",
            join_rel::JoinType::LeftMark => "LeftMark",
            join_rel::JoinType::RightMark => "RightMark",
        };
        Ok(Cow::Borrowed(s))
    }
}

impl<'a> Textify for NamedArg<'a> {
    fn name() -> &'static str {
        "NamedArg"
    }
    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}=", self.name)?;
        self.value.textify(ctx, w)
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
    use substrait::proto::r#type::{self as ptype, Kind, Nullability, Struct};
    use substrait::proto::{
        Expression, FunctionArgument, NamedStruct, ReadRel, Type, aggregate_rel,
    };

    use super::*;
    use crate::fixtures::TestContext;
    use crate::parser::expressions::FieldIndex;

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
                output_type: None,
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
Filter[gt($0, 10:i32) => $0, $1]
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
        assert_eq!(result, "sum($1)");
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
        // Expected: Aggregate[_ => sum($1), count($1)] we chose to emit only measures
        assert!(result.contains("Aggregate[_ => sum($1), count($1)]"));
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
        assert!(result.contains("($0), ($0, $1) => $0, $1, count($2)"));
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
            result
                .contains("Aggregate[($0, $1), ($0, $1), ($1), ($1, $1), _ => $0, $1, count($2)]")
        );
    }

    #[test]
    fn test_arguments_textify_positional_only() {
        let ctx = TestContext::new();
        let args = Arguments {
            positional: vec![Value::Integer(42), Value::Integer(7)],
            named: vec![],
        };
        let (result, errors) = ctx.textify(&args);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert_eq!(result, "42, 7");
    }

    #[test]
    fn test_arguments_textify_named_only() {
        let ctx = TestContext::new();
        let args = Arguments {
            positional: vec![],
            named: vec![
                NamedArg {
                    name: Cow::Borrowed("limit"),
                    value: Value::Integer(10),
                },
                NamedArg {
                    name: Cow::Borrowed("offset"),
                    value: Value::Integer(5),
                },
            ],
        };
        let (result, errors) = ctx.textify(&args);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert_eq!(result, "limit=10, offset=5");
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
    fn test_arguments_textify_both() {
        let ctx = TestContext::new();
        let args = Arguments {
            positional: vec![Value::Integer(1)],
            named: vec![NamedArg {
                name: "foo".into(),
                value: Value::Integer(2),
            }],
        };
        let (result, errors) = ctx.textify(&args);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert_eq!(result, "1, foo=2");
    }

    #[test]
    fn test_arguments_textify_empty() {
        let ctx = TestContext::new();
        let args = Arguments {
            positional: vec![],
            named: vec![],
        };
        let (result, errors) = ctx.textify(&args);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert_eq!(result, "_");
    }

    #[test]
    fn test_named_arg_textify_error_token() {
        let ctx = TestContext::new();
        let named_arg = NamedArg {
            name: "foo".into(),
            value: Value::Missing(PlanError::invalid(
                "my_enum",
                Some(Cow::Borrowed("my_enum")),
                Cow::Borrowed("my_enum"),
            )),
        };
        let (result, errors) = ctx.textify(&named_arg);
        // Should show !{my_enum} in the output
        assert!(result.contains("foo=!{my_enum}"), "Output: {result}");
        // Should also accumulate an error
        assert!(!errors.is_empty(), "Expected error for error token");
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
            output_type: None,
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
        use substrait::proto::CrossRel;

        let ctx = TestContext::new();

        // CrossRel is a valid Substrait relation type that the textifier
        // does not yet support.  Wrapping it in a Rel and textifying should
        // produce a `!{Rel}` failure token rather than panicking.
        let rel = Rel {
            rel_type: Some(RelType::Cross(Box::new(CrossRel {
                common: None,
                left: None,
                right: None,
                advanced_extension: None,
            }))),
        };

        let (result, errors) = ctx.textify(&rel);

        // The output should contain the failure token, not an empty string.
        assert!(
            result.contains("!{Rel}"),
            "Expected '!{{Rel}}' in output, got: {result}"
        );

        // Exactly one error should have been collected.
        assert_eq!(errors.0.len(), 1, "Expected exactly one error: {errors:?}");

        // The error should be a Format / Unimplemented error mentioning CrossRel.
        match &errors.0[0] {
            FormatError::Format(plan_err) => {
                assert_eq!(plan_err.message, "Rel");
                assert_eq!(
                    plan_err.error_type,
                    crate::textify::foundation::FormatErrorType::Unimplemented
                );
                assert!(
                    plan_err.lookup.as_deref().unwrap_or("").contains("Cross"),
                    "Expected lookup to mention 'Cross', got: {:?}",
                    plan_err.lookup
                );
            }
            other => panic!("Expected FormatError::Format, got: {other:?}"),
        }
    }
}
