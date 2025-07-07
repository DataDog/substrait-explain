use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::Debug;

use prost::UnknownEnumValue;
use substrait::proto::fetch_rel::CountMode;
use substrait::proto::plan_rel::RelType as PlanRelType;
use substrait::proto::read_rel::ReadType;
use substrait::proto::rel::RelType;
use substrait::proto::rel_common::EmitKind;
use substrait::proto::sort_field::{SortDirection, SortKind};
use substrait::proto::{
    AggregateFunction, AggregateRel, Expression, FetchRel, FilterRel, JoinRel, NamedStruct,
    PlanRel, ProjectRel, ReadRel, Rel, RelCommon, RelRoot, SortField, SortRel, Type, join_rel,
};

use super::expressions::Reference;
use super::types::Name;
use super::{PlanError, Scope, Textify};

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
    pub name: &'a str,
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
    Integer(i32),
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
    /// Positional arguments (e.g., a filter condition, group-bys, etc.))
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
    pub name: &'a str,
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
    /// The input relations.
    pub children: Vec<Option<Relation<'a>>>,
}

impl Textify for Relation<'_> {
    fn name() -> &'static str {
        "Relation"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let cols = Emitted::new(&self.columns, self.emit);
        let indent = ctx.indent();
        let name = self.name;
        let cols = ctx.display(&cols);
        match &self.arguments {
            None => {
                write!(w, "{indent}{name}[{cols}]")?;
            }
            Some(args) => {
                let args = ctx.display(args);
                write!(w, "{indent}{name}[{args} => {cols}]")?;
            }
        }
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

pub fn get_table_name(rel: Option<&ReadType>) -> Result<&[String], PlanError> {
    match rel {
        Some(ReadType::NamedTable(r)) => Ok(r.names.as_slice()),
        _ => Err(PlanError::unimplemented(
            "ReadRel",
            Some("table_name"),
            format!("Unexpected read type {rel:?}") as String,
        )),
    }
}

impl<'a> From<&'a ReadRel> for Relation<'a> {
    fn from(rel: &'a ReadRel) -> Self {
        let name = get_table_name(rel.read_type.as_ref());
        let table_name: Value = match name {
            Ok(n) => Value::TableName(n.iter().map(|n| Name(n)).collect()),
            Err(e) => Value::Missing(e),
        };

        let columns = match rel.base_schema {
            Some(ref schema) => schema_to_values(schema),
            None => {
                let err = PlanError::unimplemented(
                    "ReadRel",
                    Some("base_schema"),
                    "Base schema is required",
                );
                vec![Value::Missing(err)]
            }
        };
        let emit = rel.common.as_ref().and_then(|c| c.emit_kind.as_ref());

        Relation {
            name: "Read",
            arguments: Some(Arguments {
                positional: vec![table_name],
                named: vec![],
            }),
            columns,
            emit,
            children: vec![],
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
    pub fn convert_children(refs: Vec<Option<&'a Rel>>) -> (Vec<Option<Relation<'a>>>, usize) {
        let mut children = vec![];
        let mut inputs = 0;

        for maybe_rel in refs {
            match maybe_rel {
                Some(rel) => {
                    let child = Relation::from(rel);
                    inputs += child.emitted();
                    children.push(Some(child));
                }
                None => children.push(None),
            }
        }

        (children, inputs)
    }
}

impl<'a> From<&'a FilterRel> for Relation<'a> {
    fn from(rel: &'a FilterRel) -> Self {
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
        let (children, columns) = Relation::convert_children(vec![rel.input.as_deref()]);
        let columns = (0..columns).map(|i| Value::Reference(i as i32)).collect();

        Relation {
            name: "Filter",
            arguments,
            columns,
            emit,
            children,
        }
    }
}

impl<'a> From<&'a ProjectRel> for Relation<'a> {
    fn from(rel: &'a ProjectRel) -> Self {
        let (children, columns) = Relation::convert_children(vec![rel.input.as_deref()]);
        let expressions = rel.expressions.iter().map(Value::Expression);
        let mut columns: Vec<Value> = (0..columns).map(|i| Value::Reference(i as i32)).collect();
        columns.extend(expressions);

        Relation {
            name: "Project",
            arguments: None,
            columns,
            emit: get_emit(rel.common.as_ref()),
            children,
        }
    }
}

impl<'a> From<&'a Rel> for Relation<'a> {
    fn from(rel: &'a Rel) -> Self {
        match rel.rel_type.as_ref() {
            Some(RelType::Read(r)) => Relation::from(r.as_ref()),
            Some(RelType::Filter(r)) => Relation::from(r.as_ref()),
            Some(RelType::Project(r)) => Relation::from(r.as_ref()),
            Some(RelType::Aggregate(r)) => Relation::from(r.as_ref()),
            Some(RelType::Sort(r)) => Relation::from(r.as_ref()),
            Some(RelType::Fetch(r)) => Relation::from(r.as_ref()),
            Some(RelType::Join(r)) => Relation::from(r.as_ref()),
            _ => todo!(),
        }
    }
}

impl<'a> From<&'a AggregateRel> for Relation<'a> {
    /// Convert an AggregateRel to a Relation for textification.
    ///
    /// The conversion follows this logic:
    /// 1. Arguments: Group-by expressions (as Value::Expression)
    /// 2. Columns: All possible outputs in order:
    ///    - First: Group-by field references (Value::Reference)
    ///    - Then: Aggregate function measures (Value::AggregateFunction)
    /// 3. Emit: Uses the relation's emit mapping to select which outputs to display
    /// 4. Children: The input relation
    fn from(rel: &'a AggregateRel) -> Self {
        // Arguments: group-by fields (as expressions)
        let positional = rel
            .grouping_expressions
            .iter()
            .map(Value::Expression)
            .collect::<Vec<_>>();

        let arguments = Some(Arguments {
            positional,
            named: vec![],
        });
        // The columns are the direct outputs of this relation (before emit)
        let mut all_outputs: Vec<Value> = vec![];
        let input_field_count = rel.grouping_expressions.len();
        for i in 0..input_field_count {
            all_outputs.push(Value::Reference(i as i32));
        }

        // Then, add all measures (aggregate functions)
        // These are indexed after the group-by fields
        for m in &rel.measures {
            if let Some(agg_fn) = m.measure.as_ref() {
                all_outputs.push(Value::AggregateFunction(agg_fn));
            }
        }
        let emit = get_emit(rel.common.as_ref());
        Relation {
            name: "Aggregate",
            arguments,
            columns: all_outputs,
            emit,
            children: rel
                .input
                .as_ref()
                .map(|c| Some(Relation::from(c.as_ref())))
                .into_iter()
                .collect(),
        }
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
            let child = Relation::from(child);
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
            PlanRelType::Rel(rel) => Relation::from(rel).textify(ctx, w),
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

impl<'a> From<&'a SortRel> for Relation<'a> {
    fn from(rel: &'a SortRel) -> Self {
        let (children, columns) = Relation::convert_children(vec![rel.input.as_deref()]);
        let positional = rel.sorts.iter().map(Value::from).collect::<Vec<_>>();
        let arguments = Some(Arguments {
            positional,
            named: vec![],
        });
        // The columns are the direct outputs of this relation (before emit)
        let columns = (0..columns).map(|i| Value::Reference(i as i32)).collect();
        let emit = get_emit(rel.common.as_ref());
        Relation {
            name: "Sort",
            arguments,
            columns,
            emit,
            children,
        }
    }
}

impl<'a> From<&'a FetchRel> for Relation<'a> {
    fn from(rel: &'a FetchRel) -> Self {
        let (children, _columns) = Relation::convert_children(vec![rel.input.as_deref()]);
        let mut named_args = Vec::new();
        match &rel.count_mode {
            Some(CountMode::CountExpr(expr)) => {
                named_args.push(NamedArg {
                    name: "limit",
                    value: Value::Expression(expr),
                });
            }
            Some(CountMode::Count(val)) => {
                named_args.push(NamedArg {
                    name: "limit",
                    value: Value::Integer(*val as i32),
                });
            }
            None => {}
        }
        if let Some(offset) = &rel.offset_mode {
            match offset {
                substrait::proto::fetch_rel::OffsetMode::OffsetExpr(expr) => {
                    named_args.push(NamedArg {
                        name: "offset",
                        value: Value::Expression(expr),
                    });
                }
                substrait::proto::fetch_rel::OffsetMode::Offset(val) => {
                    named_args.push(NamedArg {
                        name: "offset",
                        value: Value::Integer(*val as i32),
                    });
                }
            }
        }

        let emit = get_emit(rel.common.as_ref());
        let columns = match emit {
            Some(EmitKind::Emit(e)) => e
                .output_mapping
                .iter()
                .map(|&i| Value::Reference(i))
                .collect(),
            _ => vec![],
        };
        Relation {
            name: "Fetch",
            arguments: Some(Arguments {
                positional: vec![],
                named: named_args,
            }),
            columns,
            emit,
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

impl<'a> From<&'a JoinRel> for Relation<'a> {
    fn from(rel: &'a JoinRel) -> Self {
        let (children, _total_columns) =
            Relation::convert_children(vec![rel.left.as_deref(), rel.right.as_deref()]);

        // Join relations must have exactly 2 children (left and right)
        assert_eq!(children.len(), 2, "JoinRel should have exactly 2 children");

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
            name: "Join",
            arguments,
            columns,
            emit,
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
    use substrait::proto::expression::literal::LiteralType;
    use substrait::proto::expression::{Literal, RexType, ScalarFunction};
    use substrait::proto::function_argument::ArgType;
    use substrait::proto::read_rel::{NamedTable, ReadType};
    use substrait::proto::rel_common::Emit;
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

        let rel = Relation::from(&read_rel);

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
            .with_uri(1, "test_uri")
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

        let rel = Relation::from(&rel);

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
        .with_uri(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
        .with_function(1, 10, "sum")
        .with_function(1, 11, "count");

        // Create a simple AggregateFunction
        let agg_fn = AggregateFunction {
            function_reference: 10, // sum
            arguments: vec![FunctionArgument {
                arg_type: Some(ArgType::Value(Expression {
                    rex_type: Some(RexType::Selection(Box::new(
                        FieldIndex(1).to_field_reference(),
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
        };

        let value = Value::AggregateFunction(&agg_fn);
        let (result, errors) = ctx.textify(&value);

        println!("Textification result: {result}");
        if !errors.is_empty() {
            println!("Errors: {errors:?}");
        }

        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert_eq!(result, "sum($1)");
    }

    #[test]
    fn test_aggregate_relation_textify() {
        let ctx = TestContext::new()
        .with_uri(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
        .with_function(1, 10, "sum")
        .with_function(1, 11, "count");

        // Create a simple AggregateRel
        let agg_fn1 = AggregateFunction {
            function_reference: 10, // sum
            arguments: vec![FunctionArgument {
                arg_type: Some(ArgType::Value(Expression {
                    rex_type: Some(RexType::Selection(Box::new(
                        FieldIndex(1).to_field_reference(),
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
        };

        let agg_fn2 = AggregateFunction {
            function_reference: 11, // count
            arguments: vec![FunctionArgument {
                arg_type: Some(ArgType::Value(Expression {
                    rex_type: Some(RexType::Selection(Box::new(
                        FieldIndex(1).to_field_reference(),
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
        };

        let aggregate_rel = AggregateRel {
            input: Some(Box::new(Rel {
                rel_type: Some(RelType::Read(Box::new(ReadRel {
                    common: None,
                    base_schema: Some(NamedStruct {
                        names: vec!["category".into(), "amount".into()],
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
                            ],
                            nullability: Nullability::Nullable as i32,
                        }),
                    }),
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
            grouping_expressions: vec![Expression {
                rex_type: Some(RexType::Selection(Box::new(
                    FieldIndex(0).to_field_reference(),
                ))),
            }],
            groupings: vec![],
            measures: vec![
                aggregate_rel::Measure {
                    measure: Some(agg_fn1),
                    filter: None,
                },
                aggregate_rel::Measure {
                    measure: Some(agg_fn2),
                    filter: None,
                },
            ],
            common: Some(RelCommon {
                emit_kind: Some(EmitKind::Emit(Emit {
                    output_mapping: vec![1, 2], // measures only
                })),
                ..Default::default()
            }),
            advanced_extension: None,
        };

        let relation = Relation::from(&aggregate_rel);
        let (result, errors) = ctx.textify(&relation);

        println!("Aggregate relation textification result:");
        println!("{result}");
        if !errors.is_empty() {
            println!("Errors: {errors:?}");
        }

        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        // Expected: Aggregate[$0 => sum($1), count($1)]
        assert!(result.contains("Aggregate[$0 => sum($1), count($1)]"));
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
                    name: "limit",
                    value: Value::Integer(10),
                },
                NamedArg {
                    name: "offset",
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

        let relation = Relation::from(&join_rel);
        let (result, errors) = ctx.textify(&relation);

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
        println!("Unknown join type result: {result}");
    }

    #[test]
    fn test_arguments_textify_both() {
        let ctx = TestContext::new();
        let args = Arguments {
            positional: vec![Value::Integer(1)],
            named: vec![NamedArg {
                name: "foo",
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
            name: "foo",
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
}
