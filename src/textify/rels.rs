use std::fmt;

use substrait::proto::plan_rel::RelType as PlanRelType;
use substrait::proto::read_rel::ReadType;
use substrait::proto::rel::RelType;
use substrait::proto::rel_common::EmitKind;
use substrait::proto::{
    AggregateFunction, AggregateRel, Expression, FilterRel, NamedStruct, PlanRel, ProjectRel,
    ReadRel, Rel, RelCommon, RelRoot, Type,
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
    Missing(PlanError),
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

            write!(w, "{}", ctx.expect(self.values.get(index as usize)))?;
        }

        Ok(())
    }
}

pub struct Relation<'a> {
    pub name: &'a str,
    pub arguments: Vec<Value<'a>>,
    pub columns: Vec<Value<'a>>,
    pub emit: Option<&'a EmitKind>,
    // The children of this relation - its inputs.
    pub children: Vec<Option<Relation<'a>>>,
}

impl Textify for Relation<'_> {
    fn name() -> &'static str {
        "Relation"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let args = ctx.separated(self.arguments.iter(), ", ");
        let cols = Emitted::new(&self.columns, self.emit);

        let indent = ctx.indent();
        let name = self.name;
        let cols = ctx.display(&cols);
        if self.arguments.is_empty() {
            write!(w, "{indent}{name}[{cols}]")?;
        } else {
            write!(w, "{indent}{name}[{args} => {cols}]")?;
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
        let named: Value = match name {
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
            arguments: vec![named],
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
        let emit = get_emit(rel.common.as_ref());
        let (children, columns) = Relation::convert_children(vec![rel.input.as_deref()]);
        let columns = (0..columns).map(|i| Value::Reference(i as i32)).collect();

        Relation {
            name: "Filter",
            arguments: vec![condition],
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
            arguments: vec![],
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
        let arguments = rel
            .grouping_expressions
            .iter()
            .map(Value::Expression)
            .collect();

        // Build all possible outputs in the correct order
        let mut all_outputs: Vec<Value> = vec![];

        // First, add all input fields (group-by references)
        // These are indexed 0..group_by_count in the output
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

        // Get the emit mapping to select the correct outputs
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
                        crate::parser::expressions::reference(1),
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
                        crate::parser::expressions::reference(1),
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
                        crate::parser::expressions::reference(1),
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
                    crate::parser::expressions::reference(0),
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
}
