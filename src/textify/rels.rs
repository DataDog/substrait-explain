use std::fmt;

use substrait::proto::read_rel::ReadType;
use substrait::proto::rel::RelType;
use substrait::proto::rel_common::EmitKind;
use substrait::proto::{
    Expression, FilterRel, NamedStruct, ProjectRel, ReadRel, Rel, RelCommon, Type,
};

use super::expressions::Reference;
use super::types::Name;
use super::{Scope, Textify, TextifyError};

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
    Missing(TextifyError),
}

impl<'a> Value<'a> {
    pub fn expect(maybe_value: Option<Self>, f: impl FnOnce() -> TextifyError) -> Self {
        match maybe_value {
            Some(s) => s,
            None => Value::Missing(f()),
        }
    }
}

impl<'a> From<Result<Vec<Name<'a>>, TextifyError>> for Value<'a> {
    fn from(token: Result<Vec<Name<'a>>, TextifyError>) -> Self {
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
    pub name: &'static str,
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

        write!(w, "{}[{} | {}]", self.name, args, ctx.display(&cols))?;
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

// /// A RelRef is a tree of relations in a uniform format.
// pub struct RelRef<'a> {
//     pub rel_type: &'a RelType,
//     // The number of column inputs. We don't track types here.
//     pub inputs: usize,
//     // The children of this relation. None means the child is missing.
//     pub children: Vec<Option<RelRef<'a>>>,
// }

// impl<'a> RelRef<'a> {
//     pub fn new(rel_type: &'a RelType, inputs: usize, children: Vec<Option<RelRef<'a>>>) -> Self {
//         Self {
//             rel_type,
//             inputs,
//             children,
//         }
//     }
// }

// #[derive(Debug, Clone, Error)]
// #[error("Expected {expected} children, found {found}")]
// pub struct MissingChild<'a> {
//     // The relation that is missing a child. TODO: Add its name or path or
//     // something to the error message.
//     pub rel: &'a Rel,
//     pub expected: usize,
//     pub found: usize,
// }

// impl<'a> RelRef<'a> {
//     pub fn assemble(rel: &'a RelType, children: Vec<Option<RelRef<'a>>>) -> Self {
//         let children = rel.iter().map(|r| r.map(|r| RelRef::from(r))).collect();
//         todo!()
//     }
// }

// impl<'a> From<&'a Rel> for RelRef<'a> {
//     fn from(rel: &'a Rel) -> Self {
//         let mut errors = vec![];
//         let rel = match rel.rel_type {
//             Some(ref rt @ RelType::Read(_)) => Self {
//                 rel_type: rt,
//                 inputs: 0,
//                 children: vec![],
//             },
//             Some(ref rt @ RelType::Filter(ref f)) => {
//                 let (children, child_errors) = convert_children(vec![f.input.as_deref()], rel);
//                 let inputs = children.iter().map(|c| c.inputs).sum();
//                 errors.extend(child_errors);
//                 Self {
//                     rel_type: rt,
//                     inputs,
//                     children,
//                 }
//             }
//             Some(ref rt @ RelType::Project(ref p)) => {
//                 let (children, child_errors) = convert_children(vec![p.input.as_deref()], rel);
//                 let inputs = children.iter().map(|c| c.inputs).sum();
//                 errors.extend(child_errors);
//                 Self {
//                     rel_type: rt,
//                     inputs,
//                     children,
//                 }
//             }

//             _ => todo!(),
//         };

//         (rel, errors)
//     }
// }

// impl<'a> Textify for Rel {
//     fn name() -> &'static str {
//         "Rel"
//     }

//     fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
//         let rel_type = match &self.rel_type {
//             Some(RelType::Read(r)) => Relation::from(r.as_ref()),
//             Some(RelType::Filter(r)) => Relation::from_filter(
//                 r.as_ref(),
//                 &Relation::from(r.input.as_ref().unwrap().as_ref()),
//             ),
//             Some(r) => {
//                 return write!(
//                     w,
//                     "{}",
//                     ctx.failure(TextifyError::unimplemented(
//                         "Rel",
//                         Some(format!("{:?}", r)),
//                         "Reltype not yet implemented",
//                     ))
//                 );
//             }

//             None => {
//                 return write!(
//                     w,
//                     "{}",
//                     ctx.failure(TextifyError::invalid(
//                         "Rel",
//                         Some("rel_type"),
//                         "Rel type is required",
//                     ))
//                 );
//             }
//         };

//         rel_type.textify(ctx, w)
//     }
// }

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

pub fn get_table_name(rel: Option<&ReadType>) -> Result<&[String], TextifyError> {
    match rel {
        Some(ReadType::NamedTable(r)) => Ok(r.names.as_slice()),
        _ => Err(TextifyError::unimplemented(
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
                let err = TextifyError::unimplemented(
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
            TextifyError::unimplemented("FilterRel", Some("condition"), "Condition is None")
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
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use substrait::proto::expression::literal::LiteralType;
    use substrait::proto::expression::{Literal, RexType, ScalarFunction};
    use substrait::proto::function_argument::ArgType;
    use substrait::proto::read_rel::{NamedTable, ReadType};
    use substrait::proto::r#type::{self as ptype, Kind, Nullability, Struct};
    use substrait::proto::{FunctionArgument, NamedStruct, ReadRel, Type};

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
        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
        assert_eq!(
            result,
            "Read[some_db.test_table | col1:i32?, \"column 2\":string?]"
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
        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
        assert_eq!(result, "Filter[gt($0, 10:i32) | $0, $1]");

        let child = rel.children[0].as_ref().unwrap();
        let (cs, cerr) = ctx.textify(child);

        assert!(cerr.is_empty(), "Expected no errors, got: {:?}", cerr);
        assert_eq!(cs, "Read[test_table | col1:i32?, col2:i32?]");
    }
}
