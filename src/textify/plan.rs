use std::fmt;

use substrait::proto;

use super::Textify;
use crate::extensions::SimpleExtensions;
use crate::parser::PLAN_HEADER;
use crate::textify::foundation::ErrorAccumulator;
use crate::textify::{OutputOptions, ScopedContext};

#[derive(Debug, Clone)]
pub struct PlanWriter<'a, E: ErrorAccumulator + Default> {
    options: &'a OutputOptions,
    extensions: SimpleExtensions,
    relations: &'a [proto::PlanRel],
    errors: E,
}

impl<'a, E: ErrorAccumulator + Default> PlanWriter<'a, E> {
    pub fn new(options: &'a OutputOptions, plan: &'a proto::Plan) -> Self {
        let (extensions, errs) =
            SimpleExtensions::from_extensions(&plan.extension_uris, &plan.extensions);

        let errors = E::default();
        for err in errs {
            errors.push(err.into());
        }

        let relations = plan.relations.as_slice();

        Self {
            options,
            extensions,
            relations,
            errors,
        }
    }

    pub fn scope(&'a self) -> ScopedContext<'a, E> {
        ScopedContext::new(self.options, &self.errors, &self.extensions)
    }

    pub fn write_extensions(&self, w: &mut impl fmt::Write) -> fmt::Result {
        self.extensions.write(w, &self.options.indent)
    }

    pub fn write_relations(&self, w: &mut impl fmt::Write) -> fmt::Result {
        // We always write the plan header, even if there are no relations.
        writeln!(w, "{PLAN_HEADER}")?;
        let scope = self.scope();
        for (i, relation) in self.relations.iter().enumerate() {
            if i > 0 {
                writeln!(w)?;
            }
            relation.textify(&scope, w)?;
        }
        Ok(())
    }
}

impl<'a, E: ErrorAccumulator + Default> fmt::Display for PlanWriter<'a, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write_extensions(f)?;
        if !self.extensions.is_empty() {
            writeln!(f)?;
        }
        self.write_relations(f)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use pext::simple_extension_declaration::{ExtensionFunction, MappingType};
    use substrait::proto::expression::{RexType, ScalarFunction};
    use substrait::proto::function_argument::ArgType;
    use substrait::proto::read_rel::{NamedTable, ReadType};
    use substrait::proto::r#type::{Kind, Nullability, Struct};
    use substrait::proto::{
        Expression, FunctionArgument, NamedStruct, ReadRel, Type, extensions as pext,
    };

    use super::*;
    use crate::parser::reference;
    use crate::textify::ErrorQueue;

    /// Test a fairly basic plan with an extension, read, and project.
    ///
    /// This has a manually constructed plan, rather than using the parser; more
    /// complete testing is in the integration tests.
    #[test]
    fn test_plan_writer() {
        let mut plan = proto::Plan::default();

        // Add extension URI
        plan.extension_uris.push(pext::SimpleExtensionUri {
            extension_uri_anchor: 1,
            uri: "https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml".to_string(),
        });

        // Add extension function declaration
        plan.extensions.push(pext::SimpleExtensionDeclaration {
            mapping_type: Some(MappingType::ExtensionFunction(ExtensionFunction {
                extension_uri_reference: 1,
                function_anchor: 10,
                name: "add".to_string(),
            })),
        });

        // Create read relation
        let read_rel = ReadRel {
            read_type: Some(ReadType::NamedTable(NamedTable {
                names: vec!["table1".to_string()],
                ..Default::default()
            })),
            base_schema: Some(NamedStruct {
                names: vec!["col1".to_string(), "col2".to_string()],
                r#struct: Some(Struct {
                    types: vec![
                        Type {
                            kind: Some(Kind::I32(proto::r#type::I32 {
                                nullability: Nullability::Nullable as i32,
                                type_variation_reference: 0,
                            })),
                        },
                        Type {
                            kind: Some(Kind::I32(proto::r#type::I32 {
                                nullability: Nullability::Nullable as i32,
                                type_variation_reference: 0,
                            })),
                        },
                    ],
                    ..Default::default()
                }),
            }),
            ..Default::default()
        };

        // Create project relation with add function
        let add_function = ScalarFunction {
            function_reference: 10,
            arguments: vec![
                FunctionArgument {
                    arg_type: Some(ArgType::Value(Expression {
                        rex_type: Some(RexType::Selection(Box::new(reference(0)))),
                    })),
                },
                FunctionArgument {
                    arg_type: Some(ArgType::Value(Expression {
                        rex_type: Some(RexType::Selection(Box::new(reference(1)))),
                    })),
                },
            ],
            options: vec![],
            output_type: None,
            #[allow(deprecated)]
            args: vec![],
        };

        let project_rel = proto::ProjectRel {
            expressions: vec![Expression {
                rex_type: Some(RexType::ScalarFunction(add_function)),
            }],
            input: Some(Box::new(proto::Rel {
                rel_type: Some(proto::rel::RelType::Read(Box::new(read_rel))),
            })),
            common: None,
            advanced_extension: None,
        };

        // Add relations to plan
        plan.relations.push(proto::PlanRel {
            rel_type: Some(proto::plan_rel::RelType::Rel(proto::Rel {
                rel_type: Some(proto::rel::RelType::Project(Box::new(project_rel))),
            })),
        });

        let options = OutputOptions::default();
        let writer = PlanWriter::<ErrorQueue>::new(&options, &plan);
        let mut output = String::new();
        write!(output, "{}", writer).unwrap();

        let expected = r#"
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: add

=== Plan
Project[$0, $1, add($0, $1)]
  Read[table1 => col1:i32?, col2:i32?]"#
            .trim_start();

        assert_eq!(
            output, expected,
            "Output:\n---\n{}\n---\nExpected:\n---\n{}\n---",
            output, expected
        );
    }
}
