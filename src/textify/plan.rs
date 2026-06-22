use std::fmt;

use substrait::proto;

use super::Textify;
use crate::extensions::{ExtensionRegistry, SimpleExtensions};
use crate::parser::{HEADER_HEADER, PLAN_HEADER};
use crate::textify::foundation::ErrorAccumulator;
use crate::textify::{OutputOptions, ScopedContext};

#[derive(Debug, Clone)]
pub(crate) struct PlanWriter<'a, E: ErrorAccumulator + Default> {
    options: &'a OutputOptions,
    version: Option<&'a proto::Version>,
    extensions: SimpleExtensions,
    relations: &'a [proto::PlanRel],
    errors: E,
    extension_registry: &'a ExtensionRegistry,
}

/// Returns true if the version carries no information worth writing: all three
/// version numbers are zero and both string fields are empty.
fn version_is_empty(version: &proto::Version) -> bool {
    version.major_number == 0
        && version.minor_number == 0
        && version.patch_number == 0
        && version.git_hash.is_empty()
        && version.producer.is_empty()
}

impl<'a, E: ErrorAccumulator + Default + Clone> PlanWriter<'a, E> {
    pub(crate) fn new(
        options: &'a OutputOptions,
        plan: &'a proto::Plan,
        extension_registry: &'a ExtensionRegistry,
    ) -> (Self, E) {
        let (extensions, errs) =
            SimpleExtensions::from_extensions(&plan.extension_urns, &plan.extensions);

        let errors = E::default();
        for err in errs {
            errors.push(err.into());
        }

        let relations = plan.relations.as_slice();

        (
            Self {
                options,
                version: plan.version.as_ref(),
                extensions,
                relations,
                errors: errors.clone(),
                extension_registry,
            },
            errors,
        )
    }

    /// Returns true if there is a header worth writing.
    pub(crate) fn has_header(&self) -> bool {
        self.version.is_some_and(|v| !version_is_empty(v))
    }

    /// Write the `=== Header` section, if the plan carries any header metadata.
    ///
    /// When any version number is non-zero, `producer` and `git_hash` are
    /// nested under a compact `Version:` line. When all version numbers are
    /// zero but metadata is present, the metadata is written at the top level
    /// with no `Version:` line.
    pub(crate) fn write_header(&self, w: &mut impl fmt::Write) -> fmt::Result {
        let Some(version) = self.version else {
            return Ok(());
        };
        if version_is_empty(version) {
            return Ok(());
        }

        writeln!(w, "{HEADER_HEADER}")?;

        let indent = &self.options.indent;
        let has_numbers =
            version.major_number != 0 || version.minor_number != 0 || version.patch_number != 0;

        if has_numbers {
            writeln!(
                w,
                "Version: {}.{}.{}",
                version.major_number, version.minor_number, version.patch_number
            )?;
            if !version.producer.is_empty() {
                writeln!(w, "{indent}producer: \"{}\"", version.producer)?;
            }
            if !version.git_hash.is_empty() {
                writeln!(w, "{indent}git_hash: {}", version.git_hash)?;
            }
        } else {
            // No version numbers: emit metadata at the top level, with no
            // `Version:` line to nest it under.
            if !version.producer.is_empty() {
                writeln!(w, "producer: \"{}\"", version.producer)?;
            }
            if !version.git_hash.is_empty() {
                writeln!(w, "git_hash: {}", version.git_hash)?;
            }
        }

        Ok(())
    }

    pub(crate) fn scope(&'a self) -> ScopedContext<'a, E> {
        ScopedContext::new(
            self.options,
            &self.errors,
            &self.extensions,
            self.extension_registry,
        )
    }

    pub(crate) fn write_extensions(&self, w: &mut impl fmt::Write) -> fmt::Result {
        self.extensions.write(w, &self.options.indent)
    }

    pub(crate) fn write_relations(&self, w: &mut impl fmt::Write) -> fmt::Result {
        // We always write the plan header, even if there are no relations.
        writeln!(w, "{PLAN_HEADER}")?;
        let scope = self.scope();
        for (i, relation) in self.relations.iter().enumerate() {
            if i > 0 {
                writeln!(w)?;
                writeln!(w)?;
            }
            relation.textify(&scope, w)?;
        }
        Ok(())
    }
}

impl<'a, E: ErrorAccumulator + Default> fmt::Display for PlanWriter<'a, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.has_header() {
            self.write_header(f)?;
            writeln!(f)?;
        }
        self.write_extensions(f)?;
        if !self.extensions.is_empty() {
            writeln!(f)?;
        }
        self.write_relations(f)?;
        writeln!(f)?;
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
    use substrait::proto::r#type::{I64, Kind, Nullability, Struct};
    use substrait::proto::{
        Expression, FunctionArgument, NamedStruct, ReadRel, Type, extensions as pext,
    };

    use super::*;
    use crate::parser::expressions::FieldIndex;
    use crate::textify::ErrorQueue;

    /// Test a fairly basic plan with an extension, read, and project.
    ///
    /// This has a manually constructed plan, rather than using the parser; more
    /// complete testing is in the integration tests.
    #[test]
    fn test_plan_writer() {
        let mut plan = proto::Plan::default();

        // Add extension URN
        plan.extension_urns.push(pext::SimpleExtensionUrn {
            extension_urn_anchor: 1,
            urn: "https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml".to_string(),
        });

        // Add extension function declaration
        plan.extensions.push(pext::SimpleExtensionDeclaration {
            mapping_type: Some(MappingType::ExtensionFunction(ExtensionFunction {
                extension_urn_reference: 1,
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
                        rex_type: Some(RexType::Selection(Box::new(
                            FieldIndex(0).to_field_reference(),
                        ))),
                    })),
                },
                FunctionArgument {
                    arg_type: Some(ArgType::Value(Expression {
                        rex_type: Some(RexType::Selection(Box::new(
                            FieldIndex(1).to_field_reference(),
                        ))),
                    })),
                },
            ],
            options: vec![],
            output_type: Some(Type {
                kind: Some(Kind::I64(I64 {
                    nullability: Nullability::Required as i32,
                    type_variation_reference: 0,
                })),
            }),
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
        let extension_registry = ExtensionRegistry::new();
        let (writer, errors) = PlanWriter::<ErrorQueue>::new(&options, &plan, &extension_registry);
        let mut output = String::new();
        write!(output, "{writer}").unwrap();

        // Assert that there are no errors
        let errors: Vec<_> = errors.into();
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");

        let expected = r#"
=== Extensions
URNs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: add

=== Plan
Project[$0, $1, add($0, $1):i64]
  Read[table1 => col1:i32?, col2:i32?]
"#
        .trim_start();

        assert_eq!(
            output, expected,
            "Output:\n---\n{output}\n---\nExpected:\n---\n{expected}\n---"
        );
    }
}
