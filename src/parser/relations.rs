use substrait::proto::rel_common::{Emit, EmitKind};
use substrait::proto::{Expression, Type};

use super::{Rule, ScopedParsePair};
use crate::extensions::SimpleExtensions;
use crate::parser::expressions::Name;
use crate::parser::{MessageParseError, ParsePair, RuleIter};

pub struct TableName(Vec<String>);

impl ParsePair for TableName {
    fn rule() -> Rule {
        Rule::table_name
    }

    fn message() -> &'static str {
        "TableName"
    }

    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let pairs = pair.into_inner();
        let mut names = Vec::with_capacity(pairs.len());
        let mut iter = RuleIter::from(pairs);
        while let Some(name) = iter.parse_if_next::<Name>() {
            names.push(name.0);
        }
        iter.done();
        Self(names)
    }
}

#[derive(Debug, Clone)]
pub struct Column {
    pub name: String,
    pub typ: Type,
}

impl ScopedParsePair for Column {
    fn rule() -> Rule {
        Rule::named_column
    }

    fn message() -> &'static str {
        "Column"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut iter = RuleIter::from(pair.into_inner());
        let name = iter.parse_next::<Name>().0;
        let typ = iter.parse_next_scoped(extensions)?;
        iter.done();
        Ok(Self { name, typ })
    }
}

pub struct NamedColumnList(Vec<Column>);

impl ScopedParsePair for NamedColumnList {
    fn rule() -> Rule {
        Rule::named_column_list
    }

    fn message() -> &'static str {
        "NamedColumnList"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut columns = Vec::new();
        for col in pair.into_inner() {
            columns.push(Column::parse_pair(extensions, col)?);
        }
        Ok(Self(columns))
    }
}

impl ScopedParsePair for substrait::proto::ReadRel {
    fn rule() -> Rule {
        Rule::read_relation
    }

    fn message() -> &'static str {
        "ReadRel"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        let mut pairs = pair.into_inner();
        let table = TableName::parse_pair(pairs.next().unwrap()).0;
        let columns = NamedColumnList::parse_pair(extensions, pairs.next().unwrap())?.0;
        assert!(pairs.next().is_none());

        let (names, types): (Vec<_>, Vec<_>) = columns.into_iter().map(|c| (c.name, c.typ)).unzip();
        let struct_ = substrait::proto::r#type::Struct {
            types,
            type_variation_reference: 0,
            nullability: substrait::proto::r#type::Nullability::Required as i32,
        };
        let named_struct = substrait::proto::NamedStruct {
            names,
            r#struct: Some(struct_),
        };

        Ok(substrait::proto::ReadRel {
            base_schema: Some(named_struct),
            read_type: Some(substrait::proto::read_rel::ReadType::NamedTable(
                substrait::proto::read_rel::NamedTable {
                    names: table,
                    advanced_extension: None,
                },
            )),
            ..Default::default()
        })
    }
}

impl ScopedParsePair for substrait::proto::FilterRel {
    fn rule() -> Rule {
        Rule::filter_relation
    }

    fn message() -> &'static str {
        "FilterRel"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        let mut pairs = pair.into_inner();
        let condition = Expression::parse_pair(extensions, pairs.next().unwrap())?;
        let references_pair = pairs.next().unwrap();
        let output_mapping = references_pair
            .into_inner()
            .map(|p| {
                let inner = crate::parser::unwrap_single_pair(p);
                inner.as_str().parse::<i32>().unwrap()
            })
            .collect::<Vec<i32>>();

        let emit = EmitKind::Emit(Emit { output_mapping });
        let common = substrait::proto::RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };

        assert!(pairs.next().is_none());
        Ok(substrait::proto::FilterRel {
            input: None,
            condition: Some(Box::new(condition)),
            common: Some(common),
            advanced_extension: None,
        })
    }
}

/// Parse a ProjectRel with input context
pub fn parse_project_relation(
    extensions: &SimpleExtensions,
    pair: pest::iterators::Pair<Rule>,
    input_field_count: usize,
    input_rel: Box<substrait::proto::Rel>,
) -> Result<substrait::proto::ProjectRel, MessageParseError> {
    let mut pairs = pair.into_inner();
    let arguments_pair = pairs.next().unwrap();
    assert!(pairs.next().is_none());

    let mut expressions = Vec::new();
    let mut output_mapping = Vec::new();

    for arg in arguments_pair.into_inner() {
        // Each project_argument contains either a reference or expression
        let inner_arg = arg.into_inner().next().unwrap();
        match inner_arg.as_rule() {
            Rule::reference => {
                // Parse reference like "$0" -> 0
                let inner = crate::parser::unwrap_single_pair(inner_arg);
                let ref_index = inner.as_str().parse::<i32>().unwrap();
                output_mapping.push(ref_index);
            }
            Rule::expression => {
                let expr = Expression::parse_pair(extensions, inner_arg)?;
                expressions.push(expr);
                // Expression: index after all input fields
                output_mapping.push(input_field_count as i32 + (expressions.len() as i32 - 1));
            }
            _ => panic!("Unexpected inner argument rule: {:?}", inner_arg.as_rule()),
        }
    }

    let emit = EmitKind::Emit(Emit { output_mapping });
    let common = substrait::proto::RelCommon {
        emit_kind: Some(emit),
        ..Default::default()
    };

    Ok(substrait::proto::ProjectRel {
        input: Some(input_rel),
        expressions,
        common: Some(common),
        advanced_extension: None,
    })
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use super::*;
    use crate::parser::{ExpressionParser, Rule};

    #[test]
    fn test_parse_relation() {
        // Removed: test_parse_relation for old Relation struct
    }

    #[test]
    fn test_parse_read_relation() {
        let extensions = SimpleExtensions::default();
        let read = substrait::proto::ReadRel::parse_pair(
            &extensions,
            parse_exact(Rule::read_relation, "Read[ab.cd.ef => a:i32, b:string?]"),
        )
        .unwrap();
        let names = match &read.read_type {
            Some(substrait::proto::read_rel::ReadType::NamedTable(table)) => &table.names,
            _ => panic!("Expected NamedTable"),
        };
        assert_eq!(names, &["ab", "cd", "ef"]);
        let columns = &read
            .base_schema
            .as_ref()
            .unwrap()
            .r#struct
            .as_ref()
            .unwrap()
            .types;
        assert_eq!(columns.len(), 2);
    }

    #[test]
    fn test_parse_filter_relation() {
        let extensions = SimpleExtensions::default();
        let filter = substrait::proto::FilterRel::parse_pair(
            &extensions,
            parse_exact(Rule::filter_relation, "Filter[$1 => $0, $1, $2]"),
        )
        .unwrap();
        let emit_kind = &filter.common.as_ref().unwrap().emit_kind.as_ref().unwrap();
        let emit = match emit_kind {
            EmitKind::Emit(emit) => &emit.output_mapping,
            _ => panic!("Expected EmitKind::Emit, got {:?}", emit_kind),
        };
        assert_eq!(emit, &[0, 1, 2]);
    }

    #[test]
    fn test_parse_project_relation() {
        let extensions = SimpleExtensions::default();
        let project = parse_project_relation(
            &extensions,
            parse_exact(Rule::project_relation, "Project[$0, $1, 42]"),
            5, // Assume 5 input fields
            Box::default(),
        )
        .unwrap();

        // Should have 1 expression (42) and 2 references ($0, $1)
        assert_eq!(project.expressions.len(), 1);

        let emit_kind = &project.common.as_ref().unwrap().emit_kind.as_ref().unwrap();
        let emit = match emit_kind {
            EmitKind::Emit(emit) => &emit.output_mapping,
            _ => panic!("Expected EmitKind::Emit, got {:?}", emit_kind),
        };
        // Output mapping should be [0, 1, 5] (reference 0, reference 1, expression 0 at index 5)
        assert_eq!(emit, &[0, 1, 5]);
    }

    #[test]
    fn test_parse_project_relation_complex() {
        let extensions = SimpleExtensions::default();
        let project = parse_project_relation(
            &extensions,
            parse_exact(Rule::project_relation, "Project[42, $0, 100, $2, $1]"),
            5, // Assume 5 input fields
            Box::default(),
        )
        .unwrap();

        // Should have 2 expressions (42, 100) and 3 references ($0, $2, $1)
        assert_eq!(project.expressions.len(), 2);

        let emit_kind = &project.common.as_ref().unwrap().emit_kind.as_ref().unwrap();
        let emit = match emit_kind {
            EmitKind::Emit(emit) => &emit.output_mapping,
            _ => panic!("Expected EmitKind::Emit, got {:?}", emit_kind),
        };
        // Direct mapping: [input_fields..., 42, 100] (input fields first, then expressions)
        // Output mapping: [5, 0, 6, 2, 1] (to get: 42, $0, 100, $2, $1)
        assert_eq!(emit, &[5, 0, 6, 2, 1]);
    }

    fn parse_exact(rule: Rule, input: &str) -> pest::iterators::Pair<Rule> {
        let mut pairs = ExpressionParser::parse(rule, input).unwrap();
        assert_eq!(pairs.as_str(), input);
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        pair
    }
}
