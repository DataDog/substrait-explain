use std::fmt;

use substrait::proto::read_rel::{NamedTable, ReadType};
use substrait::proto::rel::RelType;
use substrait::proto::r#type::{Nullability, Struct};
use substrait::proto::{Expression, NamedStruct, ReadRel, Type};
use thiserror::Error;

use super::{Rule, ScopedParsePair};
use crate::extensions::SimpleExtensions;
use crate::parser::expressions::Name;
use crate::parser::{MessageParseError, ParsePair, RuleIter, unwrap_single_pair};

#[derive(Error, Debug, Clone)]
pub enum RelationParseError {
    #[error("Unexpected argument: {0}")]
    UnexpectedArgument(String),
    #[error("Incorrect number of children for {relation}: expected {expected}, found {found}")]
    IncorrectChildren {
        relation: String,
        expected: usize,
        found: usize,
    },
}

pub struct Relation {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub columns: Vec<Column>,
}

impl Relation {
    pub fn make_schema(&self) -> NamedStruct {
        let (names, types) = self
            .columns
            .iter()
            .cloned()
            .map(|c| (c.name, c.typ))
            .unzip();
        let struct_ = Struct {
            types,
            type_variation_reference: 0,
            nullability: Nullability::Required as i32,
        };

        NamedStruct {
            names,
            r#struct: Some(struct_),
        }
    }

    pub fn as_read(
        &self,
        children: Vec<substrait::proto::Rel>,
    ) -> Result<ReadRel, RelationParseError> {
        assert_eq!(self.name, "Read");

        // Expecting one child (the input relation)
        if !children.is_empty() {
            return Err(RelationParseError::IncorrectChildren {
                expected: 0,
                found: children.len(),
                relation: "Read".to_string(),
            });
        }

        let names = match self.arguments.as_slice() {
            [Argument::TableName(table_name)] => table_name.clone(),
            [a] => {
                return Err(RelationParseError::UnexpectedArgument(format!(
                    "Expected 1 argument of type TableName, got {a}",
                )));
            }
            _ => {
                return Err(RelationParseError::UnexpectedArgument(format!(
                    "Expected 1 argument of type TableName, got {} arguments",
                    self.arguments.len(),
                )));
            }
        };

        Ok(ReadRel {
            base_schema: Some(self.make_schema()),
            read_type: Some(ReadType::NamedTable(NamedTable {
                names,
                advanced_extension: None,
            })),
            ..Default::default()
        })
    }

    pub fn as_filter(
        &self,
        children: Vec<substrait::proto::Rel>,
    ) -> Result<substrait::proto::FilterRel, RelationParseError> {
        assert_eq!(self.name, "Filter");

        // Expecting one child (the input relation)
        let input = match children.as_slice() {
            [child] => Box::new(child.clone()),
            _ => {
                return Err(RelationParseError::IncorrectChildren {
                    expected: 1,
                    found: children.len(),
                    relation: "Filter".to_string(),
                });
            }
        };

        // Expecting one argument: the filter expression
        let condition = match self.arguments.as_slice() {
            [Argument::Expression(expr)] => Some(expr.clone()),
            [a] => {
                return Err(RelationParseError::UnexpectedArgument(format!(
                    "Expected 1 argument of type Expression, got {a}",
                )));
            }
            _ => {
                return Err(RelationParseError::UnexpectedArgument(format!(
                    "Expected 1 argument of type Expression, got {} arguments",
                    self.arguments.len(),
                )));
            }
        };

        Ok(substrait::proto::FilterRel {
            input: Some(input),
            condition: condition.map(Box::new),
            ..Default::default()
        })
    }

    pub fn convert_to_proto(
        &self,
        children: Vec<substrait::proto::Rel>,
    ) -> Result<substrait::proto::Rel, RelationParseError> {
        let rel_type = match self.name.as_str() {
            "Read" => RelType::Read(Box::new(self.as_read(children)?)),
            "Filter" => RelType::Filter(Box::new(self.as_filter(children)?)),
            _ => {
                todo!()
            }
        };

        Ok(substrait::proto::Rel {
            rel_type: Some(rel_type),
        })
    }
}

#[derive(Debug, Clone)]
pub enum Argument {
    TableName(Vec<String>),
    Expression(Expression),
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Argument::TableName(_) => write!(f, "TableName"),
            Argument::Expression(_) => write!(f, "Expression"),
        }
    }
}

impl ScopedParsePair for Argument {
    fn rule() -> Rule {
        Rule::argument
    }

    fn message() -> &'static str {
        "Argument"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let inner = unwrap_single_pair(pair);

        match inner.as_rule() {
            Rule::table_name => Ok(Self::TableName(TableName::parse_pair(inner).0)),
            Rule::expression => Ok(Self::Expression(Expression::parse_pair(extensions, inner)?)),
            _ => unreachable!("Unexpected rule: {:?}", inner.as_rule()),
        }
    }
}

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

pub struct Arguments(Vec<Argument>);

impl ScopedParsePair for Arguments {
    fn rule() -> Rule {
        Rule::arguments
    }

    fn message() -> &'static str {
        "Arguments"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Rule::arguments);
        let mut arguments = Vec::new();
        for arg in pair.into_inner() {
            arguments.push(Argument::parse_pair(extensions, arg)?);
        }
        Ok(Self(arguments))
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

impl ScopedParsePair for Relation {
    fn rule() -> Rule {
        Rule::relation
    }

    fn message() -> &'static str {
        "Relation"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        dbg!(&pair.as_rule());
        let mut iter = RuleIter::from(pair.into_inner());
        let name = iter.parse_next::<Name>().0;

        let arguments = iter.parse_next_scoped::<Arguments>(extensions)?.0;
        let columns = iter.parse_next_scoped::<NamedColumnList>(extensions)?.0;

        Ok(Relation {
            name,
            arguments,
            columns,
        })
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use super::*;
    use crate::parser::{ExpressionParser, Rule};

    #[test]
    fn test_parse_relation_no_args() {
        let try_parse = |r: Rule, s: &str| {
            let result = ExpressionParser::parse(r, s);
            if let Err(e) = result {
                println!("{:#}", e);
                panic!("Failed to parse {s} as {r:?}");
            }
        };
        try_parse(Rule::name, "Read");
        try_parse(Rule::table_name, "ab.cd.ef");
        try_parse(Rule::arguments, "ab.cd.ef");
        try_parse(Rule::named_column_list, "a:i32, b:string?");
        try_parse(Rule::relation, "Read[ab.cd.ef => a:i32, b:string?]");
    }

    #[test]
    fn test_parse_relation() {
        let extensions = SimpleExtensions::default();
        let relation = Relation::parse_pair(
            &extensions,
            parse_exact(Rule::relation, "Read[ab.cd.ef => a:i32, b:string?]"),
        )
        .unwrap();
        assert_eq!(relation.name, "Read");
        assert_eq!(relation.arguments.len(), 1);
        assert_eq!(relation.columns.len(), 2);
        assert_eq!(relation.columns[0].name, "a");
    }

    fn parse_exact(rule: Rule, input: &str) -> pest::iterators::Pair<Rule> {
        let mut pairs = ExpressionParser::parse(rule, input).unwrap();
        assert_eq!(pairs.as_str(), input);
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        pair
    }
}
