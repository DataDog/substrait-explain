//! Semantic relation-line payloads used between parse and lower phases.
//!
//! These types decouple "parse typed syntax" from "build protobuf relation",
//! so structural parsing can store parsed relation semantics without reparsing.

use substrait::proto::expression::Literal;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::fetch_rel::{CountMode, OffsetMode};
use substrait::proto::sort_field::SortDirection;
use substrait::proto::{AggregateFunction, Expression, Type, join_rel};

use crate::parser::convert::{FieldIndex, Name, RelationOutputIndex, TablePath};

#[derive(Debug, Clone)]
pub(crate) struct Column {
    /// Output column name.
    pub name: Name,
    /// Output column type.
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub(crate) enum ProjectArgument {
    /// Passthrough field reference.
    Reference(FieldIndex),
    /// Computed expression.
    Expression(Box<Expression>),
}

#[derive(Debug, Clone)]
pub(crate) enum AggregateOutputItem {
    /// Grouping reference in aggregate output.
    Reference(FieldIndex),
    /// Aggregate measure output.
    Measure(AggregateFunction),
}

#[derive(Debug, Clone)]
pub(crate) struct SortFieldSpec {
    /// Referenced field.
    pub field: FieldIndex,
    /// Parsed sort direction.
    pub direction: SortDirection,
}

#[derive(Debug, Clone)]
pub(crate) enum FetchValue {
    /// Integer fetch value from text.
    Integer(i64),
    /// Expression fetch value from text.
    Expression(Box<Expression>),
}

impl FetchValue {
    pub fn into_count_mode(self) -> CountMode {
        match self {
            FetchValue::Integer(value) => CountMode::CountExpr(i64_literal_expr(value)),
            FetchValue::Expression(expression) => CountMode::CountExpr(expression),
        }
    }

    pub fn into_offset_mode(self) -> OffsetMode {
        match self {
            FetchValue::Integer(value) => OffsetMode::OffsetExpr(i64_literal_expr(value)),
            FetchValue::Expression(expression) => OffsetMode::OffsetExpr(expression),
        }
    }
}

fn i64_literal_expr(value: i64) -> Box<Expression> {
    Box::new(Expression {
        rex_type: Some(substrait::proto::expression::RexType::Literal(Literal {
            nullable: false,
            type_variation_reference: 0,
            literal_type: Some(LiteralType::I64(value)),
        })),
    })
}

/// Parsed representation of one non-extension relation line.
#[derive(Debug, Clone)]
pub(crate) enum StandardRelationLine {
    Read {
        table: TablePath,
        columns: Vec<Column>,
    },
    Filter {
        condition: Expression,
        emit: Vec<RelationOutputIndex>,
    },
    Project {
        args: Vec<ProjectArgument>,
    },
    Aggregate {
        group_by: Vec<FieldIndex>,
        outputs: Vec<AggregateOutputItem>,
    },
    Sort {
        fields: Vec<SortFieldSpec>,
        emit: Vec<RelationOutputIndex>,
    },
    Fetch {
        limit: Option<FetchValue>,
        offset: Option<FetchValue>,
        emit: Vec<RelationOutputIndex>,
    },
    Join {
        join_type: join_rel::JoinType,
        expression: Box<Expression>,
        emit: Vec<RelationOutputIndex>,
    },
}
