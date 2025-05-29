use std::fmt;

use substrait::proto::{ReadRel, Rel, rel::RelType};

use crate::{
    TextifyError,
    textify::{Scope, Textify},
};

impl Textify for Rel {
    fn name() -> &'static str {
        "Rel"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &mut S, w: &mut W) -> fmt::Result {
        ctx.expect(w, &self.rel_type)
    }
}

impl Textify for ReadRel {
    fn name() -> &'static str {
        "ReadRel"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &mut S, w: &mut W) -> fmt::Result {
        let mut first = true;

        let names = self.base_schema.as_ref().map(|s| &s.names).iter().flatten();

        for field in self.base_schema.as_ref()latmap(|s| &s.names).iter().flatten() {
            if !first {
                w.write_str(", ")?;
            }
            first = false;
        }

        //     w.write_str(": ")?;
        //     w.write_str(&field.type_)?;
        // }

        todo!()
    }
}

impl Textify for RelType {
    fn name() -> &'static str {
        "RelType"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &mut S, w: &mut W) -> fmt::Result {
        match self {
            RelType::Read(_r) => ctx.failure(
                w,
                TextifyError::unimplemented("RelType", Some("Read"), "RelType not yet implemented"),
            ),

            RelType::Filter(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("Filter"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::Project(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("Project"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::Fetch(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("Fetch"),
                    "RelType not yet implemented",
                ),
            ),

            RelType::Aggregate(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("Aggregate"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::Sort(_r) => ctx.failure(
                w,
                TextifyError::unimplemented("RelType", Some("Sort"), "RelType not yet implemented"),
            ),

            RelType::HashJoin(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("HashJoin"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::Exchange(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("Exchange"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::Window(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("Window"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::Join(_r) => ctx.failure(
                w,
                TextifyError::unimplemented("RelType", Some("Join"), "RelType not yet implemented"),
            ),
            RelType::Set(_r) => ctx.failure(
                w,
                TextifyError::unimplemented("RelType", Some("Set"), "RelType not yet implemented"),
            ),
            RelType::ExtensionSingle(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("ExtensionSingle"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::ExtensionMulti(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("ExtensionMulti"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::ExtensionLeaf(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("ExtensionLeaf"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::Cross(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("Cross"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::Reference(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("Reference"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::Write(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("Write"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::Ddl(_r) => ctx.failure(
                w,
                TextifyError::unimplemented("RelType", Some("Ddl"), "RelType not yet implemented"),
            ),
            RelType::Update(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("Update"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::MergeJoin(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("MergeJoin"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::NestedLoopJoin(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("NestedLoopJoin"),
                    "RelType not yet implemented",
                ),
            ),
            RelType::Expand(_r) => ctx.failure(
                w,
                TextifyError::unimplemented(
                    "RelType",
                    Some("Expand"),
                    "RelType not yet implemented",
                ),
            ),
        }
    }
}
