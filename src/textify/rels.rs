use std::fmt;

use substrait::proto::rel::RelType;
use substrait::proto::{ReadRel, Rel};

use super::{Scope, Textify, TextifyError};

impl Textify for Rel {
    fn name() -> &'static str {
        "Rel"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}", ctx.expect(&self.rel_type))
    }
}

impl Textify for ReadRel {
    fn name() -> &'static str {
        "ReadRel"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, _ctx: &S, _w: &mut W) -> fmt::Result {
        todo!();
        // let mut first = true;

        // let names = self.base_schema.as_ref().map(|s| &s.names).iter().flatten();

        // for field in self.base_schema.as_ref().flatmap(|s| &s.names).iter().flatten() {
        //     if !first {
        //         w.write_str(", ")?;
        //     }
        //     first = false;
        // }

        //     w.write_str(": ")?;
        //     w.write_str(&field.type_)?;
        // }
    }
}

impl Textify for RelType {
    fn name() -> &'static str {
        "RelType"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let mut unimplemented = |name: &'static str| {
            let token = ctx.failure(TextifyError::unimplemented(
                "RelType",
                Some(name),
                "RelType not yet implemented",
            ));
            write!(w, "{}", token)
        };

        match self {
            RelType::Read(_r) => unimplemented("Read"),

            RelType::Filter(_r) => unimplemented("Filter"),
            RelType::Project(_r) => unimplemented("Project"),
            RelType::Fetch(_r) => unimplemented("Fetch"),

            RelType::Aggregate(_r) => unimplemented("Aggregate"),
            RelType::Sort(_r) => unimplemented("Sort"),

            RelType::HashJoin(_r) => unimplemented("HashJoin"),
            RelType::Exchange(_r) => unimplemented("Exchange"),
            RelType::Window(_r) => unimplemented("Window"),

            RelType::Join(_r) => unimplemented("Join"),

            RelType::Set(_r) => unimplemented("Set"),

            RelType::ExtensionSingle(_r) => unimplemented("ExtensionSingle"),

            RelType::ExtensionMulti(_r) => unimplemented("ExtensionMulti"),

            RelType::ExtensionLeaf(_r) => unimplemented("ExtensionLeaf"),

            RelType::Cross(_r) => unimplemented("Cross"),

            RelType::Reference(_r) => unimplemented("Reference"),

            RelType::Write(_r) => unimplemented("Write"),

            RelType::Ddl(_r) => unimplemented("Ddl"),

            RelType::Update(_r) => unimplemented("Update"),

            RelType::MergeJoin(_r) => unimplemented("MergeJoin"),

            RelType::NestedLoopJoin(_r) => unimplemented("NestedLoopJoin"),

            RelType::Expand(_r) => unimplemented("Expand"),
        }
    }
}
