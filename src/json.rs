//! The standard `serde`/pbjson JSON encoding used by Rust stores `google.protobuf.Any`
//! fields as `{"typeUrl": "...", "value": "<base64>"}`. Go's `protojson` library uses a
//! different encoding: `{"@type": "...", "field1": val, ...}` where the concrete message's
//! fields are inlined. `serde_json::from_str::<Plan>` fails on Go-produced JSON because it
//! only understands the `typeUrl/value` form.
//!
//! [`prost_reflect::DynamicMessage`] implements the full protobuf JSON mapping spec and
//! handles both forms, as long as the `DescriptorPool` contains the schema for every type
//! URL referenced in the JSON. 
//! 
//! This module exposes [`build_descriptor_pool`] (to construct
//! the pool, optionally merging in extra descriptor blobs for extension types) and
//! [`parse_json`] (to parse a JSON string into a [`Plan`] using the pool).
//!
//! # Example
//!
//! ```rust,ignore
//! use substrait_explain::json::{build_descriptor_pool, parse_json};
//! 
//! static MY_EXT: &[u8] = include_bytes!("my_extensions.bin");
//! let pool = build_descriptor_pool(&[MY_EXT]).unwrap();
//!  // Works with both Go protojson and Rust pbjson encoding.
//! let plan = parse_json(json_str, &pool).unwrap();
//! ```

use anyhow::Context;
use prost::Message;
use prost_reflect::{DescriptorPool, DynamicMessage};
use prost_types::FileDescriptorSet;
use substrait::proto::Plan;

/// Build a [`DescriptorPool`] covering the Substrait core schema plus any extra
/// descriptor passed in.
pub fn build_descriptor_pool(extra_descriptors: &[&[u8]]) -> anyhow::Result<DescriptorPool> {
    let mut fds = FileDescriptorSet::decode(substrait::proto::FILE_DESCRIPTOR_SET)
        .context("failed to decode substrait core descriptor")?;

    // Descriptor blobs compiled from proto files bundle their transitive, 
    // therefore custom descriptors are likely to have repeat file names 
    // such as: google/protobuf/timestamp.proto, google/protobuf/any.proto,
    // which are also present in substrait core protos. 
    // DescriptorPool::decode treats duplicate filenames as a hard error.
    // Track filenames already in the set so we can skip duplicates.
    let mut seen: std::collections::HashSet<String> =
        fds.file.iter().map(|f| f.name().to_owned()).collect();

    for blob in extra_descriptors {
        let extra =
            FileDescriptorSet::decode(*blob).context("failed to decode extra descriptor")?;
        for f in extra.file {
            if seen.insert(f.name().to_owned()) {
                fds.file.push(f);
            }
        }
    }

    DescriptorPool::decode(fds.encode_to_vec().as_slice())
        .context("failed to build descriptor pool")
}

/// Parse a protobuf JSON string into a [`Plan`], handling both JSON encodings:
/// - **Go `protojson`**: `{"@type": "type.googleapis.com/pkg.Msg", "field": value, ...}`
/// - **Rust `pbjson`**: `{"typeUrl": "type.googleapis.com/pkg.Msg", "value": "<base64>"}`
pub fn parse_json(json: &str, pool: &DescriptorPool) -> anyhow::Result<Plan> {
    let plan_desc = pool
        .get_message_by_name("substrait.Plan")
        .context("substrait.Plan not found in descriptor pool")?;

    let dyn_msg =
        DynamicMessage::deserialize(plan_desc, &mut serde_json::Deserializer::from_str(json))
            .context("failed to parse JSON as substrait.Plan")?;

    Plan::decode(dyn_msg.encode_to_vec().as_slice())
        .context("failed to decode Plan from dynamic message bytes")
}
