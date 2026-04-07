use std::path::PathBuf;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let proto = "tests/json_parsing/parquet_scan.proto";

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={proto}");

    let out_dir = PathBuf::from(std::env::var("OUT_DIR")?);

    // Compile the proto using protox — pure Rust, no system protoc required.
    let fds = protox::compile([proto], ["tests/json_parsing/"])?;

    // Write the FileDescriptorSet binary for use with build_descriptor_pool in tests.
    // Tests include this via include_bytes!(concat!(env!("OUT_DIR"), "/parquet_scan.bin")).
    use prost::Message;
    std::fs::write(out_dir.join("parquet_scan.bin"), fds.encode_to_vec())?;

    // Generate Rust types from the compiled descriptor. type_name_domains configures
    // impl prost::Name with the type.googleapis.com prefix so that type_url() returns
    // the correct value for google.protobuf.Any encoding and pool lookup.
    prost_build::Config::new()
        .enable_type_names()
        .type_name_domain(["."], "type.googleapis.com")
        .compile_fds(fds)?;

    Ok(())
}
