# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2025-12-01

### Features

- Extension system with registry, namespace support, and `Any` protobuf encoding
- `ExtensionLeaf`, `ExtensionSingle`, and `ExtensionMulti` relation types
- IfThen conditional expression support
- Date/time literal textifier support
- Float and boolean literal parsing
- CLI format auto-detection from file extensions

### Refactoring

- Redesigned `ExtensionArgs` with `IndexMap` and public fields
- Separated extension error types and made `register()` fallible
- Centralized parser context with enforced hard errors

## [0.1.0] - 2025-07-07

Initial release.

- Parse Substrait protobuf plans to human-readable text format
- Format human-readable text back to Substrait protobuf
- Support for Read, Project, Filter, Aggregate, Join, Sort, Fetch, and Root relations
- Extension system for function/type URNs and anchors
- CLI tool with `convert` and `validate` commands
- Pest-based PEG parser for the text format
- Roundtrip testing infrastructure

[0.2.0]: https://github.com/DataDog/substrait-explain/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/DataDog/substrait-explain/releases/tag/v0.1.0
