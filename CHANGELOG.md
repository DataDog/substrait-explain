# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0](https://github.com/DataDog/substrait-explain/compare/v0.2.0...v0.3.0) - 2026-03-12

### Bug Fixes

- resolve merge conflicts between cli-fail-on-failure and main
- *(extensions)* check type URL conflicts before mutating registry state
- add empty args support and use proper string unescaping
- remove dead code and fix documentation issues
- replace expression panics with typed variants and escape string literals
- fixing bug in parsing of if_clause
- fixing missed labelling

### Documentation

- note TODO for conventional commit CI enforcement
- document namespace registry for extension types
- document extension system and registry

### Features

- *(extensions)* namespace registry for relation/enh/opt
- simplify extension arg parsing with ArgsExtractor
- implement extension system with registry and Any encoding

### Refactoring

- *(extensions)* redesign ExtensionArgs with IndexMap and public fields
- *(extensions)* separate error types and make register() fallible
- simplify extension traits and improve examples
- centralize parser context and enforce hard errors
- improve extension parsing and textify
- move fixture methods to dedicated module

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
