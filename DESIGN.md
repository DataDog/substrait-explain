# Design Philosophy

`substrait-explain` is a human-facing text format and toolchain for Substrait
plans. It should make generated plans easy to inspect, discuss, test, and
round-trip. It prioritizes both being human-readable in its syntax, with its
semantics closely tied to Substrait and its protobuf format.

For the detailed syntax principles of the text format itself, see
[`GRAMMAR.md`](GRAMMAR.md).

## What This Project Is For

Substrait protobuf plans are precise and portable, but they are verbose and hard
to read directly. `substrait-explain` exists to give those plans a compact,
stable text representation that is useful for:

- debugging generated query plans;
- writing readable examples, fixtures, and tests;
- converting between text, JSON, YAML, and binary protobuf forms;
- preserving enough Substrait structure to parse the text back into protobuf;
- discussing plan structure without expanding deeply nested protobuf fields.

The text format is not SQL, an optimizer IR, or a semantic validator for all of
Substrait. It is a representation of Substrait plan structure.

## Core Principles

### Human Audience, Protobuf Semantics

The primary reader is a human trying to understand a plan. The primary data
model is still Substrait protobuf. Text should be concise and regular, but
format choices should map back to concrete Substrait fields. The _syntax_ of
`substrait-explain` is intended for humans; the _semantics_ should match
Substrait.

### Bidirectional

Supported Substrait features should exist in both the parser and formatter, with
identical syntax and semantics. To support this, the canonical testing method is
a round-trip test:

1. parse text into protobuf;
2. format protobuf back to text;
3. compare the canonical text output.

This demonstrates that the syntax and semantics are equivalent between the two,
and as the protobuf is in between, also indirectly demonstrates that the feature
is mapping to and from the protobuf format. Note that arbitrary or newer
protobufs may contain unsupported fields, old encodings, or custom payloads that
need best-effort output.

### Lenient Conversion, Explicit Warnings

Parsing and formatting should accept representable plans even when they are not
fully valid Substrait. If the text or protobuf has a clear mapping into the
supported `substrait-explain` model, conversion should usually preserve it rather
than reject it for semantic issues such as invalid URNs, type mismatches, or
function arity problems.

Invalidity should be reported as diagnostics or warnings where possible. That is
partly implemented and partly aspirational today: formatting already has an
error channel for degraded output, but parsing does not yet have a warning
channel, and output does not perform full validation or type checking. The
design goal is to keep conversion and validation separate so users can still
inspect, debug, and round-trip plans that are good enough to represent.

Parsing should still reject input that is structurally ambiguous, missing fields
needed to build the protobuf shape, or outside the supported text-format contract.
Formatting should make unsupported or malformed protobuf structure visible rather
than panicking or silently dropping the problem.

### Neutral Extension Support

Substrait extensions are intentionally open-ended, and `substrait-explain` should
stay neutral rather than baking in one organization's extension catalog or
semantics. The project should provide broad syntax, data structures, and hooks
that let extension users represent their plans without requiring those extensions
to live in this repository.

Simple extensions are supported as first-class text-format concepts via the
`=== Extensions` section. Consistent with lenient conversion, `substrait-explain`
does not need to validate those declarations against external YAML extension
files in order to preserve them.

Advanced extensions carry `google.protobuf.Any` payloads and require external
serialization/deserialization, and so are supported via a registry system,
allowing users to extend `substrait-explain` to get full support for their
advanced extensions. For extensions without handlers, the formatter should still
preserve the surrounding plan and report the unsupported payload clearly.

### Regular, Readable Syntax Over Protobuf Verbosity

The text format should expose the important plan shape directly: relation names,
arguments, output columns, expressions, types, extensions, and hierarchy. It
should avoid mirroring every protobuf nesting level when a regular text form can
represent the same structure more clearly.

### Boundaries Should Be Documented

Unsupported Substrait features are expected while the project grows. The docs
should state current boundaries clearly and point users to the grammar or API
surface that is actually implemented. Avoid broad support claims that are true
only for part of the library.

## Changing The Format

Format changes should preserve the project goals above:

- start from the upstream Substrait spec or protobuf definition;
- describe text-format choices as representation choices, not Substrait changes;
- update `GRAMMAR.md`, the Pest grammar, parser, textifier, and round-trip tests
  together when syntax changes;
- prefer canonical output that is easy to diff and review;
- keep conversion lenient where the plan is representable, while surfacing
  invalidity through diagnostics where the current APIs support it;
- document any compatibility impact in the PR.

`GRAMMAR.md` is the user-facing text-format contract. This file explains the
larger design posture that humans and agents should use before changing that
contract.
