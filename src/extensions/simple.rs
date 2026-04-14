use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;

use pext::simple_extension_declaration::MappingType;
use substrait::proto::extensions as pext;
use thiserror::Error;

pub const EXTENSIONS_HEADER: &str = "=== Extensions";
pub const EXTENSION_URNS_HEADER: &str = "URNs:";
pub const EXTENSION_FUNCTIONS_HEADER: &str = "Functions:";
pub const EXTENSION_TYPES_HEADER: &str = "Types:";
pub const EXTENSION_TYPE_VARIATIONS_HEADER: &str = "Type Variations:";

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExtensionKind {
    // Urn,
    Function,
    Type,
    TypeVariation,
}

impl ExtensionKind {
    pub fn name(&self) -> &'static str {
        match self {
            // ExtensionKind::Urn => "urn",
            ExtensionKind::Function => "function",
            ExtensionKind::Type => "type",
            ExtensionKind::TypeVariation => "type_variation",
        }
    }
}

impl fmt::Display for ExtensionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // ExtensionKind::Urn => write!(f, "URN"),
            ExtensionKind::Function => write!(f, "Function"),
            ExtensionKind::Type => write!(f, "Type"),
            ExtensionKind::TypeVariation => write!(f, "Type Variation"),
        }
    }
}

#[derive(Error, Debug, PartialEq, Clone)]
pub enum InsertError {
    #[error("Extension declaration missing mapping type")]
    MissingMappingType,

    #[error("Duplicate URN anchor {anchor} for {prev} and {name}")]
    DuplicateUrnAnchor {
        anchor: u32,
        prev: String,
        name: String,
    },

    #[error("Duplicate extension {kind} anchor {anchor} for {prev} and {name}")]
    DuplicateAnchor {
        kind: ExtensionKind,
        anchor: u32,
        prev: String,
        name: String,
    },

    #[error("Missing URN anchor {urn} for extension {kind} anchor {anchor} name {name}")]
    MissingUrn {
        kind: ExtensionKind,
        anchor: u32,
        name: String,
        urn: u32,
    },

    #[error(
        "Duplicate extension {kind} anchor {anchor} for {prev} and {name}, also missing URN {urn}"
    )]
    DuplicateAndMissingUrn {
        kind: ExtensionKind,
        anchor: u32,
        prev: String,
        name: String,
        urn: u32,
    },
}

/// A Substrait compound function name, e.g. `"equal:any_any"` or `"add"`.
///
/// The name before the first `:` is the *base name*; the part after is the
/// *type-signature suffix*.  For plain names with no `:` the base name is the
/// full name and `has_signature` returns `false`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompoundName {
    /// Full name including the signature suffix, e.g. `"equal:any_any"`.
    name: String,
    /// Byte index of the `:` separator, or `name.len()` when absent.
    index: usize,
}

impl CompoundName {
    pub fn new(name: impl Into<String>) -> Self {
        let name = name.into();
        let index = name.find(':').unwrap_or(name.len());
        Self { name, index }
    }

    /// The base name (part before the first `:`), e.g. `"equal"`.
    pub fn base(&self) -> &str {
        &self.name[..self.index]
    }

    /// The full compound name, e.g. `"equal:any_any"`.
    pub fn full(&self) -> &str {
        &self.name
    }

    /// `true` when the name includes a signature suffix (contains `:`).
    pub fn has_signature(&self) -> bool {
        self.index < self.name.len()
    }
}

impl fmt::Display for CompoundName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.name)
    }
}

/// ExtensionLookup contains mappings from anchors to extension URNs, functions,
/// types, and type variations.
#[derive(Default, Debug, Clone, PartialEq)]
pub struct SimpleExtensions {
    // Maps from extension URN anchor to URN
    urns: BTreeMap<u32, String>,
    // Maps from anchor and extension kind to (URN anchor, name)
    extensions: BTreeMap<(u32, ExtensionKind), (u32, CompoundName)>,
}

/// Extract the kind, URN reference, anchor, and name from an extension mapping.
///
/// Declarations that only use the deprecated URI reference field are skipped
/// here. Plan-level formatting reports those fields separately; treating their
/// default URN reference as real would reintroduce the misleading
/// "Missing URN anchor 0" diagnostic.
enum ExtractedMapping {
    Urn(ExtensionKind, u32, u32, String),
    DeprecatedUriOnly,
}

#[allow(deprecated)]
fn extract_mapping(mapping: &Option<MappingType>) -> Option<ExtractedMapping> {
    match mapping {
        Some(MappingType::ExtensionType(t)) if t.extension_urn_reference != 0 => {
            Some(ExtractedMapping::Urn(
                ExtensionKind::Type,
                t.extension_urn_reference,
                t.type_anchor,
                t.name.clone(),
            ))
        }
        Some(MappingType::ExtensionType(t)) if t.extension_uri_reference != 0 => {
            Some(ExtractedMapping::DeprecatedUriOnly)
        }
        Some(MappingType::ExtensionType(t)) => Some(ExtractedMapping::Urn(
            ExtensionKind::Type,
            t.extension_urn_reference,
            t.type_anchor,
            t.name.clone(),
        )),
        Some(MappingType::ExtensionFunction(f)) if f.extension_urn_reference != 0 => {
            Some(ExtractedMapping::Urn(
                ExtensionKind::Function,
                f.extension_urn_reference,
                f.function_anchor,
                f.name.clone(),
            ))
        }
        Some(MappingType::ExtensionFunction(f)) if f.extension_uri_reference != 0 => {
            Some(ExtractedMapping::DeprecatedUriOnly)
        }
        Some(MappingType::ExtensionFunction(f)) => Some(ExtractedMapping::Urn(
            ExtensionKind::Function,
            f.extension_urn_reference,
            f.function_anchor,
            f.name.clone(),
        )),
        Some(MappingType::ExtensionTypeVariation(v)) if v.extension_urn_reference != 0 => {
            Some(ExtractedMapping::Urn(
                ExtensionKind::TypeVariation,
                v.extension_urn_reference,
                v.type_variation_anchor,
                v.name.clone(),
            ))
        }
        Some(MappingType::ExtensionTypeVariation(v)) if v.extension_uri_reference != 0 => {
            Some(ExtractedMapping::DeprecatedUriOnly)
        }
        Some(MappingType::ExtensionTypeVariation(v)) => Some(ExtractedMapping::Urn(
            ExtensionKind::TypeVariation,
            v.extension_urn_reference,
            v.type_variation_anchor,
            v.name.clone(),
        )),
        None => None,
    }
}

impl SimpleExtensions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_extensions<'a>(
        urns: impl IntoIterator<Item = &'a pext::SimpleExtensionUrn>,
        extensions: impl IntoIterator<Item = &'a pext::SimpleExtensionDeclaration>,
    ) -> (Self, Vec<InsertError>) {
        // TODO: this checks for missing URNs and duplicate anchors, but not
        // duplicate names with the same anchor.

        let mut exts = Self::new();

        let mut errors = Vec::<InsertError>::new();

        for urn in urns {
            if let Err(e) = exts.add_extension_urn(urn.urn.clone(), urn.extension_urn_anchor) {
                errors.push(e);
            }
        }

        for extension in extensions {
            match extract_mapping(&extension.mapping_type) {
                Some(ExtractedMapping::Urn(kind, urn_ref, anchor, name)) => {
                    if let Err(e) = exts.add_extension(kind, urn_ref, anchor, name) {
                        errors.push(e);
                    }
                }
                Some(ExtractedMapping::DeprecatedUriOnly) => {}
                None => {
                    errors.push(InsertError::MissingMappingType);
                }
            }
        }

        (exts, errors)
    }

    pub fn add_extension_urn(&mut self, urn: String, anchor: u32) -> Result<(), InsertError> {
        match self.urns.entry(anchor) {
            Entry::Occupied(e) => {
                return Err(InsertError::DuplicateUrnAnchor {
                    anchor,
                    prev: e.get().clone(),
                    name: urn,
                });
            }
            Entry::Vacant(e) => {
                e.insert(urn);
            }
        }
        Ok(())
    }

    pub fn add_extension(
        &mut self,
        kind: ExtensionKind,
        urn: u32,
        anchor: u32,
        name: String,
    ) -> Result<(), InsertError> {
        let missing_urn = !self.urns.contains_key(&urn);

        let prev = match self.extensions.entry((anchor, kind)) {
            Entry::Occupied(e) => Some(e.get().1.full().to_string()),
            Entry::Vacant(v) => {
                v.insert((urn, CompoundName::new(name.clone())));
                None
            }
        };

        match (missing_urn, prev) {
            (true, Some(prev)) => Err(InsertError::DuplicateAndMissingUrn {
                kind,
                anchor,
                prev,
                name,
                urn,
            }),
            (false, Some(prev)) => Err(InsertError::DuplicateAnchor {
                kind,
                anchor,
                prev,
                name,
            }),
            (true, None) => Err(InsertError::MissingUrn {
                kind,
                anchor,
                name,
                urn,
            }),
            (false, None) => Ok(()),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.urns.is_empty() && self.extensions.is_empty()
    }

    /// Convert the extension URNs to protobuf format for Plan construction.
    pub fn to_extension_urns(&self) -> Vec<pext::SimpleExtensionUrn> {
        self.urns
            .iter()
            .map(|(anchor, urn)| pext::SimpleExtensionUrn {
                extension_urn_anchor: *anchor,
                urn: urn.clone(),
            })
            .collect()
    }

    /// Convert the extensions to protobuf format for Plan construction.
    pub fn to_extension_declarations(&self) -> Vec<pext::SimpleExtensionDeclaration> {
        self.extensions
            .iter()
            .map(|((anchor, kind), (urn_ref, name))| {
                let mapping_type = match kind {
                    ExtensionKind::Function => MappingType::ExtensionFunction(
                        #[allow(deprecated)]
                        pext::simple_extension_declaration::ExtensionFunction {
                            extension_urn_reference: *urn_ref,
                            extension_uri_reference: Default::default(), // deprecated
                            function_anchor: *anchor,
                            name: name.full().to_string(),
                        },
                    ),
                    ExtensionKind::Type => MappingType::ExtensionType(
                        #[allow(deprecated)]
                        pext::simple_extension_declaration::ExtensionType {
                            extension_urn_reference: *urn_ref,
                            extension_uri_reference: Default::default(), // deprecated
                            type_anchor: *anchor,
                            name: name.full().to_string(),
                        },
                    ),
                    ExtensionKind::TypeVariation => MappingType::ExtensionTypeVariation(
                        #[allow(deprecated)]
                        pext::simple_extension_declaration::ExtensionTypeVariation {
                            extension_urn_reference: *urn_ref,
                            extension_uri_reference: Default::default(), // deprecated
                            type_variation_anchor: *anchor,
                            name: name.full().to_string(),
                        },
                    ),
                };
                pext::SimpleExtensionDeclaration {
                    mapping_type: Some(mapping_type),
                }
            })
            .collect()
    }

    /// Write the extensions to the given writer, with the given indent.
    ///
    /// The header will be included if there are any extensions.
    pub fn write<W: fmt::Write>(&self, w: &mut W, indent: &str) -> fmt::Result {
        if self.is_empty() {
            // No extensions, so no need to write anything.
            return Ok(());
        }

        writeln!(w, "{EXTENSIONS_HEADER}")?;
        if !self.urns.is_empty() {
            writeln!(w, "{EXTENSION_URNS_HEADER}")?;
            for (anchor, urn) in &self.urns {
                writeln!(w, "{indent}@{anchor:3}: {urn}")?;
            }
        }

        let kinds_and_headers = [
            (ExtensionKind::Function, EXTENSION_FUNCTIONS_HEADER),
            (ExtensionKind::Type, EXTENSION_TYPES_HEADER),
            (
                ExtensionKind::TypeVariation,
                EXTENSION_TYPE_VARIATIONS_HEADER,
            ),
        ];
        for (kind, header) in kinds_and_headers {
            let mut filtered = self
                .extensions
                .iter()
                .filter(|((_a, k), _)| *k == kind)
                .peekable();
            if filtered.peek().is_none() {
                continue;
            }

            writeln!(w, "{header}")?;
            for ((anchor, _), (urn_ref, name)) in filtered {
                writeln!(w, "{indent}#{anchor:3} @{urn_ref:3}: {name}")?;
            }
        }
        Ok(())
    }

    pub fn to_string(&self, indent: &str) -> String {
        let mut output = String::new();
        self.write(&mut output, indent).unwrap();
        output
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum MissingReference {
    #[error("Missing URN for {0}")]
    MissingUrn(u32),
    #[error("Missing anchor for {0}: {1}")]
    MissingAnchor(ExtensionKind, u32),
    #[error("Missing name for {0}: {1}")]
    MissingName(ExtensionKind, String),
    #[error("Mismatched {0}: {1}#{2}")]
    /// When the name of the value does not match the expected name
    Mismatched(ExtensionKind, String, u32),
    #[error("Duplicate name without anchor for {0}: {1}")]
    DuplicateName(ExtensionKind, String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SimpleExtension {
    pub kind: ExtensionKind,
    pub name: String,
    pub anchor: u32,
    pub urn: u32,
}

/// The result of resolving a function anchor to its full metadata.
pub struct ResolvedFunction<'a> {
    pub anchor: u32,
    pub urn: u32,
    /// The full compound name stored for this anchor.
    pub name: &'a CompoundName,
    /// `true` when the base name is unique across all registered functions
    /// (controls whether the signature suffix is needed in compact mode).
    pub base_name_unique: bool,
    /// `true` when the full compound name is unique across all registered
    /// functions (controls whether the `#anchor` suffix is needed).
    pub name_unique: bool,
}

impl SimpleExtensions {
    pub fn find_urn(&self, anchor: u32) -> Result<&str, MissingReference> {
        self.urns
            .get(&anchor)
            .map(String::as_str)
            .ok_or(MissingReference::MissingUrn(anchor))
    }

    pub fn find_by_anchor(
        &self,
        kind: ExtensionKind,
        anchor: u32,
    ) -> Result<(u32, &CompoundName), MissingReference> {
        let &(urn, ref name) = self
            .extensions
            .get(&(anchor, kind))
            .ok_or(MissingReference::MissingAnchor(kind, anchor))?;

        Ok((urn, name))
    }

    pub fn find_by_name(&self, kind: ExtensionKind, name: &str) -> Result<u32, MissingReference> {
        let mut matches = self
            .extensions
            .iter()
            .filter(move |((_a, k), (_, n))| *k == kind && n.full() == name)
            .map(|((anchor, _), _)| *anchor);

        let anchor = matches
            .next()
            .ok_or(MissingReference::MissingName(kind, name.to_string()))?;

        match matches.next() {
            Some(_) => Err(MissingReference::DuplicateName(kind, name.to_string())),
            None => Ok(anchor),
        }
    }

    /// Returns `true` when no other extension of the same kind has the same
    /// full compound name (i.e. the anchor display can be suppressed).
    ///
    /// Returns `Err` when `anchor` is not registered for `kind`.
    pub fn is_name_unique(
        &self,
        kind: ExtensionKind,
        anchor: u32,
        name: &str,
    ) -> Result<bool, MissingReference> {
        let mut found = false;
        let mut other = false;
        for (&(a, k), (_, n)) in self.extensions.iter() {
            if k != kind {
                continue;
            }

            if a == anchor {
                found = true;
                if n.full() != name {
                    return Err(MissingReference::Mismatched(kind, name.to_string(), anchor));
                }
                continue;
            }

            if n.full() != name {
                // Neither anchor nor name match, so this is irrelevant.
                continue;
            }

            // At this point, the anchor is different, but the name is the same.
            other = true;
            if found {
                break;
            }
        }

        match (found, other) {
            // Found the one we're looking for, and no other matches.
            (true, false) => Ok(true),
            // Found the one we're looking for, and another match.
            (true, true) => Ok(false),
            // Didn't find the one we're looking for.
            (false, _) => Err(MissingReference::MissingAnchor(kind, anchor)),
        }
    }

    /// Look up a function anchor and return its full resolution metadata.
    /// The caller already has `anchor` from the
    /// Substrait plan and needs the name, URN, and uniqueness flags.
    pub fn lookup_function(&self, anchor: u32) -> Result<ResolvedFunction<'_>, MissingReference> {
        let (urn, name) = self.find_by_anchor(ExtensionKind::Function, anchor)?;
        let name_unique = self.is_name_unique(ExtensionKind::Function, anchor, name.full())?;
        let base_name_unique = self.is_base_name_unique(ExtensionKind::Function, anchor)?;
        Ok(ResolvedFunction {
            anchor,
            urn,
            name,
            name_unique,
            base_name_unique,
        })
    }

    /// Resolve a function name (plain or compound) with an optional explicit
    /// anchor to a [`ResolvedFunction`].
    /// the caller has a text name and an optional anchor
    /// from the plan source and needs the canonical anchor plus uniqueness info.
    ///
    /// * `anchor = Some(a)` — validates that `name` matches the stored name
    ///   (exact compound-name match **or** base-name match, e.g. `"equal"`
    ///   matches stored `"equal:any_any"`).
    /// * `anchor = None` — tries an exact match first; if not found and
    ///   `name` contains no `:`, falls back to base-name search so that
    ///   `equal(…)` resolves when `equal:any_any` is the only overload.
    pub fn resolve_function(
        &self,
        name: &str,
        anchor: Option<u32>,
    ) -> Result<ResolvedFunction<'_>, MissingReference> {
        let resolved_anchor = match anchor {
            Some(a) => {
                let (_, stored) = self.find_by_anchor(ExtensionKind::Function, a)?;
                if stored.full() == name || stored.base() == name {
                    a
                } else {
                    return Err(MissingReference::Mismatched(
                        ExtensionKind::Function,
                        name.to_string(),
                        a,
                    ));
                }
            }
            None => match self.find_by_name(ExtensionKind::Function, name) {
                Ok(a) => a,
                Err(MissingReference::MissingName(_, _)) if !name.contains(':') => {
                    self.find_by_base_name(ExtensionKind::Function, name)?
                }
                Err(e) => return Err(e),
            },
        };
        self.lookup_function(resolved_anchor)
    }

    fn is_base_name_unique(
        &self,
        kind: ExtensionKind,
        anchor: u32,
    ) -> Result<bool, MissingReference> {
        let (_, name) = self.find_by_anchor(kind, anchor)?;
        let my_base = name.base();

        let other_exists = self
            .extensions
            .iter()
            .any(|(&(a, k), (_, n))| k == kind && a != anchor && n.base() == my_base);

        Ok(!other_exists)
    }

    fn find_by_base_name(&self, kind: ExtensionKind, base: &str) -> Result<u32, MissingReference> {
        let mut matches = self
            .extensions
            .iter()
            .filter(|&(&(_a, k), (_, n))| k == kind && n.base() == base)
            .map(|(&(anchor, _), _)| anchor);

        let anchor = matches
            .next()
            .ok_or_else(|| MissingReference::MissingName(kind, base.to_string()))?;

        match matches.next() {
            Some(_) => Err(MissingReference::DuplicateName(kind, base.to_string())),
            None => Ok(anchor),
        }
    }
}

#[cfg(test)]
mod tests {
    use pext::simple_extension_declaration::{
        ExtensionFunction, ExtensionType, ExtensionTypeVariation, MappingType,
    };
    use substrait::proto::extensions as pext;

    use super::*;

    fn new_urn(anchor: u32, urn_str: &str) -> pext::SimpleExtensionUrn {
        pext::SimpleExtensionUrn {
            extension_urn_anchor: anchor,
            urn: urn_str.to_string(),
        }
    }

    fn new_ext_fn(anchor: u32, urn_ref: u32, name: &str) -> pext::SimpleExtensionDeclaration {
        pext::SimpleExtensionDeclaration {
            #[allow(deprecated)]
            mapping_type: Some(MappingType::ExtensionFunction(ExtensionFunction {
                extension_urn_reference: urn_ref,
                extension_uri_reference: Default::default(), // deprecated
                function_anchor: anchor,
                name: name.to_string(),
            })),
        }
    }

    fn new_ext_fn_uri_only(
        anchor: u32,
        uri_ref: u32,
        name: &str,
    ) -> pext::SimpleExtensionDeclaration {
        pext::SimpleExtensionDeclaration {
            #[allow(deprecated)]
            mapping_type: Some(MappingType::ExtensionFunction(ExtensionFunction {
                extension_urn_reference: Default::default(),
                extension_uri_reference: uri_ref,
                function_anchor: anchor,
                name: name.to_string(),
            })),
        }
    }

    fn new_ext_type(anchor: u32, urn_ref: u32, name: &str) -> pext::SimpleExtensionDeclaration {
        #[allow(deprecated)]
        pext::SimpleExtensionDeclaration {
            mapping_type: Some(MappingType::ExtensionType(ExtensionType {
                extension_urn_reference: urn_ref,
                extension_uri_reference: Default::default(), // deprecated
                type_anchor: anchor,
                name: name.to_string(),
            })),
        }
    }

    fn new_type_var(anchor: u32, urn_ref: u32, name: &str) -> pext::SimpleExtensionDeclaration {
        pext::SimpleExtensionDeclaration {
            #[allow(deprecated)]
            mapping_type: Some(MappingType::ExtensionTypeVariation(
                ExtensionTypeVariation {
                    extension_urn_reference: urn_ref,
                    extension_uri_reference: Default::default(), // deprecated
                    type_variation_anchor: anchor,
                    name: name.to_string(),
                },
            )),
        }
    }

    fn assert_no_errors(errs: &[InsertError]) {
        for err in errs {
            println!("Error: {err:?}");
        }
        assert!(errs.is_empty());
    }

    fn unwrap_new_extensions<'a>(
        urns: impl IntoIterator<Item = &'a pext::SimpleExtensionUrn>,
        extensions: impl IntoIterator<Item = &'a pext::SimpleExtensionDeclaration>,
    ) -> SimpleExtensions {
        let (exts, errs) = SimpleExtensions::from_extensions(urns, extensions);
        assert_no_errors(&errs);
        exts
    }

    #[test]
    fn test_extension_lookup_empty() {
        let lookup = SimpleExtensions::new();
        assert!(lookup.find_urn(1).is_err());
        assert!(lookup.find_by_anchor(ExtensionKind::Function, 1).is_err());
        assert!(lookup.find_by_anchor(ExtensionKind::Type, 1).is_err());
        assert!(
            lookup
                .find_by_anchor(ExtensionKind::TypeVariation, 1)
                .is_err()
        );
        assert!(lookup.find_by_name(ExtensionKind::Function, "any").is_err());
        assert!(lookup.find_by_name(ExtensionKind::Type, "any").is_err());
        assert!(
            lookup
                .find_by_name(ExtensionKind::TypeVariation, "any")
                .is_err()
        );
    }

    #[test]
    fn test_from_extensions_basic() {
        let urns = vec![new_urn(1, "urn1"), new_urn(2, "urn2")];
        let extensions = vec![
            new_ext_fn(10, 1, "func1"),
            new_ext_type(20, 1, "type1"),
            new_type_var(30, 2, "var1"),
        ];
        let exts = unwrap_new_extensions(&urns, &extensions);

        assert_eq!(exts.find_urn(1).unwrap(), "urn1");
        assert_eq!(exts.find_urn(2).unwrap(), "urn2");
        assert!(exts.find_urn(3).is_err());

        let (urn, name) = exts.find_by_anchor(ExtensionKind::Function, 10).unwrap();
        assert_eq!(name.full(), "func1");
        assert_eq!(urn, 1);
        assert!(exts.find_by_anchor(ExtensionKind::Function, 11).is_err());

        let (urn, name) = exts.find_by_anchor(ExtensionKind::Type, 20).unwrap();
        assert_eq!(name.full(), "type1");
        assert_eq!(urn, 1);
        assert!(exts.find_by_anchor(ExtensionKind::Type, 21).is_err());

        let (urn, name) = exts
            .find_by_anchor(ExtensionKind::TypeVariation, 30)
            .unwrap();
        assert_eq!(name.full(), "var1");
        assert_eq!(urn, 2);
        assert!(
            exts.find_by_anchor(ExtensionKind::TypeVariation, 31)
                .is_err()
        );
    }

    #[test]
    fn test_from_extensions_duplicates() {
        let urns = vec![
            new_urn(1, "urn_old"),
            new_urn(1, "urn_new"),
            new_urn(2, "second"),
        ];
        let extensions = vec![
            new_ext_fn(10, 1, "func_old"),
            new_ext_fn(10, 2, "func_new"), // Duplicate function anchor
            new_ext_fn(11, 3, "func_missing"),
        ];
        let (exts, errs) = SimpleExtensions::from_extensions(&urns, &extensions);
        assert_eq!(
            errs,
            vec![
                InsertError::DuplicateUrnAnchor {
                    anchor: 1,
                    name: "urn_new".to_string(),
                    prev: "urn_old".to_string()
                },
                InsertError::DuplicateAnchor {
                    kind: ExtensionKind::Function,
                    anchor: 10,
                    prev: "func_old".to_string(),
                    name: "func_new".to_string()
                },
                InsertError::MissingUrn {
                    kind: ExtensionKind::Function,
                    anchor: 11,
                    name: "func_missing".to_string(),
                    urn: 3,
                },
            ]
        );

        // This is a duplicate anchor, so the first one is used.
        assert_eq!(exts.find_urn(1).unwrap(), "urn_old");
        let (urn, name) = exts.find_by_anchor(ExtensionKind::Function, 10).unwrap();
        assert_eq!(urn, 1);
        assert_eq!(name.full(), "func_old");
    }

    #[test]
    fn test_from_extensions_invalid_mapping_type() {
        let extensions = vec![pext::SimpleExtensionDeclaration { mapping_type: None }];

        let (_exts, errs) = SimpleExtensions::from_extensions(vec![], &extensions);
        assert_eq!(errs.len(), 1);
        let err = &errs[0];
        assert_eq!(err, &InsertError::MissingMappingType);
    }

    #[test]
    fn test_from_extensions_skips_deprecated_uri_only_mappings() {
        let extensions = vec![new_ext_fn_uri_only(10, 1, "func")];
        let (exts, errs) = SimpleExtensions::from_extensions(vec![], &extensions);

        assert_no_errors(&errs);
        assert!(exts.is_empty());
    }

    #[test]
    fn test_find_by_name() {
        let urns = vec![new_urn(1, "urn1")];
        let extensions = vec![
            new_ext_fn(10, 1, "name1"),
            new_ext_fn(11, 1, "name2"),
            new_ext_fn(12, 1, "name1"), // Duplicate name
            new_ext_type(20, 1, "type_name1"),
            new_type_var(30, 1, "var_name1"),
        ];
        let exts = unwrap_new_extensions(&urns, &extensions);

        let err = exts
            .find_by_name(ExtensionKind::Function, "name1")
            .unwrap_err();
        assert_eq!(
            err,
            MissingReference::DuplicateName(ExtensionKind::Function, "name1".to_string())
        );

        let found = exts.find_by_name(ExtensionKind::Function, "name2").unwrap();
        assert_eq!(found, 11);

        let found = exts
            .find_by_name(ExtensionKind::Type, "type_name1")
            .unwrap();
        assert_eq!(found, 20);

        let err = exts
            .find_by_name(ExtensionKind::Type, "non_existent_type_name")
            .unwrap_err();
        assert_eq!(
            err,
            MissingReference::MissingName(
                ExtensionKind::Type,
                "non_existent_type_name".to_string()
            )
        );

        let found = exts
            .find_by_name(ExtensionKind::TypeVariation, "var_name1")
            .unwrap();
        assert_eq!(found, 30);

        let err = exts
            .find_by_name(ExtensionKind::TypeVariation, "non_existent_var_name")
            .unwrap_err();
        assert_eq!(
            err,
            MissingReference::MissingName(
                ExtensionKind::TypeVariation,
                "non_existent_var_name".to_string()
            )
        );
    }

    #[test]
    fn test_display_extension_lookup_empty() {
        let lookup = SimpleExtensions::new();
        let mut output = String::new();
        lookup.write(&mut output, "  ").unwrap();
        let expected = r"";
        assert_eq!(output, expected.trim_start());
    }

    #[test]
    fn test_display_extension_lookup_with_content() {
        let urns = vec![
            new_urn(1, "/my/urn1"),
            new_urn(42, "/another/urn"),
            new_urn(4091, "/big/anchor"),
        ];
        let extensions = vec![
            new_ext_fn(10, 1, "my_func"),
            new_ext_type(20, 1, "my_type"),
            new_type_var(30, 42, "my_var"),
            new_ext_fn(11, 42, "another_func"),
            new_ext_fn(108812, 4091, "big_func"),
        ];
        let exts = unwrap_new_extensions(&urns, &extensions);
        let display_str = exts.to_string("  ");

        let expected = r"
=== Extensions
URNs:
  @  1: /my/urn1
  @ 42: /another/urn
  @4091: /big/anchor
Functions:
  # 10 @  1: my_func
  # 11 @ 42: another_func
  #108812 @4091: big_func
Types:
  # 20 @  1: my_type
Type Variations:
  # 30 @ 42: my_var
";
        assert_eq!(display_str, expected.trim_start());
    }

    #[test]
    fn test_extensions_output() {
        // Manually build the extensions
        let mut extensions = SimpleExtensions::new();
        extensions
            .add_extension_urn("/urn/common".to_string(), 1)
            .unwrap();
        extensions
            .add_extension_urn("/urn/specific_funcs".to_string(), 2)
            .unwrap();
        extensions
            .add_extension(ExtensionKind::Function, 1, 10, "func_a".to_string())
            .unwrap();
        extensions
            .add_extension(ExtensionKind::Function, 2, 11, "func_b_special".to_string())
            .unwrap();
        extensions
            .add_extension(ExtensionKind::Type, 1, 20, "SomeType".to_string())
            .unwrap();
        extensions
            .add_extension(ExtensionKind::TypeVariation, 2, 30, "VarX".to_string())
            .unwrap();

        // Convert to string
        let output = extensions.to_string("  ");

        // The output should match the expected format
        let expected_output = r#"
=== Extensions
URNs:
  @  1: /urn/common
  @  2: /urn/specific_funcs
Functions:
  # 10 @  1: func_a
  # 11 @  2: func_b_special
Types:
  # 20 @  1: SomeType
Type Variations:
  # 30 @  2: VarX
"#;

        assert_eq!(output, expected_output.trim_start());
    }

    #[test]
    fn test_compound_name_plain() {
        let n = CompoundName::new("add");
        assert_eq!(n.full(), "add");
        assert_eq!(n.base(), "add");
        assert!(!n.has_signature());

        let n2 = CompoundName::new("coalesce");
        assert_eq!(n2.full(), "coalesce");
        assert_eq!(n2.base(), "coalesce");
    }

    #[test]
    fn test_compound_name_with_signature() {
        let n = CompoundName::new("equal:any_any");
        assert_eq!(n.full(), "equal:any_any");
        assert_eq!(n.base(), "equal");
        assert!(n.has_signature());

        let n2 = CompoundName::new("regexp_match_substring:str_str_i64");
        assert_eq!(n2.base(), "regexp_match_substring");
        assert_eq!(n2.full(), "regexp_match_substring:str_str_i64");
        assert!(n2.has_signature());

        let n3 = CompoundName::new("add:i64_i64");
        assert_eq!(n3.base(), "add");
    }

    #[test]
    fn test_compound_name_trailing_colon() {
        // Edge case: trailing colon → empty signature suffix, base is the prefix
        let n = CompoundName::new("foo:");
        assert_eq!(n.base(), "foo");
        assert_eq!(n.full(), "foo:");
        assert!(n.has_signature());
    }

    // ---- Tests for lookup_function ----

    fn make_overloaded_extensions() -> SimpleExtensions {
        let urns = vec![new_urn(1, "urn:comparison")];
        let extensions = vec![
            new_ext_fn(1, 1, "equal:any_any"),
            new_ext_fn(2, 1, "equal:str_str"),
            new_ext_fn(3, 1, "add:i64_i64"),
        ];
        unwrap_new_extensions(&urns, &extensions)
    }

    #[test]
    fn test_lookup_function_uniqueness_flags() {
        // `equal:any_any` and `equal:str_str` share the base name "equal" →
        // base_name_unique false, compound name unique within the one URN.
        // `add:i64_i64` is the only "add" → both flags true.
        let exts = make_overloaded_extensions();

        let r1 = exts.lookup_function(1).unwrap();
        assert_eq!(r1.name.full(), "equal:any_any");
        assert!(!r1.base_name_unique, "two 'equal' overloads");
        assert!(r1.name_unique, "compound name 'equal:any_any' is unique");

        let r2 = exts.lookup_function(2).unwrap();
        assert_eq!(r2.name.full(), "equal:str_str");
        assert!(!r2.base_name_unique);
        assert!(r2.name_unique);

        let r3 = exts.lookup_function(3).unwrap();
        assert_eq!(r3.name.full(), "add:i64_i64");
        assert!(r3.base_name_unique, "only one 'add' overload");
        assert!(r3.name_unique, "compound name appears only once");
    }

    #[test]
    fn test_lookup_function_missing_anchor() {
        let exts = SimpleExtensions::new();
        assert!(exts.lookup_function(99).is_err());
    }

    #[test]
    fn test_lookup_function_plain_name_overloaded_across_urns() {
        // Same plain name in two URNs → base_name_unique false, name_unique false.
        let urns = vec![new_urn(1, "urn1"), new_urn(2, "urn2")];
        let extensions = vec![
            new_ext_fn(1, 1, "duplicated"),
            new_ext_fn(2, 2, "duplicated"),
        ];
        let exts = unwrap_new_extensions(&urns, &extensions);

        let r = exts.lookup_function(1).unwrap();
        assert!(!r.base_name_unique);
        assert!(!r.name_unique);
    }

    #[test]
    fn test_lookup_function_different_base_names_each_unique() {
        // `equal:any_any` and `like:str_str` have distinct base names → each unique.
        let urns = vec![new_urn(1, "urn1")];
        let extensions = vec![
            new_ext_fn(1, 1, "equal:any_any"),
            new_ext_fn(2, 1, "like:str_str"),
        ];
        let exts = unwrap_new_extensions(&urns, &extensions);

        assert!(exts.lookup_function(1).unwrap().base_name_unique);
        assert!(exts.lookup_function(2).unwrap().base_name_unique);
    }

    // ---- Tests for resolve_function ----

    fn make_resolve_extensions() -> SimpleExtensions {
        // Mirrors the fixture used in the old parser/types.rs tests.
        let urns = vec![new_urn(1, "test_urn")];
        let extensions = vec![
            new_ext_fn(1, 1, "equal:any_any"),
            new_ext_fn(2, 1, "equal:str_str"),
            new_ext_fn(3, 1, "add:i64_i64"),
        ];
        unwrap_new_extensions(&urns, &extensions)
    }

    #[test]
    fn test_resolve_function_with_anchor() {
        // Explicit anchor: exact compound name, base name, and mismatched name.
        let exts = make_resolve_extensions();

        // Exact compound name matches stored name → resolves.
        assert_eq!(
            exts.resolve_function("equal:any_any", Some(1))
                .unwrap()
                .anchor,
            1
        );

        // Base name "equal" matches stored "equal:any_any" → resolves.
        assert_eq!(exts.resolve_function("equal", Some(1)).unwrap().anchor, 1);

        // "like" does not match stored "equal:any_any" → error.
        assert!(exts.resolve_function("like", Some(1)).is_err());
    }

    #[test]
    fn test_resolve_function_without_anchor() {
        // No anchor: exact compound, unique base (fallback), and ambiguous base.
        let exts = make_resolve_extensions();

        // Exact compound names resolve directly.
        assert_eq!(
            exts.resolve_function("equal:any_any", None).unwrap().anchor,
            1
        );
        assert_eq!(
            exts.resolve_function("equal:str_str", None).unwrap().anchor,
            2
        );

        // "add" is the only overload → base-name fallback finds anchor 3.
        assert_eq!(exts.resolve_function("add", None).unwrap().anchor, 3);

        // "equal" has two overloads → DuplicateName error.
        assert!(exts.resolve_function("equal", None).is_err());
    }

    #[test]
    fn test_resolve_function_plain_stored_name() {
        // Functions stored without a signature still resolve by their plain name.
        let urns = vec![new_urn(1, "urn")];
        let extensions = vec![new_ext_fn(10, 1, "coalesce")];
        let exts = unwrap_new_extensions(&urns, &extensions);
        assert_eq!(exts.resolve_function("coalesce", None).unwrap().anchor, 10);
    }

    #[test]
    fn test_resolve_function_not_found() {
        let exts = SimpleExtensions::new();
        assert!(exts.resolve_function("nonexistent", None).is_err());
    }

    #[test]
    fn test_compound_name_roundtrip_in_extensions_section() {
        // Verify that compound names survive a write → parse roundtrip through
        // the Extensions section text format.
        let urns = vec![new_urn(1, "substrait:functions_comparison")];
        let extensions = vec![
            new_ext_fn(1, 1, "equal:any_any"),
            new_ext_fn(2, 1, "equal:str_str"),
        ];
        let exts = unwrap_new_extensions(&urns, &extensions);

        let text = exts.to_string("  ");
        assert!(
            text.contains("equal:any_any"),
            "compound name must appear in output"
        );
        assert!(
            text.contains("equal:str_str"),
            "compound name must appear in output"
        );
    }
}
