use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;

use pext::simple_extension_declaration::MappingType;
use substrait::proto::extensions as pext;
use thiserror::Error;

pub const EXTENSIONS_HEADER: &str = "=== Extensions";
pub const EXTENSION_URIS_HEADER: &str = "URIs:";
pub const EXTENSION_FUNCTIONS_HEADER: &str = "Functions:";
pub const EXTENSION_TYPES_HEADER: &str = "Types:";
pub const EXTENSION_TYPE_VARIATIONS_HEADER: &str = "Type Variations:";

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExtensionKind {
    // Uri,
    Function,
    Type,
    TypeVariation,
}

impl ExtensionKind {
    pub fn name(&self) -> &'static str {
        match self {
            // ExtensionKind::Uri => "uri",
            ExtensionKind::Function => "function",
            ExtensionKind::Type => "type",
            ExtensionKind::TypeVariation => "type_variation",
        }
    }
}

impl fmt::Display for ExtensionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // ExtensionKind::Uri => write!(f, "URI"),
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

    #[error("Duplicate URI anchor {anchor} for {prev} and {name}")]
    DuplicateUriAnchor {
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

    #[error("Missing URI anchor {uri} for extension {kind} anchor {anchor} name {name}")]
    MissingUri {
        kind: ExtensionKind,
        anchor: u32,
        name: String,
        uri: u32,
    },

    #[error(
        "Duplicate extension {kind} anchor {anchor} for {prev} and {name}, also missing URI {uri}"
    )]
    DuplicateAndMissingUri {
        kind: ExtensionKind,
        anchor: u32,
        prev: String,
        name: String,
        uri: u32,
    },
}

/// ExtensionLookup contains mappings from anchors to extension URIs, functions,
/// types, and type variations.
#[derive(Default, Debug, Clone, PartialEq)]
pub struct SimpleExtensions {
    // Maps from extension URI anchor to URI
    uris: BTreeMap<u32, String>,
    // Maps from anchor and extension kind to (URI, name)
    extensions: BTreeMap<(u32, ExtensionKind), (u32, String)>,
}

impl SimpleExtensions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_extensions(
        uris: impl IntoIterator<Item = pext::SimpleExtensionUri>,
        extensions: impl IntoIterator<Item = pext::SimpleExtensionDeclaration>,
    ) -> (Self, Vec<InsertError>) {
        // TODO: this checks for missing URIs and duplicate anchors, but not
        // duplicate names with the same anchor.

        let mut exts = Self::new();

        let mut errors = Vec::<InsertError>::new();

        for uri in uris {
            if let Err(e) = exts.add_extension_uri(uri.uri, uri.extension_uri_anchor) {
                errors.push(e);
            }
        }

        for extension in extensions {
            match extension.mapping_type {
                Some(MappingType::ExtensionType(t)) => {
                    if let Err(e) = exts.add_extension(
                        ExtensionKind::Type,
                        t.extension_uri_reference,
                        t.type_anchor,
                        t.name,
                    ) {
                        errors.push(e);
                    }
                }
                Some(MappingType::ExtensionFunction(f)) => {
                    if let Err(e) = exts.add_extension(
                        ExtensionKind::Function,
                        f.extension_uri_reference,
                        f.function_anchor,
                        f.name,
                    ) {
                        errors.push(e);
                    }
                }
                Some(MappingType::ExtensionTypeVariation(v)) => {
                    if let Err(e) = exts.add_extension(
                        ExtensionKind::TypeVariation,
                        v.extension_uri_reference,
                        v.type_variation_anchor,
                        v.name,
                    ) {
                        errors.push(e);
                    }
                }
                None => {
                    errors.push(InsertError::MissingMappingType);
                }
            }
        }

        (exts, errors)
    }

    pub fn add_extension_uri(&mut self, uri: String, anchor: u32) -> Result<(), InsertError> {
        match self.uris.entry(anchor) {
            Entry::Occupied(e) => {
                return Err(InsertError::DuplicateUriAnchor {
                    anchor,
                    prev: e.get().clone(),
                    name: uri,
                });
            }
            Entry::Vacant(e) => {
                e.insert(uri);
            }
        }
        Ok(())
    }

    pub fn add_extension(
        &mut self,
        kind: ExtensionKind,
        uri: u32,
        anchor: u32,
        name: String,
    ) -> Result<(), InsertError> {
        let missing_uri = !self.uris.contains_key(&uri);

        let prev = match self.extensions.entry((anchor, kind)) {
            Entry::Occupied(e) => Some(e.get().1.clone()),
            Entry::Vacant(v) => {
                v.insert((uri, name.clone()));
                None
            }
        };

        match (missing_uri, prev) {
            (true, Some(prev)) => Err(InsertError::DuplicateAndMissingUri {
                kind,
                anchor,
                prev,
                name,
                uri,
            }),
            (false, Some(prev)) => Err(InsertError::DuplicateAnchor {
                kind,
                anchor,
                prev,
                name,
            }),
            (true, None) => Err(InsertError::MissingUri {
                kind,
                anchor,
                name,
                uri,
            }),
            (false, None) => Ok(()),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.uris.is_empty() && self.extensions.is_empty()
    }

    /// Convert the extension URIs to protobuf format for Plan construction.
    pub fn to_extension_uris(&self) -> Vec<pext::SimpleExtensionUri> {
        self.uris
            .iter()
            .map(|(anchor, uri)| pext::SimpleExtensionUri {
                extension_uri_anchor: *anchor,
                uri: uri.clone(),
            })
            .collect()
    }

    /// Convert the extensions to protobuf format for Plan construction.
    pub fn to_extension_declarations(&self) -> Vec<pext::SimpleExtensionDeclaration> {
        self.extensions
            .iter()
            .map(|((anchor, kind), (uri_ref, name))| {
                let mapping_type = match kind {
                    ExtensionKind::Function => MappingType::ExtensionFunction(
                        pext::simple_extension_declaration::ExtensionFunction {
                            extension_uri_reference: *uri_ref,
                            function_anchor: *anchor,
                            name: name.clone(),
                        },
                    ),
                    ExtensionKind::Type => MappingType::ExtensionType(
                        pext::simple_extension_declaration::ExtensionType {
                            extension_uri_reference: *uri_ref,
                            type_anchor: *anchor,
                            name: name.clone(),
                        },
                    ),
                    ExtensionKind::TypeVariation => MappingType::ExtensionTypeVariation(
                        pext::simple_extension_declaration::ExtensionTypeVariation {
                            extension_uri_reference: *uri_ref,
                            type_variation_anchor: *anchor,
                            name: name.clone(),
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

        // TODO: write the header. I think we can put this in the main block
        writeln!(w, "{EXTENSIONS_HEADER}")?;
        if !self.uris.is_empty() {
            writeln!(w, "{EXTENSION_URIS_HEADER}")?;
            for (anchor, uri) in &self.uris {
                writeln!(w, "{indent}@{anchor:3}: {uri}")?;
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
            for ((anchor, _), (uri_ref, name)) in filtered {
                writeln!(w, "{indent}#{anchor:3} @{uri_ref:3}: {name}")?;
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
    #[error("Missing URI for {0}")]
    MissingUri(u32),
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
    pub uri: u32,
}

impl SimpleExtensions {
    pub fn find_uri(&self, anchor: u32) -> Result<&str, MissingReference> {
        self.uris
            .get(&anchor)
            .map(String::as_str)
            .ok_or(MissingReference::MissingUri(anchor))
    }

    pub fn find_by_anchor(
        &self,
        kind: ExtensionKind,
        anchor: u32,
    ) -> Result<(u32, &str), MissingReference> {
        let &(uri, ref name) = self
            .extensions
            .get(&(anchor, kind))
            .ok_or(MissingReference::MissingAnchor(kind, anchor))?;

        Ok((uri, name))
    }

    pub fn find_by_name<'a>(
        &'a self,
        kind: ExtensionKind,
        name: &'a str,
    ) -> Result<u32, MissingReference> {
        let mut matches = self
            .extensions
            .iter()
            .filter(move |((_a, k), (_, n))| *k == kind && n.as_str() == name)
            .map(|((anchor, _), _)| *anchor);

        let anchor = matches
            .next()
            .ok_or(MissingReference::MissingName(kind, name.to_string()))?;

        match matches.next() {
            Some(_) => Err(MissingReference::DuplicateName(kind, name.to_string())),
            None => Ok(anchor),
        }
    }

    // Validate that the extension exists, has the given name and anchor, and
    // returns true if the name is unique for that extension kind.
    //
    // If the name is not unique, returns Ok(false). This is a valid case where
    // two extensions have the same name (and presumably different URIs), but
    // different anchors.
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
                if n != name {
                    return Err(MissingReference::Mismatched(kind, name.to_string(), anchor));
                }
                continue;
            }

            if n.as_str() != name {
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
            // Didn't find the one we're looking for;
            (false, _) => Err(MissingReference::MissingAnchor(kind, anchor)),
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

    fn new_uri(anchor: u32, uri_str: &str) -> pext::SimpleExtensionUri {
        pext::SimpleExtensionUri {
            extension_uri_anchor: anchor,
            uri: uri_str.to_string(),
        }
    }

    fn new_ext_fn(anchor: u32, uri_ref: u32, name: &str) -> pext::SimpleExtensionDeclaration {
        pext::SimpleExtensionDeclaration {
            mapping_type: Some(MappingType::ExtensionFunction(ExtensionFunction {
                extension_uri_reference: uri_ref,
                function_anchor: anchor,
                name: name.to_string(),
            })),
        }
    }

    fn new_ext_type(anchor: u32, uri_ref: u32, name: &str) -> pext::SimpleExtensionDeclaration {
        pext::SimpleExtensionDeclaration {
            mapping_type: Some(MappingType::ExtensionType(ExtensionType {
                extension_uri_reference: uri_ref,
                type_anchor: anchor,
                name: name.to_string(),
            })),
        }
    }

    fn new_type_var(anchor: u32, uri_ref: u32, name: &str) -> pext::SimpleExtensionDeclaration {
        pext::SimpleExtensionDeclaration {
            mapping_type: Some(MappingType::ExtensionTypeVariation(
                ExtensionTypeVariation {
                    extension_uri_reference: uri_ref,
                    type_variation_anchor: anchor,
                    name: name.to_string(),
                },
            )),
        }
    }

    fn assert_no_errors(errs: &[InsertError]) {
        for err in errs {
            println!("Error: {:?}", err);
        }
        assert!(errs.is_empty());
    }

    fn unwrap_new_extensions(
        uris: impl IntoIterator<Item = pext::SimpleExtensionUri>,
        extensions: impl IntoIterator<Item = pext::SimpleExtensionDeclaration>,
    ) -> SimpleExtensions {
        let (exts, errs) = SimpleExtensions::from_extensions(uris, extensions);
        assert_no_errors(&errs);
        exts
    }

    #[test]
    fn test_extension_lookup_empty() {
        let lookup = SimpleExtensions::new();
        assert!(lookup.find_uri(1).is_err());
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
        let uris = vec![new_uri(1, "uri1"), new_uri(2, "uri2")];
        let extensions = vec![
            new_ext_fn(10, 1, "func1"),
            new_ext_type(20, 1, "type1"),
            new_type_var(30, 2, "var1"),
        ];
        let exts = unwrap_new_extensions(uris, extensions);

        assert_eq!(exts.find_uri(1).unwrap(), "uri1");
        assert_eq!(exts.find_uri(2).unwrap(), "uri2");
        assert!(exts.find_uri(3).is_err());

        let (uri, name) = exts.find_by_anchor(ExtensionKind::Function, 10).unwrap();
        assert_eq!(name, "func1");
        assert_eq!(uri, 1);
        assert!(exts.find_by_anchor(ExtensionKind::Function, 11).is_err());

        let (uri, name) = exts.find_by_anchor(ExtensionKind::Type, 20).unwrap();
        assert_eq!(name, "type1");
        assert_eq!(uri, 1);
        assert!(exts.find_by_anchor(ExtensionKind::Type, 21).is_err());

        let (uri, name) = exts
            .find_by_anchor(ExtensionKind::TypeVariation, 30)
            .unwrap();
        assert_eq!(name, "var1");
        assert_eq!(uri, 2);
        assert!(
            exts.find_by_anchor(ExtensionKind::TypeVariation, 31)
                .is_err()
        );
    }

    #[test]
    fn test_from_extensions_duplicates() {
        let uris = vec![
            new_uri(1, "uri_old"),
            new_uri(1, "uri_new"),
            new_uri(2, "second"),
        ];
        let extensions = vec![
            new_ext_fn(10, 1, "func_old"),
            new_ext_fn(10, 2, "func_new"), // Duplicate function anchor
            new_ext_fn(11, 3, "func_missing"),
        ];
        let (exts, errs) = SimpleExtensions::from_extensions(uris, extensions);
        assert_eq!(
            errs,
            vec![
                InsertError::DuplicateUriAnchor {
                    anchor: 1,
                    name: "uri_new".to_string(),
                    prev: "uri_old".to_string()
                },
                InsertError::DuplicateAnchor {
                    kind: ExtensionKind::Function,
                    anchor: 10,
                    prev: "func_old".to_string(),
                    name: "func_new".to_string()
                },
                InsertError::MissingUri {
                    kind: ExtensionKind::Function,
                    anchor: 11,
                    name: "func_missing".to_string(),
                    uri: 3,
                },
            ]
        );

        // This is a duplicate anchor, so the first one is used.
        assert_eq!(exts.find_uri(1).unwrap(), "uri_old");
        assert_eq!(
            exts.find_by_anchor(ExtensionKind::Function, 10).unwrap(),
            (1, "func_old")
        );
    }

    #[test]
    fn test_from_extensions_invalid_mapping_type() {
        let extensions = vec![pext::SimpleExtensionDeclaration { mapping_type: None }];

        let (_exts, errs) = SimpleExtensions::from_extensions(vec![], extensions);
        assert_eq!(errs.len(), 1);
        let err = &errs[0];
        assert_eq!(err, &InsertError::MissingMappingType);
    }

    #[test]
    fn test_find_by_name() {
        let uris = vec![new_uri(1, "uri1")];
        let extensions = vec![
            new_ext_fn(10, 1, "name1"),
            new_ext_fn(11, 1, "name2"),
            new_ext_fn(12, 1, "name1"), // Duplicate name
            new_ext_type(20, 1, "type_name1"),
            new_type_var(30, 1, "var_name1"),
        ];
        let exts = unwrap_new_extensions(uris, extensions);

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
        let uris = vec![
            new_uri(1, "/my/uri1"),
            new_uri(42, "/another/uri"),
            new_uri(4091, "/big/anchor"),
        ];
        let extensions = vec![
            new_ext_fn(10, 1, "my_func"),
            new_ext_type(20, 1, "my_type"),
            new_type_var(30, 42, "my_var"),
            new_ext_fn(11, 42, "another_func"),
            new_ext_fn(108812, 4091, "big_func"),
        ];
        let exts = unwrap_new_extensions(uris, extensions);
        let display_str = exts.to_string("  ");

        let expected = r"
=== Extensions
URIs:
  @  1: /my/uri1
  @ 42: /another/uri
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
            .add_extension_uri("/uri/common".to_string(), 1)
            .unwrap();
        extensions
            .add_extension_uri("/uri/specific_funcs".to_string(), 2)
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
URIs:
  @  1: /uri/common
  @  2: /uri/specific_funcs
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
}
