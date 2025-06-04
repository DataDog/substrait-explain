use thiserror::Error;

use std::collections::{BTreeMap, btree_map::Entry};
use std::fmt;

use substrait::proto::extensions as pext;

use pext::simple_extension_declaration::{
    ExtensionFunction, ExtensionType, ExtensionTypeVariation, MappingType,
};

pub const EXTENSIONS_HEADER: &str = "=== Extensions";
pub const EXTENSION_URIS_HEADER: &str = "URIs:";
pub const EXTENSION_FUNCTIONS_HEADER: &str = "Functions:";
pub const EXTENSION_TYPES_HEADER: &str = "Types:";
pub const EXTENSION_TYPE_VARIATIONS_HEADER: &str = "Type Variations:";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtensionKind {
    Uri,
    Function,
    Type,
    TypeVariation,
}

impl fmt::Display for ExtensionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtensionKind::Uri => write!(f, "URI"),
            ExtensionKind::Function => write!(f, "Function"),
            ExtensionKind::Type => write!(f, "Type"),
            ExtensionKind::TypeVariation => write!(f, "Type Variation"),
        }
    }
}

#[derive(Error, Debug, PartialEq, Clone)]
pub enum ExtensionError {
    #[error("Extension declaration missing mapping type")]
    MissingMappingType,
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
    uris: BTreeMap<u32, pext::SimpleExtensionUri>,
    // Maps from function anchor to (extension URI anchor, function name)
    functions: BTreeMap<u32, ExtensionFunction>,
    // Maps from type anchor to (extension URI anchor, type name)
    types: BTreeMap<u32, ExtensionType>,
    // Maps from type variation anchor to (extension URI anchor, type variation name)
    type_variations: BTreeMap<u32, ExtensionTypeVariation>,
}

impl SimpleExtensions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_extensions(
        uris: impl IntoIterator<Item = pext::SimpleExtensionUri>,
        extensions: impl IntoIterator<Item = pext::SimpleExtensionDeclaration>,
    ) -> (Self, Vec<ExtensionError>) {
        // TODO: this checks for missing URIs and duplicate anchors, but not
        // duplicate names with the same anchor.

        let mut exts = Self::new();

        let mut errors = Vec::<ExtensionError>::new();

        for uri in uris {
            if let Err(e) = exts.add_extension_uri(uri.uri, uri.extension_uri_anchor) {
                errors.push(e);
            }
        }

        for extension in extensions {
            match extension.mapping_type {
                Some(MappingType::ExtensionType(t)) => {
                    if let Err(e) =
                        exts.add_extension_type(t.extension_uri_reference, t.type_anchor, t.name)
                    {
                        errors.push(e);
                    }
                }
                Some(MappingType::ExtensionFunction(f)) => {
                    if let Err(e) = exts.add_extension_function(
                        f.extension_uri_reference,
                        f.function_anchor,
                        f.name,
                    ) {
                        errors.push(e);
                    }
                }
                Some(MappingType::ExtensionTypeVariation(v)) => {
                    if let Err(e) = exts.add_extension_type_variation(
                        v.extension_uri_reference,
                        v.type_variation_anchor,
                        v.name,
                    ) {
                        errors.push(e);
                    }
                }
                None => {
                    errors.push(ExtensionError::MissingMappingType);
                }
            }
        }

        (exts, errors)
    }

    pub(crate) fn add_extension_uri(
        &mut self,
        uri: String,
        anchor: u32,
    ) -> Result<(), ExtensionError> {
        match self.uris.entry(anchor) {
            Entry::Occupied(e) => {
                return Err(ExtensionError::DuplicateAnchor {
                    kind: ExtensionKind::Uri,
                    anchor,
                    prev: e.get().uri.clone(),
                    name: uri,
                });
            }
            Entry::Vacant(e) => {
                e.insert(pext::SimpleExtensionUri {
                    extension_uri_anchor: anchor,
                    uri,
                });
            }
        }
        Ok(())
    }

    // Helper function to return the correct error for missing URIs and/or duplicate anchors
    fn missing_or_duplicate_error(
        // Set to true if the URI is missing
        missing_uri: bool,
        kind: ExtensionKind,
        anchor: u32,
        name: String,
        // Set to Some(name) if duplicate
        prev: Option<String>,
        uri: u32,
    ) -> Result<(), ExtensionError> {
        match (missing_uri, prev) {
            (true, Some(prev)) => Err(ExtensionError::DuplicateAndMissingUri {
                kind,
                anchor,
                prev,
                name,
                uri,
            }),
            (false, Some(prev)) => Err(ExtensionError::DuplicateAnchor {
                kind,
                anchor,
                prev,
                name,
            }),
            (true, None) => Err(ExtensionError::MissingUri {
                kind,
                anchor,
                name,
                uri,
            }),
            (false, None) => Ok(()),
        }
    }

    pub(crate) fn add_extension_function(
        &mut self,
        uri: u32,
        anchor: u32,
        name: String,
    ) -> Result<(), ExtensionError> {
        let missing_uri = !self.uris.contains_key(&uri);

        let duplicate = match self.functions.entry(anchor) {
            Entry::Occupied(e) => Some(e.get().name.clone()),
            Entry::Vacant(v) => {
                v.insert(ExtensionFunction {
                    extension_uri_reference: uri,
                    function_anchor: anchor,
                    name: name.clone(),
                });
                None
            }
        };
        Self::missing_or_duplicate_error(
            missing_uri,
            ExtensionKind::Function,
            anchor,
            name,
            duplicate,
            uri,
        )
    }

    pub(crate) fn add_extension_type(
        &mut self,
        uri: u32,
        anchor: u32,
        name: String,
    ) -> Result<(), ExtensionError> {
        let missing_uri = !self.uris.contains_key(&uri);

        let duplicate = match self.types.entry(anchor) {
            Entry::Occupied(e) => Some(e.get().name.clone()),
            Entry::Vacant(v) => {
                v.insert(ExtensionType {
                    extension_uri_reference: uri,
                    type_anchor: anchor,
                    name: name.clone(),
                });
                None
            }
        };
        Self::missing_or_duplicate_error(
            missing_uri,
            ExtensionKind::Type,
            anchor,
            name,
            duplicate,
            uri,
        )
    }

    pub(crate) fn add_extension_type_variation(
        &mut self,
        uri: u32,
        anchor: u32,
        name: String,
    ) -> Result<(), ExtensionError> {
        let missing_uri = !self.uris.contains_key(&uri);

        let duplicate = match self.type_variations.entry(anchor) {
            Entry::Occupied(e) => Some(e.get().name.clone()),
            Entry::Vacant(v) => {
                v.insert(ExtensionTypeVariation {
                    extension_uri_reference: uri,
                    type_variation_anchor: anchor,
                    name: name.clone(),
                });
                None
            }
        };

        Self::missing_or_duplicate_error(
            missing_uri,
            ExtensionKind::TypeVariation,
            anchor,
            name,
            duplicate,
            uri,
        )
    }

    pub fn is_empty(&self) -> bool {
        self.uris.is_empty()
            && self.functions.is_empty()
            && self.types.is_empty()
            && self.type_variations.is_empty()
    }

    pub fn write<W: fmt::Write>(&self, w: &mut W, indent: &str) -> fmt::Result {
        if self.uris.is_empty()
            && self.functions.is_empty()
            && self.types.is_empty()
            && self.type_variations.is_empty()
        {
            // No extensions, so no need to write anything.
            return Ok(());
        }

        // TODO: write the header. I think we can put this in the main block
        // writeln!(w, "{}", EXTENSIONS_HEADER)?;
        if !self.uris.is_empty() {
            writeln!(w, "{}", EXTENSION_URIS_HEADER)?;
            for (anchor, uri) in &self.uris {
                writeln!(w, "{}@{:3}: {}", indent, anchor, uri.uri)?;
            }
        }
        if !self.functions.is_empty() {
            writeln!(w, "{}", EXTENSION_FUNCTIONS_HEADER)?;
            for (anchor, function) in &self.functions {
                writeln!(
                    w,
                    "{}#{anchor:3} @{uri_ref:3}: {name}",
                    indent,
                    anchor = anchor,
                    uri_ref = function.extension_uri_reference,
                    name = function.name
                )?;
            }
        }
        if !self.types.is_empty() {
            writeln!(w, "{}", EXTENSION_TYPES_HEADER)?;
            for (anchor, typ) in &self.types {
                writeln!(
                    w,
                    "{}#{anchor:3} @{uri_ref:3}: {name}",
                    indent,
                    anchor = anchor,
                    uri_ref = typ.extension_uri_reference,
                    name = typ.name
                )?;
            }
        }
        if !self.type_variations.is_empty() {
            writeln!(w, "{}", EXTENSION_TYPE_VARIATIONS_HEADER)?;
            for (anchor, variation) in &self.type_variations {
                writeln!(
                    w,
                    "{}#{anchor:3} @{uri_ref:3}: {name}",
                    indent,
                    anchor = anchor,
                    uri_ref = variation.extension_uri_reference,
                    name = variation.name
                )?;
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

/// ExtensionLookup is a trait that provides methods for looking up extensions
/// by anchor or name.
pub trait ExtensionLookup {
    fn find_uri(&self, anchor: u32) -> Option<pext::SimpleExtensionUri>;
    fn find_function(&self, anchor: u32) -> Option<ExtensionFunction>;
    fn find_type(&self, anchor: u32) -> Option<ExtensionType>;
    fn find_type_variation(&self, anchor: u32) -> Option<ExtensionTypeVariation>;

    fn find_functions(&self, name: &str) -> impl Iterator<Item = &ExtensionFunction>;
    fn find_types(&self, name: &str) -> impl Iterator<Item = &ExtensionType>;
    fn find_variations(&self, name: &str) -> impl Iterator<Item = &ExtensionTypeVariation>;
}

impl ExtensionLookup for SimpleExtensions {
    fn find_uri(&self, anchor: u32) -> Option<pext::SimpleExtensionUri> {
        self.uris.get(&anchor).cloned()
    }

    fn find_function(&self, anchor: u32) -> Option<ExtensionFunction> {
        self.functions.get(&anchor).cloned()
    }

    fn find_type(&self, anchor: u32) -> Option<ExtensionType> {
        self.types.get(&anchor).cloned()
    }

    fn find_type_variation(&self, anchor: u32) -> Option<ExtensionTypeVariation> {
        self.type_variations.get(&anchor).cloned()
    }

    fn find_functions(&self, name: &str) -> impl Iterator<Item = &ExtensionFunction> {
        self.functions.values().filter(move |f| f.name == name)
    }

    fn find_types(&self, name: &str) -> impl Iterator<Item = &ExtensionType> {
        self.types.values().filter(move |f| f.name == name)
    }

    fn find_variations(&self, name: &str) -> impl Iterator<Item = &ExtensionTypeVariation> {
        self.type_variations
            .values()
            .filter(move |f| f.name == name)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use pext::simple_extension_declaration::{
        ExtensionFunction, ExtensionType, ExtensionTypeVariation, MappingType,
    };
    use substrait::proto::extensions as pext;

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

    fn assert_no_errors(errs: &[ExtensionError]) {
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
        dbg!(&exts);
        assert_no_errors(&errs);
        exts
    }

    #[test]
    fn test_extension_lookup_empty() {
        let lookup = SimpleExtensions::new();
        assert!(lookup.find_uri(1).is_none());
        assert!(lookup.find_function(1).is_none());
        assert!(lookup.find_type(1).is_none());
        assert!(lookup.find_type_variation(1).is_none());
        assert_eq!(lookup.find_functions("any").count(), 0);
        assert_eq!(lookup.find_types("any").count(), 0);
        assert_eq!(lookup.find_variations("any").count(), 0);
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

        assert_eq!(exts.find_uri(1).unwrap().uri, "uri1");
        assert_eq!(exts.find_uri(2).unwrap().uri, "uri2");
        assert!(exts.find_uri(3).is_none());

        let func1 = exts.find_function(10).unwrap();
        assert_eq!(func1.name, "func1");
        assert_eq!(func1.extension_uri_reference, 1);
        assert!(exts.find_function(11).is_none());

        let type1 = exts.find_type(20).unwrap();
        assert_eq!(type1.name, "type1");
        assert_eq!(type1.extension_uri_reference, 1);
        assert!(exts.find_type(21).is_none());

        let var1 = exts.find_type_variation(30).unwrap();
        assert_eq!(var1.name, "var1");
        assert_eq!(var1.extension_uri_reference, 2);
        assert!(exts.find_type_variation(31).is_none());
    }

    #[test]
    fn test_from_extensions_duplicates_overwrite() {
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
                ExtensionError::DuplicateAnchor {
                    kind: ExtensionKind::Uri,
                    anchor: 1,
                    name: "uri_new".to_string(),
                    prev: "uri_old".to_string()
                },
                ExtensionError::DuplicateAnchor {
                    kind: ExtensionKind::Function,
                    anchor: 10,
                    prev: "func_old".to_string(),
                    name: "func_new".to_string()
                },
                ExtensionError::MissingUri {
                    kind: ExtensionKind::Function,
                    anchor: 11,
                    name: "func_missing".to_string(),
                    uri: 3,
                },
            ]
        );

        assert_eq!(exts.find_uri(1).unwrap().uri, "uri_old");
        assert_eq!(exts.find_function(10).unwrap().name, "func_old");
    }

    #[test]
    fn test_from_extensions_invalid_mapping_type() {
        let extensions = vec![pext::SimpleExtensionDeclaration { mapping_type: None }];

        let (_exts, errs) = SimpleExtensions::from_extensions(vec![], extensions);
        assert_eq!(errs.len(), 1);
        let err = &errs[0];
        assert_eq!(err, &ExtensionError::MissingMappingType);
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

        let funcs_name1: Vec<_> = exts.find_functions("name1").collect();
        assert_eq!(funcs_name1.len(), 2);
        assert!(funcs_name1.iter().any(|f| f.function_anchor == 10));
        assert!(funcs_name1.iter().any(|f| f.function_anchor == 12));

        let funcs_name2: Vec<_> = exts.find_functions("name2").collect();
        assert_eq!(funcs_name2.len(), 1);
        assert_eq!(funcs_name2[0].function_anchor, 11);

        assert_eq!(exts.find_functions("non_existent_name").count(), 0);

        let types_name1: Vec<_> = exts.find_types("type_name1").collect();
        assert_eq!(types_name1.len(), 1);
        assert_eq!(types_name1[0].type_anchor, 20);
        assert_eq!(exts.find_types("non_existent_type_name").count(), 0);

        let vars_name1: Vec<_> = exts.find_variations("var_name1").collect();
        assert_eq!(vars_name1.len(), 1);
        assert_eq!(vars_name1[0].type_variation_anchor, 30);
        assert_eq!(exts.find_variations("non_existent_var_name").count(), 0);
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
}
