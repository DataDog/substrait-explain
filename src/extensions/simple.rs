use std::collections::BTreeMap;

use substrait::proto::extensions as pext;

use pext::simple_extension_declaration::{
    ExtensionFunction, ExtensionType, ExtensionTypeVariation, MappingType,
};

#[derive(Debug, Clone)]
pub struct ExtensionError {
    pub message: &'static str,
    pub description: String,
}

/// ExtensionLookup contains mappings from anchors to extension URIs, functions,
/// types, and type variations.
#[derive(Default, Debug, Clone)]
pub struct ExtensionLookup {
    // Maps from extension URI anchor to URI
    uris: BTreeMap<u32, pext::SimpleExtensionUri>,
    // Maps from function anchor to (extension URI anchor, function name)
    functions: BTreeMap<u32, ExtensionFunction>,
    // Maps from type anchor to (extension URI anchor, type name)
    types: BTreeMap<u32, ExtensionType>,
    // Maps from type variation anchor to (extension URI anchor, type variation name)
    type_variations: BTreeMap<u32, ExtensionTypeVariation>,
}

impl ExtensionLookup {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_extensions(
        uris: Vec<pext::SimpleExtensionUri>,
        extensions: Vec<pext::SimpleExtensionDeclaration>,
    ) -> Result<Self, ExtensionError> {
        let mut uri_map = BTreeMap::new();
        let mut functions = BTreeMap::new();
        let mut types = BTreeMap::new();
        let mut type_variations = BTreeMap::new();

        for uri in uris {
            // TODO: Error on duplicate URIs
            uri_map.insert(uri.extension_uri_anchor, uri);
        }

        for extension in extensions {
            match extension.mapping_type {
                Some(MappingType::ExtensionType(t)) => {
                    types.insert(t.type_anchor, t);
                }
                Some(MappingType::ExtensionTypeVariation(v)) => {
                    type_variations.insert(v.type_variation_anchor, v);
                }
                Some(MappingType::ExtensionFunction(f)) => {
                    functions.insert(f.function_anchor, f);
                }
                None => {
                    return Err(ExtensionError {
                        // TODO: Use prost::Name here
                        message: "SimpleExtensionDeclaration",
                        description: "Required MappingType unset".to_owned(),
                    });
                }
            }
        }

        Ok(ExtensionLookup {
            uris: uri_map,
            functions,
            types,
            type_variations,
        })
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn add_extension_uri(&mut self, uri: String, anchor: u32) {
        self.uris.insert(
            anchor,
            pext::SimpleExtensionUri {
                extension_uri_anchor: anchor,
                uri,
            },
        );
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn add_extension_function(&mut self, uri: u32, anchor: u32, name: String) {
        self.functions.insert(
            anchor,
            ExtensionFunction {
                extension_uri_reference: uri,
                function_anchor: anchor,
                name,
            },
        );
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn add_extension_type(&mut self, uri: u32, anchor: u32, name: String) {
        self.types.insert(
            anchor,
            ExtensionType {
                extension_uri_reference: uri,
                type_anchor: anchor,
                name,
            },
        );
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn add_extension_type_variation(&mut self, uri: u32, anchor: u32, name: String) {
        self.type_variations.insert(
            anchor,
            ExtensionTypeVariation {
                extension_uri_reference: uri,
                type_variation_anchor: anchor,
                name,
            },
        );
    }
}

pub trait SimpleExtensions {
    fn find_uri(&self, anchor: u32) -> Option<pext::SimpleExtensionUri>;
    fn find_function(&self, anchor: u32) -> Option<ExtensionFunction>;
    fn find_type(&self, anchor: u32) -> Option<ExtensionType>;
    fn find_type_variation(&self, anchor: u32) -> Option<ExtensionTypeVariation>;

    fn find_functions(&self, name: &str) -> impl Iterator<Item = &ExtensionFunction>;
    fn find_types(&self, name: &str) -> impl Iterator<Item = &ExtensionType>;
    fn find_variations(&self, name: &str) -> impl Iterator<Item = &ExtensionTypeVariation>;
}

impl SimpleExtensions for ExtensionLookup {
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
