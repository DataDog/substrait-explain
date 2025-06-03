#[derive(Debug, Clone, PartialEq)]
pub struct URIExtensionDeclaration {
    pub anchor: u32,
    pub uri: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SimpleExtensionDeclaration {
    pub anchor: u32,
    pub uri_anchor: u32,
    pub name: String,
}
