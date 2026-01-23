#[derive(Debug, Clone, PartialEq)]
pub struct ExtensionDeclaration {
    pub anchor: u32,
    pub urn: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SimpleExtensionDeclaration {
    pub anchor: u32,
    pub urn_anchor: u32,
    pub name: String,
}
