#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Reference(Reference),
    Literal(Literal),
    FunctionCall(Box<FunctionCall>), // Boxed because FunctionCall can contain Vec<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Reference {
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    String(String),
    // Add other literal types like Float, Boolean later if needed
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub parameters: Option<Vec<String>>,
    pub variant: Option<String>,
    pub anchor: Option<u32>,
    pub uri_anchor: Option<u32>,
    pub arguments: Vec<Expression>, // Arguments list can be empty, but `()` are required
}
