#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Reference(Reference),
    Literal(Literal),
    FunctionCall(Box<FunctionCall>), // Boxed because FunctionCall can contain Vec<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Reference(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    String(String),
    // TODO: other literal types like Float, Boolean, …
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    // In the order they appear in the grammar. Name and arguments are required; others are optional.
    // E.g. fn() is valid with
    pub name: String,
    pub parameters: Option<Vec<String>>,
    pub anchor: Option<u32>,
    pub uri_anchor: Option<u32>,
    pub arguments: Vec<Expression>,
}
