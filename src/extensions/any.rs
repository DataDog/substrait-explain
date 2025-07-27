use prost::{Message, Name};

use crate::extensions::registry::ExtensionError;

/// Our own Any type that abstracts over feature-dependent implementations
#[derive(Debug, Clone, PartialEq)]
pub struct Any {
    pub type_url: String,
    pub value: Vec<u8>,
}

impl Any {
    /// Create a new Any with the given type URL and value bytes
    pub fn new(type_url: String, value: Vec<u8>) -> Self {
        Self { type_url, value }
    }

    /// Decode this Any as a specific protobuf message type
    pub fn decode<M>(&self) -> Result<M, ExtensionError>
    where
        M: prost::Message + Name + Default,
    {
        if self.type_url != M::type_url() {
            return Err(ExtensionError::ParseError(format!(
                "Type URL mismatch: expected {}, got {}",
                M::type_url(),
                self.type_url
            )));
        }

        let message = M::decode(&self.value[..])
            .map_err(|e| ExtensionError::ParseError(format!("Failed to decode message: {e}")))?;

        Ok(message)
    }

    /// Encode a protobuf message into an Any
    pub fn encode<M: Message + Name + Default>(message: &M) -> Result<Self, ExtensionError> {
        let mut buf = Vec::new();
        message
            .encode(&mut buf)
            .map_err(|e| ExtensionError::EncodeError(format!("Failed to encode message: {e}")))?;

        // Validate the encoding by attempting to decode it back
        // This ensures the data can be successfully round-tripped
        let _decoded: M = M::decode(&buf[..]).map_err(|e| {
            ExtensionError::EncodeError(format!("Failed to validate encoded message: {e}"))
        })?;

        Ok(Any {
            type_url: M::type_url(),
            value: buf,
        })
    }
}

impl From<prost_types::Any> for Any {
    fn from(any: prost_types::Any) -> Self {
        Self {
            type_url: any.type_url,
            value: any.value.to_vec(),
        }
    }
}

impl From<Any> for prost_types::Any {
    fn from(any: Any) -> Self {
        Self {
            type_url: any.type_url,
            value: any.value,
        }
    }
}

// Conversion from pbjson_types::Any
#[cfg(feature = "serde")]
impl From<pbjson_types::Any> for Any {
    fn from(any: pbjson_types::Any) -> Self {
        Self {
            type_url: any.type_url,
            value: any.value.to_vec(),
        }
    }
}

// Conversion to pbjson_types::Any
#[cfg(feature = "serde")]
impl From<Any> for pbjson_types::Any {
    fn from(any: Any) -> Self {
        Self {
            type_url: any.type_url,
            value: any.value.into(),
        }
    }
}
