use prost::{Message, Name};

use crate::extensions::registry::ExtensionError;

/// A wrapper around the protobuf `Any` type. Converts to or from
/// `prost_types::Any` or `pbjson_types::Any`.
#[derive(Debug, Clone, PartialEq)]
pub struct Any {
    pub type_url: String,
    pub value: Vec<u8>,
}

/// A reference to a protobuf `Any` type. Can be created from references to
/// [`prost_types::Any`](prost_types::Any),
/// [`pbjson_types::Any`](pbjson_types::Any), or our own [`Any`](Any) type.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct AnyRef<'a> {
    pub type_url: &'a str,
    pub value: &'a [u8],
}

impl<'a> AnyRef<'a> {
    /// Create a new [`AnyRef`]
    pub fn new(type_url: &'a str, value: &'a [u8]) -> Self {
        Self { type_url, value }
    }

    /// Decode this [`AnyRef`] as a specific protobuf message type
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

        let message = M::decode(self.value)
            .map_err(|e| ExtensionError::ParseError(format!("Failed to decode message: {e}")))?;

        Ok(message)
    }
}

impl<'a> From<&'a Any> for AnyRef<'a> {
    fn from(any: &'a Any) -> Self {
        Self {
            type_url: &any.type_url,
            value: &any.value,
        }
    }
}

impl<'a> From<&'a prost_types::Any> for AnyRef<'a> {
    fn from(any: &'a prost_types::Any) -> Self {
        Self {
            type_url: &any.type_url,
            value: &any.value,
        }
    }
}

#[cfg(feature = "serde")]
impl<'a> From<&'a pbjson_types::Any> for AnyRef<'a> {
    fn from(any: &'a pbjson_types::Any) -> Self {
        Self {
            type_url: &any.type_url,
            value: &any.value,
        }
    }
}

impl Any {
    /// Create a new `Any` for the given type from the given value bytes.
    pub fn new(type_url: String, value: Vec<u8>) -> Self {
        Self { type_url, value }
    }

    /// Convert this [`Any`] to a [`AnyRef`].
    pub fn as_ref(&self) -> AnyRef<'_> {
        AnyRef::from(self)
    }

    /// Decode this [`Any`] as a specific protobuf message type
    pub fn decode<M>(&self) -> Result<M, ExtensionError>
    where
        M: prost::Message + Name + Default,
    {
        self.as_ref().decode()
    }

    /// Encode a protobuf message into an [`Any`].
    pub fn encode<M: Message + Name + Default>(message: &M) -> Result<Self, ExtensionError> {
        let mut buf = Vec::new();
        message
            .encode(&mut buf)
            .map_err(|e| ExtensionError::EncodeError(format!("Failed to encode message: {e}")))?;

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

#[cfg(feature = "serde")]
impl From<pbjson_types::Any> for Any {
    fn from(any: pbjson_types::Any) -> Self {
        Self {
            type_url: any.type_url,
            value: any.value.to_vec(),
        }
    }
}

#[cfg(feature = "serde")]
impl From<Any> for pbjson_types::Any {
    fn from(any: Any) -> Self {
        Self {
            type_url: any.type_url,
            value: any.value.into(),
        }
    }
}
