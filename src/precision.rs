//! Sub-second precision for literal values, shared by the parser and textifier.

use std::fmt;

/// A sub-second precision that names a unit: the decimal exponent Substrait
/// stores, and the duration suffix that writes it.
///
/// Substrait allows any precision from 0 to 12 on a *type*, but a literal
/// *value* has to be written down, and only these five have a unit to write it
/// in. Constructing a `SupportedPrecision` checks that once, so code holding one
/// can convert without re-checking.
///
/// Not every literal supports every variant: `chrono`-backed literals
/// (timestamp, time) top out at nanoseconds, so they reject `Picoseconds`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum SupportedPrecision {
    Seconds,      // 0
    Milliseconds, // 3
    Microseconds, // 6
    Nanoseconds,  // 9
    Picoseconds,  // 12
}

impl SupportedPrecision {
    /// The Substrait precision unit exponent (`0`, `3`, `6`, `9`, or `12`).
    pub fn units(self) -> i32 {
        match self {
            SupportedPrecision::Seconds => 0,
            SupportedPrecision::Milliseconds => 3,
            SupportedPrecision::Microseconds => 6,
            SupportedPrecision::Nanoseconds => 9,
            SupportedPrecision::Picoseconds => 12,
        }
    }

    /// Returns `None` for a precision with no unit to write it in.
    pub fn from_units(units: i32) -> Option<Self> {
        match units {
            0 => Some(SupportedPrecision::Seconds),
            3 => Some(SupportedPrecision::Milliseconds),
            6 => Some(SupportedPrecision::Microseconds),
            9 => Some(SupportedPrecision::Nanoseconds),
            12 => Some(SupportedPrecision::Picoseconds),
            _ => None,
        }
    }

    /// The duration-string suffix for sub-seconds at this precision.
    ///
    /// `Seconds` has none: at precision 0 there is no sub-second component.
    pub fn subsecond_unit(self) -> Option<&'static str> {
        match self {
            SupportedPrecision::Seconds => None,
            SupportedPrecision::Milliseconds => Some("ms"),
            SupportedPrecision::Microseconds => Some("us"),
            SupportedPrecision::Nanoseconds => Some("ns"),
            SupportedPrecision::Picoseconds => Some("ps"),
        }
    }

    /// The precision implied by a duration-string sub-second suffix.
    pub fn from_subsecond_unit(unit: &str) -> Option<Self> {
        match unit {
            "ms" => Some(SupportedPrecision::Milliseconds),
            "us" => Some(SupportedPrecision::Microseconds),
            "ns" => Some(SupportedPrecision::Nanoseconds),
            "ps" => Some(SupportedPrecision::Picoseconds),
            _ => None,
        }
    }
}

impl fmt::Display for SupportedPrecision {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.units())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_units_roundtrip() {
        for units in [0, 3, 6, 9, 12] {
            let precision = SupportedPrecision::from_units(units).unwrap();
            assert_eq!(precision.units(), units);
        }
        // In range for a type, but with no unit to write a value in.
        assert_eq!(SupportedPrecision::from_units(4), None);
        assert_eq!(SupportedPrecision::from_units(13), None);
        assert_eq!(SupportedPrecision::from_units(-1), None);
    }

    #[test]
    fn test_subsecond_unit_roundtrip() {
        for unit in ["ms", "us", "ns", "ps"] {
            let precision = SupportedPrecision::from_subsecond_unit(unit).unwrap();
            assert_eq!(precision.subsecond_unit(), Some(unit));
        }
        // Precision 0 has no sub-second component, and "s" is not one.
        assert_eq!(SupportedPrecision::Seconds.subsecond_unit(), None);
        assert_eq!(SupportedPrecision::from_subsecond_unit("s"), None);
    }
}
