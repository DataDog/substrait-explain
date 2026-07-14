//! Sub-second precision, shared by the Substrait types that carry one.

use std::fmt;

/// The number of decimal digits used to express fractions of a second.
///
/// Substrait restricts precision to `0..=12` — seconds through picoseconds —
/// for every type that carries one (`Type.IntervalDay.precision`,
/// `Type.PrecisionTimestamp.precision`, `Type.PrecisionTime.precision`).
/// Constructing a `Precision` checks that range once, so code holding one does
/// not need to re-validate it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Precision(i32);

impl Precision {
    /// The inclusive range Substrait allows for a precision value.
    const RANGE: std::ops::RangeInclusive<i32> = 0..=12;

    /// Microsecond precision, which Substrait uses as the meaning of interval
    /// values recorded before `precision` existed.
    pub const MICROSECONDS: Precision = Precision(6);

    /// Returns `None` if `value` is outside Substrait's `0..=12` range.
    pub fn new(value: i32) -> Option<Self> {
        Self::RANGE.contains(&value).then_some(Precision(value))
    }

    /// The precision as the integer Substrait stores.
    pub fn value(self) -> i32 {
        self.0
    }

    /// The duration-string unit that expresses sub-seconds at this precision.
    ///
    /// Only the precisions that name a unit can carry a non-zero sub-second
    /// term in an `interval_day` literal; e.g. at precision 4 there is no way
    /// to write "1/10,000 of a second" as a duration term.
    pub fn subsecond_unit(self) -> Option<&'static str> {
        match self.0 {
            3 => Some("ms"),
            6 => Some("us"),
            9 => Some("ns"),
            12 => Some("ps"),
            _ => None,
        }
    }

    /// The precision implied by a duration-string sub-second unit.
    pub fn from_subsecond_unit(unit: &str) -> Option<Self> {
        match unit {
            "ms" => Some(Precision(3)),
            "us" => Some(Precision(6)),
            "ns" => Some(Precision(9)),
            "ps" => Some(Precision(12)),
            _ => None,
        }
    }
}

impl fmt::Display for Precision {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_precision_range() {
        assert_eq!(Precision::new(0).map(Precision::value), Some(0));
        assert_eq!(Precision::new(12).map(Precision::value), Some(12));
        assert_eq!(Precision::new(-1), None);
        assert_eq!(Precision::new(13), None);
    }

    #[test]
    fn test_subsecond_unit_roundtrip() {
        for unit in ["ms", "us", "ns", "ps"] {
            let precision = Precision::from_subsecond_unit(unit).unwrap();
            assert_eq!(precision.subsecond_unit(), Some(unit));
        }
        // Precisions that don't land on a unit boundary have no unit.
        assert_eq!(Precision::new(0).unwrap().subsecond_unit(), None);
        assert_eq!(Precision::new(4).unwrap().subsecond_unit(), None);
        assert_eq!(Precision::from_subsecond_unit("s"), None);
    }
}
