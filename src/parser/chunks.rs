//! Physical-line / chunk management for the structural parser.
//!
//! This layer sits *below* the grammar and the relation tree. Its only job is
//! to walk the input one physical line at a time and group adjacent lines into
//! a **chunk**: a single contiguous slice of the original input that may span
//! several physical lines. A chunk is later handed to the grammar as one unit.
//!
//! The layer is deliberately ignorant of syntax. It does not know what a
//! continuation line (`- ...`) is, how indentation maps to depth, or that blank
//! lines should be skipped. All of that is *policy*, owned by the caller. This
//! layer only provides the mechanism:
//!
//! - [`ChunkCursor::peek_line`] — look at the next physical line without
//!   consuming it,
//! - [`ChunkCursor::merge`] — extend the current chunk to include that line,
//! - [`ChunkCursor::next`] — finish the current chunk and produce a cursor for
//!   the next one.
//!
//! ## Why the cursor *is* the chunk
//!
//! Rather than a separate "chunk" value plus a cursor that could drift out of
//! sync, the cursor itself represents the current chunk: `text[..current_end]`
//! is the chunk built so far, and `text[current_end..]` is everything after it.
//! A chunk starts empty, so the first physical line is added through the same
//! `peek_line` / `merge` path as every continuation line — there is no special
//! case for the first line.
//!
//! Because the only line you can `merge` is the one [`peek_line`] just returned
//! (which always starts exactly at `current_end`), a chunk is always a span of
//! *contiguously consumed* lines. That invariant is what keeps a chunk a single
//! `&str` slice instead of a list of fragments.
//!
//! [`peek_line`]: ChunkCursor::peek_line

/// A single physical line peeked from a [`ChunkCursor`], together with the
/// offset needed to consume it.
///
/// `line` is the line's content with the trailing newline (and any preceding
/// `\r`) removed — this is what the caller inspects to decide whether to merge.
/// `end` is the byte offset, within the originating cursor's `text`, of the
/// *start of the following line* (i.e. just past the consumed `\n`). It is only
/// meaningful when passed back to the same cursor via [`ChunkCursor::merge`].
#[derive(Debug, Clone, Copy)]
pub struct Line<'a> {
    line: &'a str,
    end: usize,
}

impl<'a> Line<'a> {
    /// The line's content, without the trailing newline or a preceding `\r`.
    pub fn as_str(&self) -> &'a str {
        self.line
    }
}

/// A forward cursor over the input that yields one chunk at a time.
///
/// The cursor represents the chunk currently being built: [`chunk`] is
/// `&text[..current_end]`, and the unread remainder is `text[current_end..]`.
/// See the [module docs](self) for the overall model.
///
/// The cursor is move-only on purpose: [`next`] consumes it by value, so a
/// finished chunk cannot be accidentally reused or finished twice.
///
/// [`chunk`]: ChunkCursor::chunk
/// [`next`]: ChunkCursor::next
#[derive(Debug)]
pub struct ChunkCursor<'a> {
    /// The original input, starting at the first byte of the current chunk.
    text: &'a str,
    /// Number of bytes of `text` merged into the current chunk so far. The
    /// chunk is `&text[..current_end]`; this always sits on a line boundary.
    current_end: usize,
    /// 1-based line number of the chunk's first physical line.
    start_line_no: i64,
    /// Line number that the next [`peek_line`](Self::peek_line) will return.
    /// Equivalently, `start_line_no + (lines merged so far)`.
    next_line_no: i64,
}

impl<'a> ChunkCursor<'a> {
    /// Create a cursor over `text`, whose first physical line is numbered
    /// `start_line_no`. Returns `None` if `text` is empty (no chunk to build).
    pub fn new(text: &'a str, start_line_no: i64) -> Option<Self> {
        if text.is_empty() {
            return None;
        }
        Some(ChunkCursor {
            text,
            current_end: 0,
            start_line_no,
            next_line_no: start_line_no,
        })
    }

    /// 1-based line number of the chunk's first physical line.
    pub fn start_line_no(&self) -> i64 {
        self.start_line_no
    }

    /// The next physical line after the current chunk, without consuming it.
    /// Returns `None` once the whole input has been consumed.
    pub fn peek_line(&self) -> Option<Line<'a>> {
        if self.current_end >= self.text.len() {
            return None;
        }

        let rest = &self.text[self.current_end..];
        match rest.find('\n') {
            Some(nl) => {
                // Exclude the '\n' and a preceding '\r' from the content, but
                // advance `end` past the '\n' to the start of the next line.
                let content_end = if nl > 0 && rest.as_bytes()[nl - 1] == b'\r' {
                    nl - 1
                } else {
                    nl
                };
                Some(Line {
                    line: &rest[..content_end],
                    end: self.current_end + nl + 1,
                })
            }
            // Last line: no trailing newline.
            None => Some(Line {
                line: rest,
                end: self.text.len(),
            }),
        }
    }

    /// Extend the current chunk to include `line`.
    ///
    /// `line` must be the value most recently returned by [`peek_line`] on this
    /// same cursor; passing anything else is a programming error.
    ///
    /// [`peek_line`]: Self::peek_line
    pub fn merge(&mut self, line: Line<'a>) {
        debug_assert!(
            line.end > self.current_end && line.end <= self.text.len(),
            "merge() given a Line whose end {} is outside the unread region [{}, {}]",
            line.end,
            self.current_end,
            self.text.len(),
        );
        debug_assert_eq!(
            line.line.as_ptr(),
            self.text[self.current_end..].as_ptr(),
            "merge() given a Line that did not come from this cursor's peek_line()",
        );

        self.current_end = line.end;
        self.next_line_no += 1;
    }

    /// Finish the current chunk: return its text together with a cursor for the
    /// next chunk, or `None` if the input is exhausted.
    pub fn next(self) -> (&'a str, Option<ChunkCursor<'a>>) {
        let chunk = &self.text[..self.current_end];
        let rest = &self.text[self.current_end..];

        let next = if rest.is_empty() {
            None
        } else {
            Some(ChunkCursor {
                text: rest,
                current_end: 0,
                start_line_no: self.next_line_no,
                next_line_no: self.next_line_no,
            })
        };

        (chunk, next)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Consume `text` into chunks, merging a line only when `merge` accepts it.
    /// Returns `(chunk_text, start_line_no, end_line_no)` for each chunk.
    fn collect(
        text: &str,
        mut merge: impl FnMut(&ChunkCursor, &Line) -> bool,
    ) -> Vec<(String, i64, i64)> {
        let mut out = Vec::new();
        let mut cursor = ChunkCursor::new(text, 1);
        while let Some(mut c) = cursor {
            // First line is always part of the chunk.
            let first = c.peek_line().expect("a fresh cursor has at least one line");
            c.merge(first);
            // Subsequent lines merge only if the policy accepts them.
            while let Some(line) = c.peek_line() {
                if merge(&c, &line) {
                    c.merge(line);
                } else {
                    break;
                }
            }
            // `end` reads the internal counter directly (white-box): the public
            // surface intentionally exposes only the start line, as no caller
            // needs the end line yet.
            let (start, end) = (c.start_line_no(), c.next_line_no - 1);
            let (chunk, rest) = c.next();
            out.push((chunk.to_string(), start, end));
            cursor = rest;
        }
        out
    }

    /// Never merge past the first line: one chunk per physical line.
    fn one_per_line(text: &str) -> Vec<(String, i64, i64)> {
        collect(text, |_, _| false)
    }

    #[test]
    fn empty_input_has_no_chunks() {
        assert!(ChunkCursor::new("", 1).is_none());
    }

    #[test]
    fn single_line_no_trailing_newline() {
        assert_eq!(one_per_line("abc"), vec![("abc".to_string(), 1, 1)]);
    }

    #[test]
    fn single_line_with_trailing_newline() {
        // The trailing newline does not produce a spurious empty chunk, but it
        // is part of the (non-final) chunk slice.
        assert_eq!(one_per_line("abc\n"), vec![("abc\n".to_string(), 1, 1)]);
    }

    #[test]
    fn multiple_lines_one_per_chunk() {
        assert_eq!(
            one_per_line("a\nb\nc"),
            vec![
                ("a\n".to_string(), 1, 1),
                ("b\n".to_string(), 2, 2),
                ("c".to_string(), 3, 3),
            ],
        );
    }

    #[test]
    fn blank_lines_are_their_own_chunks() {
        // The layer does not skip blanks; that is the caller's policy.
        assert_eq!(
            one_per_line("a\n\nb"),
            vec![
                ("a\n".to_string(), 1, 1),
                ("\n".to_string(), 2, 2),
                ("b".to_string(), 3, 3),
            ],
        );
    }

    #[test]
    fn peek_does_not_consume() {
        let cursor = ChunkCursor::new("a\nb", 1).unwrap();
        let first = cursor.peek_line().unwrap();
        let again = cursor.peek_line().unwrap();
        assert_eq!(first.as_str(), "a");
        assert_eq!(again.as_str(), "a");
        assert_eq!(cursor.current_end, 0); // nothing consumed yet
    }

    #[test]
    fn merge_builds_a_contiguous_multiline_slice() {
        // Merge a line when it is indented under the first and starts with "- ".
        let text = "Read:Virtual[\n  - (1, 'alice')\n  - => id:i64]\nNext";
        let chunks = collect(text, |_, line| line.as_str().trim_start().starts_with("- "));

        assert_eq!(chunks.len(), 2);
        // The whole multi-line relation is one contiguous slice...
        assert_eq!(
            chunks[0],
            (
                "Read:Virtual[\n  - (1, 'alice')\n  - => id:i64]\n".to_string(),
                1,
                3,
            ),
        );
        // ...and the following line starts a fresh chunk at the right line number.
        assert_eq!(chunks[1], ("Next".to_string(), 4, 4));
    }

    #[test]
    fn carriage_returns_are_stripped_from_line_content_but_kept_in_chunk() {
        let cursor = ChunkCursor::new("a\r\nb", 1).unwrap();
        let line = cursor.peek_line().unwrap();
        // Content has the \r stripped...
        assert_eq!(line.as_str(), "a");
        // ...but merging keeps the original bytes in the chunk slice.
        let mut c = cursor;
        c.merge(line);
        let (chunk, _) = c.next();
        assert_eq!(chunk, "a\r\n");
    }

    #[test]
    fn line_numbers_account_for_a_custom_start() {
        // A cursor can start at an arbitrary line number (e.g. mid-file).
        let chunks = collect("x\ny", |_, _| false)
            .into_iter()
            .map(|(_, s, e)| (s, e))
            .collect::<Vec<_>>();
        assert_eq!(chunks, vec![(1, 1), (2, 2)]);

        let mut cursor = ChunkCursor::new("x\ny", 10);
        let mut starts = Vec::new();
        while let Some(mut c) = cursor {
            c.merge(c.peek_line().unwrap());
            starts.push(c.start_line_no());
            cursor = c.next().1;
        }
        assert_eq!(starts, vec![10, 11]);
    }

    #[test]
    fn end_line_no_tracks_merged_lines() {
        let text = "a\nb\nc\nd";
        // Merge everything into one chunk.
        let chunks = collect(text, |_, _| true);
        assert_eq!(chunks.len(), 1);
        assert_eq!(chunks[0].1, 1); // start
        assert_eq!(chunks[0].2, 4); // end
        assert_eq!(chunks[0].0, "a\nb\nc\nd");
    }
}
