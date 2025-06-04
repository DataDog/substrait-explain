#!/bin/sh
# clippy.sh - Lints the project using cargo clippy.

# Ensure cargo is available
if ! command -v cargo > /dev/null; then
  echo "clippy.sh: Error - cargo not found in PATH." >&2
  exit 127 # Command not found
fi

CMD="cargo clippy -- -D warnings"

$CMD
RESULT=$?

if [ $RESULT -ne 0 ]; then
  echo "clippy.sh: Error - '$CMD' failed. Please fix the issues and try again." >&2
  exit 1
fi

exit 0