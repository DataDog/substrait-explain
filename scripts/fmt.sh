#!/bin/sh
# fmt.sh - Checks project formatting using cargo fmt.

# Ensure cargo is available
if ! command -v cargo > /dev/null; then
  echo "fmt.sh: Error - cargo not found in PATH." >&2
  exit 127 # Command not found
fi

CMD="cargo fmt -- --check"
echo "fmt.sh: Running '$CMD'..."

$CMD
RESULT=$?

if [ $RESULT -ne 0 ]; then
  echo "fmt.sh: Error - '$CMD' found formatting issues." >&2
  echo "fmt.sh: Please run 'cargo fmt' to format the project and then re-stage the changes." >&2
  exit 1
fi

echo "fmt.sh: '$CMD' passed. Project formatting is correct."
exit 0