#!/bin/sh

# This script can be run in two ways:
# 1. ./pre-commit.sh install - This will create a symlink in .git/hooks/pre-commit
#                             pointing to this script.
# 2. Git will run it automatically before commits if it's linked in .git/hooks.

SCRIPT_NAME="pre-commit.sh" # This script's name, assumed to be in the repo root.
HOOK_DIR=".git/hooks"
HOOK_PATH="$HOOK_DIR/pre-commit"

# Relative path from the hook's location (.git/hooks/pre-commit) to this script in the root.
# From .git/hooks/pre-commit, to get to the root, we go up two levels (../..),
# then access SCRIPT_NAME.
RELATIVE_SCRIPT_PATH="../../$SCRIPT_NAME"

# Handle the 'install' command
if [ "$1" = "install" ]; then
  echo "Installing pre-commit hook to $HOOK_PATH..."

  # Ensure the .git/hooks directory exists
  if [ ! -d .git ]; then
    echo "Error: .git directory not found. Are you in the root of a Git repository?"
    exit 1
  fi
  mkdir -p "$HOOK_DIR"

  # Create or update the symbolic link
  # -s: symbolic
  # -f: force (overwrite if exists)
  ln -sf "$RELATIVE_SCRIPT_PATH" "$HOOK_PATH"

  # Make this script executable (important if cloned without +x)
  chmod +x "$0" # $0 is the path by which this script was called

  # Make the hook itself (the symlink) executable
  chmod +x "$HOOK_PATH"

  # Verify installation
  if [ -L "$HOOK_PATH" ] && [ -x "$HOOK_PATH" ]; then
    REAL_TARGET=$(readlink "$HOOK_PATH")
    echo "Successfully installed pre-commit hook."
    echo "$HOOK_PATH is now a symlink to $REAL_TARGET"
    echo "Please commit $SCRIPT_NAME to your repository."
  else
    echo "ERROR: Pre-commit hook installation failed."
    echo "Check if $HOOK_PATH was created and is executable, and points to the correct target."
    exit 1
  fi
  exit 0
fi

# --- Pre-commit formatting logic starts here ---
# (This part runs when Git executes the hook)

# Find all staged .rs files
STAGED_RS_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.rs$')

if [ -z "$STAGED_RS_FILES" ]; then
  # No .rs files to format, exit successfully
  exit 0
fi

echo "Formatting staged Rust files with rustfmt..."

# Format each staged .rs file
# Ensure rustfmt is available
if ! command -v rustfmt > /dev/null; then
  echo "Error: rustfmt not found in PATH. Please install rustfmt."
  echo "You can usually install it with: rustup component add rustfmt"
  exit 1
fi

HAS_ERRORS=0
for FILE in $STAGED_RS_FILES; do
  if [ -f "$FILE" ]; then # Check if file exists, as it might have been deleted
    rustfmt "$FILE"
    if [ $? -ne 0 ]; then
      echo "Error formatting $FILE with rustfmt."
      HAS_ERRORS=1
    else
      # Add the formatted file back to the staging area
      git add "$FILE"
    fi
  fi
done

if [ $HAS_ERRORS -ne 0 ]; then
  echo "Some files could not be formatted. Please review the errors above."
  exit 1
fi

echo "Formatting complete."
exit 0