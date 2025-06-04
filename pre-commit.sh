#!/bin/bash
set -eu

# This script can be run in two ways:
# 1. ./pre-commit.sh install: This will create a symlink in
#    .git/hooks/pre-commit pointing to this script.
# 2. Git will run it automatically before commits if it's linked in .git/hooks.

SCRIPT_NAME="pre-commit.sh" # This script's name, assumed to be in the repo root.
HOOK_DIR=".git/hooks"
HOOK_PATH="$HOOK_DIR/pre-commit"

# Relative path from the hook's location (.git/hooks/pre-commit) to this script in the root.
# From .git/hooks/pre-commit, to get to the root, we go up two levels (../..),
# then access SCRIPT_NAME.
RELATIVE_SCRIPT_PATH="../../$SCRIPT_NAME"

# Handle the 'install' command
if [ -n "${1:-}" ] && [ "$1" = "install" ]; then
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
  else
    echo "ERROR: Pre-commit hook installation failed."
    echo "Check if $HOOK_PATH was created and is executable, and points to the correct target."
    exit 1
  fi
  exit 0
fi

# --- Pre-commit checks start here ---
# (This part runs when Git executes the hook)

SCRIPT_DIR=$(dirname "$0") # Gets the directory of this script, should be the repo root

# Check formatting for the entire project
COMMAND="cargo fmt -- --check"
echo "\$ $COMMAND"
$COMMAND
RESULT=$?

if [ $RESULT -ne 0 ]; then
  echo "Pre-commit: Formatting check failed."
  exit 1
fi

# Run cargo clippy with warnings as errors
COMMAND="cargo clippy -- -D warnings"
echo "\$ $COMMAND"
$COMMAND
RESULT=$?

if [ $RESULT -ne 0 ]; then
  echo "clippy: Error - '$CMD' failed. Please fix the issues and try again."
  exit 1
fi

echo "pre-commit: success"
exit 0