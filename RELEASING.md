# Releasing substrait-explain

This project uses [release-plz](https://release-plz.dev/) for automated releases.

## Release Walkthrough

Here's what a typical release looks like, step by step:

### 1. You merge a PR to `main`

Business as usual. Use [conventional commit](https://www.conventionalcommits.org/)
messages on individual commits so release-plz knows how to bump the version.
(release-plz analyzes the commits that land on `main` — with merge commits, that's
each individual commit in the PR; with squash-and-merge, it's the PR title.)

| Commit prefix                  | Version bump          | Example                               |
| ------------------------------ | --------------------- | ------------------------------------- |
| `feat:`                        | Minor (0.2.0 → 0.3.0) | `feat: add Set relation support`      |
| `fix:`                         | Patch (0.2.0 → 0.2.1) | `fix: handle empty filter expression` |
| `feat!:` or `BREAKING CHANGE:` | Major (0.2.0 → 1.0.0) | `feat!: redesign extension API`       |
| `docs:`, `ci:`, `chore:`, etc. | No bump               | `docs: update GRAMMAR.md`             |

> **Note**: While below 1.0.0, breaking changes should still use `feat!:` or
> `BREAKING CHANGE:` so they appear prominently in the changelog.

### 2. You make a release PR

Steps, run locally:

1. \[Only once\] Install tools
   1. `brew install gh` - for Github access
   2. `gh auth login` - login to Github from CLI
   3. `cargo install --locked release-plz` - the CLI for making the Release PR
2. `git checkout main` - need to be on `main` for a release
3. `release-plz release-pr --git-token $(gh auth token)` - `release-plz` will analyze the current state, and generate a Changelog, version bump, and PR on Github.

### 3. You review and merge the Release PR

Review the PR like any other:

- Check that the version bump makes sense (minor vs patch vs major)
- Review the generated changelog entry
- Optionally edit the changelog before merging if you want to reword anything
- Merge when satisfied

### 4. CI: release-plz publishes to crates.io

On the push to `main` from the merged Release PR, the **Release** job runs. It:

- Compares the `Cargo.toml` version to the latest version on crates.io
- Since the version is now newer, runs `cargo publish` to publish to **crates.io**

### 5. (Optional) Tag and release on GitHub

Requires maintainer permissions (CI tokens cannot create tags due to
org-wide tag protection rulesets).

```bash
just release-tag
```

### 6. Done!

The new version is live on [crates.io](https://crates.io/crates/substrait-explain).

### What if I just merge docs/CI/chore commits?

Nothing happens — no release PR is created because those commit types don't trigger
version bumps. The commits accumulate until a `feat:` or `fix:` commit arrives.

## Changelog

The `CHANGELOG.md` is automatically updated by release-plz. Commit messages are
grouped by type (configured in `release-plz.toml`):

- **Features** (`feat:`)
- **Bug Fixes** (`fix:`)
- **Refactoring** (`refactor:`)
- **Documentation** (`docs:`)
- **Performance** (`perf:`)

`ci:` and `chore:` commits are excluded from the changelog.

## Manual Override

If you need to release manually (e.g., for a hotfix):

```bash
# Verify the package is ready
cargo publish --dry-run

# Publish to crates.io
cargo publish

# Optionally tag and release (requires maintainer permissions)
git tag v<VERSION>
git push origin v<VERSION>
gh release create v<VERSION> --title "v<VERSION>" --notes "See CHANGELOG.md"
```

## Configuration

| File                                | Purpose                                                  |
| ----------------------------------- | -------------------------------------------------------- |
| `.github/workflows/release-plz.yml` | GitHub Actions workflow (publish to crates.io)           |
| `release-plz.toml`                  | release-plz behavior and changelog format                |
| `CHANGELOG.md`                      | Auto-updated changelog                                   |

### Secrets

- `CARGO_REGISTRY_TOKEN`: crates.io API token with `publish-new` and `publish-update`
  scopes. Configured in the repository's Actions secrets.

## Known Limitations

### Release PRs must be created locally

release-plz supports [automated release PRs via CI](https://release-plz.dev/docs/github/quickstart),
but the DataDog GitHub org disables "Allow GitHub Actions to create and approve pull
requests" at the org level. This means the `release-pr` command can't run as a GitHub
Actions job. Instead, release PRs are created locally (see step 2 above).

If org settings change in the future, re-adding a `release-pr` job to the workflow
would fully automate the process.

### No CI Check for conventional commits

This is a TODO for later: enforce conventional commits for PR titles with a check. See [`substrait-java`](https://github.com/substrait-io/substrait-java/blob/fc27033dcf53a4309b30fb4072232ce812c43489/.github/workflows/pr_title.yaml) for an example of this.

### LICENSE-3rdparty.csv is not part of automated releases

The `generate_licenses.sh` script produces `LICENSE-3rdparty.csv`, which tracks
third-party license attributions. This file is **not** automatically updated by
release-plz.

When dependencies change (e.g., in a version bump), you should regenerate it:

```bash
just licenses
```

Consider running this before merging a release PR if `Cargo.lock` has changed
significantly, or as a periodic maintenance task.

## Troubleshooting

### Release PR not created

- Make sure you're on `main` (`git checkout main`) — release-plz uses the current
  branch as the PR base
- Check that commits since the last release include `feat:` or `fix:` prefixes
  (commits without these prefixes don't trigger version bumps)
- Verify your GitHub token works: `gh auth status`

### crates.io publish failed

- Verify `CARGO_REGISTRY_TOKEN` secret is set and not expired
- Check that the token has `publish-new` and `publish-update` scopes
- Verify crate ownership on crates.io: `cargo owner --list`

### Workflow doesn't run at all

- Ensure `release-plz/action@*` is in the repository's Actions allow-list
  (Settings → Actions → General → "Allow select actions")
