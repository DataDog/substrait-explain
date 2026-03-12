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

### 2. CI: release-plz creates (or updates) a Release PR

On every push to `main`, the **Release PR** job runs. It:

- Scans all commits since the last release tag
- Determines the version bump from conventional commit prefixes
- Creates a PR (or updates an existing one) that changes:
  - `Cargo.toml` — version bump
  - `Cargo.lock` — updated lockfile
  - `CHANGELOG.md` — new entry with grouped commit messages

The PR is labeled `release` for easy identification.

> **Note**: CI checks (fmt, lint, test) don't automatically run on this PR because
> it's created by `GITHUB_TOKEN` (a GitHub limitation to prevent infinite loops).
> This is acceptable because the PR only modifies version/changelog files, not source
> code. See [Known Limitations](#ci-checks-dont-run-automatically-on-release-prs)
> for workarounds.

### 3. You review and merge the Release PR

Review the PR like any other:

- Check that the version bump makes sense (minor vs patch vs major)
- Review the generated changelog entry
- Optionally edit the changelog before merging if you want to reword anything
- Merge when satisfied

### 4. CI: release-plz publishes the release

On the push to `main` from the merged Release PR, the **Release** job runs. It:

- Compares the `Cargo.toml` version to the latest version on crates.io
- Since the version is now newer, it:
  - Runs `cargo publish` to publish to **crates.io**
  - Creates a **git tag** (e.g., `v0.3.0`)
  - Creates a **GitHub Release** with the changelog entry as the release notes

### 5. Done!

The new version is live on [crates.io](https://crates.io/crates/substrait-explain)
and visible on the [GitHub Releases](https://github.com/DataDog/substrait-explain/releases)
page.

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

# Create a git tag
git tag -a v<VERSION> -m "Release v<VERSION>"
git push origin v<VERSION>

# Create a GitHub release
gh release create v<VERSION> --title "v<VERSION>" --notes "See CHANGELOG.md"
```

## Configuration

| File                                | Purpose                                                  |
| ----------------------------------- | -------------------------------------------------------- |
| `.github/workflows/release-plz.yml` | GitHub Actions workflow (two jobs: release + release-pr) |
| `release-plz.toml`                  | release-plz behavior and changelog format                |
| `CHANGELOG.md`                      | Auto-updated changelog                                   |

### Secrets

- `CARGO_REGISTRY_TOKEN`: crates.io API token with `publish-new` and `publish-update`
  scopes. Configured in the repository's Actions secrets.

## Known Limitations

### CI checks don't run automatically on release PRs

Release PRs are created using the built-in `GITHUB_TOKEN`, which means they don't
trigger other workflows (this is a GitHub limitation to prevent infinite loops). The
existing Rust CI checks (fmt, lint, test) won't run automatically on release PRs.

**Why this is acceptable**: Release PRs only modify `Cargo.toml`, `Cargo.lock`, and
`CHANGELOG.md` — no source code changes. The risk of breakage is low.

**Workarounds if needed**:

- Push an empty commit to the release PR branch to trigger CI
- Manually trigger the Rust CI workflow from the Actions tab
- Upgrade to a [GitHub App token](https://release-plz.dev/docs/github/trigger) for
  full automation (the recommended long-term solution if branch protection requires
  passing CI to merge)

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

- Check that commits since the last release include `feat:` or `fix:` prefixes
  (commits without these prefixes don't trigger version bumps)
- Verify the workflow ran: Actions tab → "Release-plz"
- Check workflow logs for errors

### crates.io publish failed

- Verify `CARGO_REGISTRY_TOKEN` secret is set and not expired
- Check that the token has `publish-new` and `publish-update` scopes
- Verify crate ownership on crates.io: `cargo owner --list`

### Workflow doesn't run at all

- Ensure `release-plz/action@*` is in the repository's Actions allow-list
  (Settings → Actions → General → "Allow select actions")
