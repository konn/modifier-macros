# Repository Guidelines

## Project Structure & Module Organization

This is a cabal-scaffold monorepo experimenting with GHC 10 modifiers.

- `modifier-common/`: shared modifier collection and consumer-independent syntax annotations. Keep TH conversion and generic metadata in their respective consumer packages.
- `modifier-generics/`: explicitly derived modifier-aware `Generic` and `Generic1`, with a metadata capture plugin. Class instances must be requested explicitly; the plugin must not add other class derivations.
- `th-reify-modifier/`: modifier queries, structured reification using `th-abstraction`, and capture plugin.
- `modifier-macros/`: original `Semigroup`/`Monoid` derivation experiment and examples.
- Each package has its own `.cabal`, `src/`, README and license; tests live in `test/`.
- Root `cabal.project`, `cabal.project.freeze`, `cabal-scaffold.yaml`: shared settings.
- `.github/workflows/haskell.yml`, `ci/scripts/`: CI and artifact handling.

## Build, Test, and Development Commands

Match CI's GHC `10.0.0.20260917` prerelease and Cabal `3.18.1.0`. Run from the repository root:

- `cabal update`: refresh the package index.
- `cabal build all`: compile all enabled components.
- `cabal run modifier-macros-exe`: run examples.
- `cabal test all --test-show-details=direct`: run tests.
- `bash ci/scripts/cabal-check-packages.sh`: validate every package.

Keep local configuration in ignored `cabal.project.local`.

## Coding Style & Naming Conventions

Use two-space indentation, `GHC2024`, explicit exports, and the checked-in `fourmolu.yaml` (leading commas, spaced record braces). Use `UpperCamelCase` for modules/types and `lowerCamelCase` for functions.

Prefer `(<>)` over `(++)`, including lists and strings; prefer `pure` over `return`. Address compiler warnings.
Use `GHC` as the qualifier for `GHC.Generics`.

Format changes with `fourmolu -i <file.hs>` and `cabal-gild --io <package>/<package>.cabal`. Use formatters supporting the project's syntax. After editing `package.yaml`, if introduced, run `hpack`.

## Testing Guidelines

Use Tasty with `tasty-discover`; add regressions under each package's `test/` with behavior-focused names. Use `falsify` for property tests and `tasty-inspection-testing` for optimization assertions. Cover record/positional modifiers, unmodified fields, and monoid identity. Declare additional testing dependencies in the Cabal test stanza. Keep compile-time metadata equalities and imported/local TH reification tests, including the separate fixture library and external-interpreter consumer.

Mirror the tested library's module hierarchy: tests for `Generics.Modifier` belong in `test/Generics/ModifierSpec.hs`. Larger specs may be split beneath the corresponding module namespace, such as `Generics.Modifier.InspectionSpec`; avoid names that collide with another library module's spec.

## Agent Tools

This guide applies to Codex and Claude Code; `CLAUDE.md` imports it.

`.codex/config.toml` and `.claude/settings.json` declare the upstream marketplaces and enable all five plugins from [haskell-claude-marketplace](https://github.com/konn/haskell-claude-marketplace) and its [Hoogle dependency](https://github.com/m4dc4p/claude-hoogle). Keep external skills in the clients' caches, outside this repository. Update both clients' pinned revisions together. See [Codex setup](.codex/README.md) for first-time downloads and prerequisites.

Use the Haskell skill, local Haddock/Hoogle lookup, and formatting tools. Plugin hooks require client support and trust; format explicitly when unavailable. Use client HLS tools when available, otherwise Cabal diagnostics.

## Commit & Pull Request Guidelines

Use concise imperative subjects, following `Fix CI test job and artifact handoff`. PRs should explain changes, link relevant issues, and report validation.

Changelogs describe the final changes since the last released version. Consolidate unreleased work into release-facing entries; omit intermediate implementation history and migration notes for designs that were never released. For an initial release, use a single “Initial release.” entry, not a feature inventory.

Every commit must include both trailers, replacing placeholders with the actual agent identity and public attribution email:

```text
Assisted-by: <agent/model>
Co-authored-by: <agent name> <public email>
```

Never include `Codex-Session:`, internal session links, or private metadata. Run `git add`/`git commit` only when explicitly requested; `git push` requires an explicit push request.
