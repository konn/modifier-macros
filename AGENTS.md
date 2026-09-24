# Repository Guidelines

## Project Structure & Module Organization

This GHC plugin derives `Semigroup` and `Monoid` through field modifiers and `ModifiersOn`.

- `src/Data/Monoid/Deriving/Modifiers.hs`: compiler plugin.
- `src/Data/Monoid/Deriving/Modifiers/Types.hs`: generic derivation machinery.
- `app/Main.hs`: executable examples.
- `test/Main.hs`: Tasty discovery entry point; currently no test cases.
- `modifier-macros.cabal`, `cabal.project`, `cabal.project.freeze`: components, settings, dependency constraints.
- `.github/workflows/haskell.yml`, `ci/scripts/`: CI and artifact handling.

## Build, Test, and Development Commands

Match CI's GHC `10.0.0.20260917` prerelease and Cabal `3.18.1.0`. Run from the repository root:

- `cabal update`: refresh the package index.
- `cabal build all`: compile all enabled components.
- `cabal run modifier-macros-exe`: run examples.
- `cabal test all --test-show-details=direct`: run tests.
- `cabal check`: validate package metadata.

Keep local configuration in ignored `cabal.project.local`.

## Coding Style & Naming Conventions

Use two-space indentation, `GHC2024`, explicit exports, and the checked-in `fourmolu.yaml` (leading commas, spaced record braces). Use `UpperCamelCase` for modules/types and `lowerCamelCase` for functions.

Prefer `(<>)` over `(++)`, including lists and strings; prefer `pure` over `return`. Address compiler warnings.

Format changes with `fourmolu -i <file.hs>` and `cabal-gild --io modifier-macros.cabal`. Use formatters supporting the project's syntax. After editing `package.yaml`, if introduced, run `hpack`.

## Testing Guidelines

Use Tasty with `tasty-discover`; add regressions under `test/` with behavior-focused names. Cover record/positional modifiers, unmodified fields, and monoid identity. Declare additional testing dependencies in the Cabal test stanza. No coverage threshold exists; an empty suite proves no behavior.

## Agent Tools

This guide applies to Codex and Claude Code; `CLAUDE.md` imports it.

`.codex/config.toml` and `.claude/settings.json` declare the upstream marketplaces and enable all five plugins from [haskell-claude-marketplace](https://github.com/konn/haskell-claude-marketplace) and its [Hoogle dependency](https://github.com/m4dc4p/claude-hoogle). Keep external skills in the clients' caches, outside this repository. Update both clients' pinned revisions together. See [Codex setup](.codex/README.md) for first-time downloads and prerequisites.

Use the Haskell skill, local Haddock/Hoogle lookup, and formatting tools. Plugin hooks require client support and trust; format explicitly when unavailable. Use client HLS tools when available, otherwise Cabal diagnostics.

## Commit & Pull Request Guidelines

Use concise imperative subjects, following `Fix CI test job and artifact handoff`. PRs should explain changes, link relevant issues, and report validation.

Every commit must include both trailers, replacing placeholders with the actual agent identity and public attribution email:

```text
Assisted-by: <agent/model>
Co-authored-by: <agent name> <public email>
```

Never include `Codex-Session:`, internal session links, or private metadata. Run `git add`/`git commit` only when explicitly requested; `git push` requires an explicit push request.
