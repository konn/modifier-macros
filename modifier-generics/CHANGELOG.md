# Changelog

## 0.1.0.0

Initial experimental release for GHC 10 modifiers.

- Replace blanket generic instances with explicit `deriving anyclass` and class
  defaults requiring explicitly provided stock instances. The plugin captures
  metadata without adding class instances or rewriting deriving clauses.
- Use ordinary user-defined types and type applications in the primary examples.
