# Changelog

## 0.1.0.0

Initial experimental release for GHC 10 modifiers.

- Persist each constructor's field modifiers separately for structured TH reification.
- Replace TH payloads with consumer-independent syntax, retaining nested arrow
  modifiers, name identity and binder structure. Remove the direct TH dependency
  and move generic metadata into `modifier-generics`. Defining modules must be
  recompiled to produce the new annotation format.
