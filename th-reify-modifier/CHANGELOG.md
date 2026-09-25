# Changelog

## 0.1.0.0

Initial experimental release for GHC 10 modifiers.

- Add `reifyDatatype`, `reifyConstructor`, and modifier-aware datatype,
  constructor and field records, reusing `th-abstraction` normalization and types.
- Preserve per-occurrence modifiers on shared record labels and positional fields.
- Convert shared modifier syntax to TH at reification time. TH limitations no
  longer prevent capture or generic derivation; recompile defining modules to
  produce the new neutral annotation format.
