# modifier-common

Shared infrastructure for `modifier-generics` and `th-reify-modifier`:

- `GHC.Modifiers`: traversal of renamed datatype, constructor and field syntax,
  including sums, GADT constructor groups and grouped record selectors.
- `GHC.Modifiers.TH`: structural conversion to TH `Type`, retaining name identity.
- `GHC.Modifiers.Types`: heterogeneous `Modifier` packaging, the
  `ModifierMetadata` family, and serialized annotation payloads for individual
  names and each constructor's field occurrences.

Capture occurs after renaming. Annotations are added both to the current TH
annotation environment and to the interface payload, with idempotent insertion
when both public plugins run. No compiler `HsType` escapes into the TH API.

This is compiler infrastructure tied to GHC 10.0.0.20260917. Most applications
should depend on one of the public packages instead.
