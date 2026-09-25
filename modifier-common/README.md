# modifier-common

Shared infrastructure for `modifier-generics` and `th-reify-modifier`:

- `GHC.Modifiers`: traversal of renamed datatype, constructor and field syntax,
  including sums, GADT constructor groups and grouped record selectors.
- `GHC.Modifiers.Syntax`: capture into consumer-independent type syntax,
  retaining global unit/module/namespace identity and local binder uniques.
- `GHC.Modifiers.Types`: the shared syntax and serializable annotation payloads
  for individual names and each constructor's field occurrences. This module
  depends only on `base`.

Capture occurs after renaming. Annotations are added both to the current compiler
annotation environment and to the interface payload, with idempotent insertion
when both public plugins run. Arrow modifiers, binder visibility and kinds,
source order, duplicates, and positional fields survive serialization.

The package does not import Template Haskell or depend directly on
`template-haskell`. It does not generate generic representations or type-level
metadata. Consumers produce those results themselves: `th-reify-modifier`
lowers syntax to TH at reification time, and `modifier-generics` owns its
type-level metadata and generation. Capture can therefore retain nested arrow
modifiers even when TH cannot represent them.

The neutral annotation payloads have distinct types from the former TH payloads.
Recompile defining modules when upgrading to this format.

This is compiler infrastructure tied to GHC 10.0.0.20260917. Most applications
should depend on one of the public packages instead.
