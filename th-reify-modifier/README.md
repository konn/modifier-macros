# th-reify-modifier

```haskell
import Language.Haskell.TH.Modifier (reifyModifier)
-- reifyModifier :: Name -> Q [Type]
```

Enable `-fplugin=Language.Haskell.TH.Modifier.Plugin` in a datatype's defining
module. Query datatype names (`''T`), constructor names (`'MkT`) or record-field
names (`'field`) from Template Haskell. Use a declaration-group boundary before
queries in the current module. Imported queries work from interface files,
including with an external TH interpreter; the consumer does not need a plugin.

The result preserves all modifier kinds, their order and duplicates. A shared
record field combines the modifiers of each occurrence. Missing capture fails
with an actionable message; a captured unmodified entity returns `[]`.

The `modifier-generics` plugin also captures these annotations, so users of both
packages need only that plugin. See the
[repository README](https://github.com/konn/modifier-macros#readme) for examples
and the pinned GHC prerelease requirements.
