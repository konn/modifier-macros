# th-reify-modifier

```haskell
import Language.Haskell.TH.Modifier
-- reifyModifier :: Name -> Q [Type]
-- reifyDatatype :: Name -> Q DatatypeInfo
-- reifyConstructor :: Name -> Q ConstructorInfo
```

Enable `-fplugin=Language.Haskell.TH.Modifier.Plugin` in a datatype's defining
module. Query datatype names (`''T`), constructor names (`'MkT`) or record-field
names (`'field`) from Template Haskell. Use a declaration-group boundary before
queries in the current module. Imported queries work from interface files,
including with an external TH interpreter; the consumer does not need a plugin.

The result preserves all modifier kinds, their order and duplicates. A shared
record field combines the modifiers of each occurrence. Missing capture fails
with an actionable message; a captured unmodified entity returns `[]`.

For a structured view, `reifyDatatype` accepts the same datatype, constructor,
and record-selector names as `th-abstraction`'s `reifyDatatype`:

```haskell
-- Inside Q, for a captured datatype named Row:
info <- reifyDatatype ''Row
let declarationModifiers = datatypeModifiers info
    constructors = datatypeCons info
    perConstructor =
      [ ( constructorName con
        , constructorModifiers con
        , [(fieldName field, fieldType field, fieldModifiers field)
          | field <- constructorFields con]
        )
      | con <- constructors
      ]
```

`DatatypeInfo` follows `th-abstraction`'s metadata structure, with the additional
`datatypeModifiers :: [Type]`. `ConstructorInfo` adds
`constructorModifiers :: [Type]` and carries `[FieldInfo]` in `constructorFields`.
Each `FieldInfo` contains `fieldName :: Maybe Name`, `fieldType :: Type`,
`fieldStrictness :: FieldStrictness`, and `fieldModifiers :: [Type]`. Record labels
work with `NoFieldSelectors`; positional fields have `Nothing` for their name.
Shared selectors keep separate modifier lists for each constructor occurrence.

Normalization uses `th-abstraction`'s `reifyDatatype` and `normalizeCon`.
`DatatypeVariant`, `ConstructorVariant`, and the strictness types are reused from
that package. GADT refinements become equality constraints, and only existential
binders remain in `constructorVars`. Modifier types undergo the same renaming
and substitution as field types, so they refer to the returned datatype and
constructor binders. Strictness lives on each field instead of in a parallel
`constructorStrictness` list.

The structured API supports ordinary `data` and `newtype` declarations, including
empty datatypes, infix constructors, existentials, and GADTs. Capture of data family
instances and `type data` is not supported. Recompile defining modules with the
updated plugin to obtain the field-occurrence annotations required by this API.

Both plugins persist the same consumer-independent syntax from `modifier-common`.
This package converts it to TH only when queried, before applying
`th-abstraction` normalization. Syntax TH cannot express (such as arbitrary
modifiers inside an arrow type) is rejected during reification, not during
capture. Recompile defining modules when upgrading from the former TH payloads.

The `modifier-generics` plugin also captures these annotations, so users of both
packages need only that plugin. See the
[repository README](https://github.com/konn/modifier-macros#readme) for examples
and the pinned GHC prerelease requirements.
