# GHC modifier experiments

A `cabal-scaffold` monorepo for the experimental `Modifiers` extension in
**GHC 10.0.0.20260917**. The compiler API dependencies are pinned to this
prerelease; CI uses Cabal 3.18.1.0.

| Package | Purpose |
| --- | --- |
| [`modifier-generics`](modifier-generics/) | `Generic` / `Generic1` with modifier lists in datatype, constructor and selector metadata |
| [`th-reify-modifier`](th-reify-modifier/) | TH modifier queries and `th-abstraction`-based datatype, constructor and field information |
| [`modifier-common`](modifier-common/) | Shared compiler traversal and persistent, consumer-independent modifier syntax |
| [`modifier-macros`](modifier-macros/) | Original field-modifier-based `Semigroup` / `Monoid` experiment |

Each directory is a separate Cabal package, with its own license, README and
changelog. The root `cabal.project` discovers `*/*.cabal`.
`cabal-scaffold.yaml` selects `monorepo-single` for future packages.

## Modifier-aware generics

```haskell
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies, Modifiers #-}
{-# OPTIONS_GHC -fplugin=Generics.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

import Data.Kind (Type)
import GHC.Generics qualified as GHC
import Generics.Modifier qualified as M

data Entity
data Json
data EncodeWith (codec :: Type)
data EmptyTag
data RecordTag

%Entity
data Document a
  = %EmptyTag EmptyDocument
  | %RecordTag Document { payload %(EncodeWith Json) %(Maybe a) %Maybe :: a }
  deriving stock (GHC.Generic, GHC.Generic1)
  deriving anyclass (M.Generic, M.Generic1)
```

Use `M.from`, `M.to`, `M.from1`, and `M.to1`. Each class is an explicit opt-in;
derive either or both with `anyclass`, explicitly providing the corresponding
stock instances used by the class defaults. The plugin supplies modifier
metadata only; it never adds class instances or changes deriving clauses.
Inline and standalone deriving work.
There are no blanket `Generic` or `Generic1` instances, so hand-written
representations can coexist with derived ones. Records and positional constructors,
sums, products, empty datatypes, newtypes and stock-derivable GADTs work.
`Rep1` retains `Par1`, `Rec1` and composition from stock `Generic1`.

`M.D1`, `M.C1` and `M.S1` wrap metadata with one extra slot:

```haskell
M.MetaData name moduleName packageName isNewtype modifiers
M.MetaCons name fixity isRecord modifiers
M.MetaSel name unpackedness strictness decidedStrictness modifiers
```

Modifiers are ordinary types: user-defined tags such as `Entity`, applications
such as `EncodeWith Json` and `Maybe a`, and constructors such as `Maybe` are all
valid. An annotation records metadata; consumers decide how to interpret it.
The modifier slot has kind `[Modifier]`. `Mod` existentially packages each
modifier's kind, so a single list can contain, for example,
`'[Mod Entity, Mod (EncodeWith Json), Mod Maybe, Mod (Eq Int)]`.
`Meta` and `Modifier` use `type data`: their constructors are written without
promotion ticks. Modifiers retain their source order and duplicates. Unmodified
entities have `[]`; grouped record fields receive the same modifier list.
Standard queries such as `datatypeName`, `conName` and `selName` remain usable.

For `Rep (Document Int)`, type parameters in modifiers are instantiated as usual.
`Rep1 Document` has no concrete last argument: its metadata uses the polykinded
symbolic type `Parameter`, just as its values use `Par1`. Thus a field modifier
`Maybe a` becomes `Mod (Maybe Parameter)` in `Rep1`; it is **not discarded**.
The remaining datatype parameters retain their actual types.

## Template Haskell reification

Enable either capture plugin in the **defining module**:

```haskell
{-# LANGUAGE Modifiers, TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Language.Haskell.TH.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

import Language.Haskell.TH
import Language.Haskell.TH.Modifier
import Data.Monoid (Sum)

data Table
data RowTag
data Json
data EncodeWith codec

%Table
data Row = %RowTag Row { value %(EncodeWith Json) %(Sum Int) :: Int }

$(pure [])  -- normal TH declaration-group boundary

-- Within Q:
-- reifyModifier ''Row  ==> [ConT ''Table]
-- reifyModifier 'Row   ==> [ConT ''RowTag]
-- reifyModifier 'value ==> [ AppT (ConT ''EncodeWith) (ConT ''Json)
--                         , AppT (ConT ''Sum) (ConT ''Int) ]
```

Results are ordinary Template Haskell `Type` values, with names retaining their
package, module and namespace. Free type variables are aligned with the binders
returned by ordinary `reify`, including after loading an interface. The plugin persists annotations in `.hi` files;
consumers can reify imported declarations without the capture plugin or source
files. The two plugins can be enabled together without duplicating annotations.
No global mutable registry or `unsafeCoerce` is used.

An inspected entity with no modifiers returns `[]`. Missing capture produces an
error explaining how to enable it, rather than silently returning `[]`. A record
selector shared by several constructors returns all its occurrences' modifiers
in constructor order. Positional fields have no `Name`; their modifiers are
available through generic selector metadata and the structured TH API.

`reifyDatatype :: Name -> Q DatatypeInfo` and
`reifyConstructor :: Name -> Q ConstructorInfo` provide a structured view using
`th-abstraction` for normalization. `DatatypeInfo` and `ConstructorInfo` carry
`datatypeModifiers` and `constructorModifiers`. Each entry in `constructorFields`
is a `FieldInfo` with a label (if present), type, strictness, and `fieldModifiers`.
This keeps shared selectors separate by constructor and includes positional
fields. GADT substitutions apply to modifiers as well as field types. See the
[package README](th-reify-modifier/README.md) for the API and supported declarations.

## Compiler boundaries

- Generic derivation has stock GHC's restrictions (for example, existential or
  genuinely indexed GADTs cannot derive stock `Generic`). The TH-only plugin
  does not require generic instances.
- This API captures datatype declarations, data constructors and record fields.
- TH cannot represent modifier annotations *inside* a modifier's function-arrow
  type beyond a single explicitly identified multiplicity. Shared capture retains
  this syntax; TH reification rejects it when queried. Generic derivation does
  not inherit TH's restrictions. Top-level heterogeneous modifier lists are supported.
- GHC requires `-fno-external-interpreter` when loading compiler plugins. An
  importing TH consumer without plugins can use `-fexternal-interpreter`.
- This GHC's experimental modifier syntax and scoping rules remain applicable.

`modifier-common` owns the shared syntax and annotation persistence, with no
direct dependency on `template-haskell`. The TH package converts that syntax to
`TH.Type` when reifying; `modifier-generics` owns the type-level `Modifier` and
`ModifierMetadata` definitions and generates its own metadata. Both plugins
capture the same neutral annotations. Recompile defining modules after this
annotation-format change; older TH payloads are not treated as neutral syntax.

## Development and validation

```sh
cabal build modifier-common
cabal build th-reify-modifier
cabal build modifier-generics
cabal build modifier-macros
cabal test all --test-show-details=direct
cabal run modifier-macros-exe
bash ci/scripts/cabal-check-packages.sh
cabal sdist all
```

Tests include compile-time representation equalities, local and imported TH
reification, and the original monoid behavior. TH fixtures are compiled as a
separate library and consumed using the external interpreter, exercising
interface-file persistence. `falsify` checks generic conversion laws on generated
sums, records and recursive values. `tasty-inspection-testing` compares optimized
Core against hand-written code and checks that generic wrappers and dictionaries
are eliminated in representative consumers.

Format source with a Fourmolu version supporting the syntax in the file, and run
`cabal-gild --io <package>/<package>.cabal` after changing package metadata.
Older Fourmolu releases reject the `Modifiers` extension; the modifier fixtures
must be maintained manually until such a formatter is available.
