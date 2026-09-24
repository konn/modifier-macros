# GHC modifier experiments

A `cabal-scaffold` monorepo for the experimental `Modifiers` extension in
**GHC 10.0.0.20260917**. The compiler API dependencies are pinned to this
prerelease; CI uses Cabal 3.18.1.0.

| Package | Purpose |
| --- | --- |
| [`modifier-generics`](modifier-generics/) | `Generic` / `Generic1` with modifier lists in datatype, constructor and selector metadata |
| [`th-reify-modifier`](th-reify-modifier/) | TH modifier queries and `th-abstraction`-based datatype, constructor and field information |
| [`modifier-common`](modifier-common/) | Shared compiler traversal, heterogeneous metadata and persistent TH annotations |
| [`modifier-macros`](modifier-macros/) | Original field-modifier-based `Semigroup` / `Monoid` experiment |

Each directory is a separate Cabal package, with its own license, README and
changelog. The root `cabal.project` discovers `*/*.cabal`.
`cabal-scaffold.yaml` selects `monorepo-single` for future packages.

## Modifier-aware generics

```haskell
{-# LANGUAGE Modifiers #-}
{-# OPTIONS_GHC -fplugin=Generics.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

import GHC.Generics qualified as Stock
import Generics.Modifier qualified as M

%"entity" %True
data Example a
  = %"empty" Empty
  | %"record" Record { payload %"json-name" %42 :: a }
  deriving (Stock.Generic, Stock.Generic1)
```

Use `M.from`, `M.to`, `M.from1`, and `M.to1`. Stock deriving supplies the
structural representation; the plugin supplies its modifiers. No second
explicit deriving clause is needed. Both records and positional constructors,
sums, products, empty datatypes, newtypes and stock-derivable GADTs work.
`Rep1` retains `Par1`, `Rec1` and composition from stock `Generic1`.

`M.D1`, `M.C1` and `M.S1` wrap metadata with one extra slot:

```haskell
M.MetaData name moduleName packageName isNewtype modifiers
M.MetaCons name fixity isRecord modifiers
M.MetaSel name unpackedness strictness decidedStrictness modifiers
```

The modifier slot has kind `[Modifier]`. `Mod` existentially packages each
modifier's kind, so a single list can contain, for example,
`'[Mod "label", Mod True, Mod 42, Mod Maybe, Mod (Eq Int)]`.
`Meta` and `Modifier` use `type data`: their constructors are written without
promotion ticks. Modifiers retain their source order and duplicates. Unmodified
entities have `[]`; grouped record fields receive the same modifier list.
Standard queries such as `datatypeName`, `conName` and `selName` remain usable.

For `Rep (Example Int)`, type parameters in modifiers are instantiated as usual.
`Rep1 Example` has no concrete last argument: its metadata uses the polykinded
symbolic type `Parameter`, just as its values use `Par1`. Thus a field modifier
`Maybe a` becomes `Mod (Maybe Parameter)` in `Rep1`; it is **not discarded**.
The remaining datatype parameters retain their actual types.

## Template Haskell reification

Enable either capture plugin in the **defining module**:

```haskell
{-# LANGUAGE DataKinds, Modifiers, TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Language.Haskell.TH.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

import Language.Haskell.TH
import Language.Haskell.TH.Modifier

%"table"
data Row = %"row" Row { value %"column" %True :: Int }

$(pure [])  -- normal TH declaration-group boundary

-- Within Q:
-- reifyModifier ''Row  ==> [LitT (StrTyLit "table")]
-- reifyModifier 'Row   ==> [LitT (StrTyLit "row")]
-- reifyModifier 'value ==> [LitT (StrTyLit "column"), PromotedT ...True...]
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
  type beyond a single explicitly identified multiplicity. Unsupported syntax is rejected explicitly, not pretty-printed into a
  lossy approximation. Top-level heterogeneous modifier lists are supported.
- GHC requires `-fno-external-interpreter` when loading compiler plugins. An
  importing TH consumer without plugins can use `-fexternal-interpreter`.
- This GHC's experimental modifier syntax and scoping rules remain applicable.

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
