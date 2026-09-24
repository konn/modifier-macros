# modifier-generics

Modifier-aware `Generic` and polykinded `Generic1`, with explicit anyclass
derivation and stock generic machinery reused internally.

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

Derive either class independently or both together, explicitly providing the
corresponding stock instances required by the default methods. Inline and
standalone deriving are supported. The plugin supplies modifier metadata only;
it never adds class instances or changes deriving clauses. Stock deriving alone
does not create modifier-generic instances, and anyclass deriving alone does not
create stock instances.
There are no blanket implementations, and hand-written instances can supply their
own `Rep`/`Rep1` and conversions.

Modifiers are arbitrary types. The example combines user-defined marker types,
type applications and a higher-kinded type constructor. Consumers assign their
meaning. Import `Generics.Modifier` for the classes, representations, conversions
and representation constructors. `MetaData`, `MetaCons` and `MetaSel` each add a
`[Modifier]` slot. `Mod` packages modifiers of arbitrary kinds without filtering,
reordering or deduplication.

`Rep1` metadata uses the polykinded symbolic `Parameter` for the datatype's final
argument. Other parameters are instantiated normally. The plugin also captures
annotations usable by `th-reify-modifier`.

See the [repository README](https://github.com/konn/modifier-macros#readme) for
examples, compiler requirements and boundaries. Tests cover sums, products,
newtypes, positional and grouped record fields, GADTs, parameterized modifiers,
explicit opt-in, hand-written instances, inline and standalone deriving,
and optimized generic consumers. Property tests use `falsify`; Core assertions
use `tasty-inspection-testing`.
