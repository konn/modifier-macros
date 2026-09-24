# modifier-macros - An experiment around using GHC 10 Modifiers to mimic attribute macro

This module explores the possibility to achieve something like Rust's attribute macro in Haskell, using `Modifiers` extension introduced in GHC 10.

This package provides a compiler plugin to collect field modifiers from parsed sources and save their representation as type family.

The example usage deriving `Monoid` instance by specifying monoid implementation as `%(..)` modifier on fields:

```haskell
{-# LANGUAGE Modifiers, DerivingVia #-}
{-# OPTIONS_GHC -fplugin Data.Monoid.Deriving.Modifiers #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}
module Main (main) where

import Data.Monoid.Deriving.Modifiers
import Data.Monoid
import GHC.Generics

data Foo a = Foo
  { total %(Sum Int) :: !Int
  , power %(Product Int) :: !Int
  , children :: [Foo a]
  , backwards %(Dual [String]):: [String]
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (Semigroup, Monoid) via ModifiersOn (Foo a)

foo0 = Foo { total = 0, power = 1, children = [], backwards = []}
foo1 = Foo { total = 1, power = 2, children = [foo0], backwards = ["foo1"]}
foo2 = Foo { total = 3, power = 4, children = [foo1], backwards = ["foo2"]}

>>> foo1 <> foo 2
Foo
  { total = 4, -- Sum: 1 + 3
    power = 8, -- Product: 2 * 4

    -- list monoid: [foo0] <> [foo1]
    children =
      [ Foo {total = 0, power = 1, children = [], backwards = []}
      , Foo {total = 1, power = 2, children = [Foo {total = 0, power = 1, children = [], backwards = []}], backwards = ["foo1"]}],

    -- list monoid, but *dual* order:
    backwards = ["foo2","foo1"]
  }
```

See [`app/Main.hs`](./app/Main.hs) for more example usage.
