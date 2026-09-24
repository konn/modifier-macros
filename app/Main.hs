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

data Bar where
  Bar ::
    !Int %(Sum Int) ->
    !Int %(Product Int) -> 
    [Bool] %(Dual [Bool]) ->
    (Maybe Bar) %(First Bar) ->
    Bar
  deriving (Show, Eq, Ord, Generic)
  deriving (Semigroup, Monoid) via ModifiersOn Bar

main :: IO ()
main = do
  let foo0 = Foo { total = 0, power = 1, children = [], backwards = []}
      foo1 = Foo { total = 1, power = 2, children = [foo0], backwards = ["foo1"]}
      foo2 = Foo { total = 3, power = 4, children = [foo1], backwards = ["foo2"]}
      foo12 = foo1 <> foo2
      expectedFoo12 = Foo 
        { total = 4
        , power = 8
        , children = [foo0, foo1]
        , backwards = ["foo2", "foo1"]
        }
  print $ foo12
  print $ 
    foo12 == expectedFoo12
