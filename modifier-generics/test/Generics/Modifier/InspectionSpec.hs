{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O2 #-}

module Generics.Modifier.InspectionSpec (test_optimized, roundTrip, roundTrip1, identity, genericField, directField, genericMap, directMap, modifierDirectedField) where

import Data.Proxy (Proxy (..))
import Fixture
import Generics.Modifier
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

roundTrip :: Example Bool -> Example Bool
roundTrip = to . from

roundTrip1 :: Example Bool -> Example Bool
roundTrip1 = to1 . from1

identity :: Example Bool -> Example Bool
identity x = x

-- A consumer of a modifier-bearing field, beyond a conversion round trip.
genericField :: Positional -> Int
genericField x = case from x of
  M1 (M1 (M1 (K1 n) :*: _)) -> n + 1

-- Read a value selected by the modifier list, then use it with a field.
class ModifierIncrement (ms :: [Modifier]) where
  modifierIncrement :: proxy ms -> Int

instance ModifierIncrement '[Mod "left", Mod True] where
  modifierIncrement _ = 1
  {-# INLINE modifierIncrement #-}

selectorIncrement :: forall ms n u s d f p. (ModifierIncrement ms) => S1 (MetaSel n u s d ms) f p -> Int
selectorIncrement _ = modifierIncrement (Proxy @ms)
{-# INLINE selectorIncrement #-}

modifierDirectedField :: Positional -> Int
modifierDirectedField x = case (from x :: Rep Positional ()) of
  M1 (M1 (left@(M1 (K1 n)) :*: _)) -> n + selectorIncrement left

directField :: Positional -> Int
directField (Positional n _) = n + 1

genericMap :: (a -> b) -> Wrapped a -> Wrapped b
genericMap f x = case from1 x of
  M1 (M1 (M1 (Par1 a))) -> to1 (M1 (M1 (M1 (Par1 (f a)))))

directMap :: (a -> b) -> Wrapped a -> Wrapped b
directMap f (Wrapped a) = Wrapped (f a)

test_optimized :: TestTree
test_optimized =
  testGroup
    "optimized Core"
    [ $(inspectTest $ 'roundTrip === 'identity)
    , $(inspectTest $ 'roundTrip1 === 'identity)
    , $(inspectTest $ 'genericField === 'directField)
    , $(inspectTest $ 'genericMap === 'directMap)
    , $(inspectTest $ 'modifierDirectedField === 'directField)
    , $(inspectTest $ hasNoTypeClasses 'modifierDirectedField)
    , $(inspectTest $ hasNoTypes 'modifierDirectedField [''M1, ''K1, ''(:*:)])
    , $(inspectTest $ hasNoTypeClasses 'genericField)
    , $(inspectTest $ hasNoTypeClasses 'genericMap)
    , $(inspectTest $ hasNoTypes 'genericField [''M1, ''K1, ''(:*:)])
    , $(inspectTest $ hasNoTypes 'genericMap [''M1, ''Par1])
    ]
