{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Modifiers #-}
{-# OPTIONS_GHC -fplugin=Data.Monoid.Deriving.Modifiers #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

module Data.Monoid.Deriving.ModifiersSpec (test_originalExperiment) where

import Data.Monoid
import Data.Monoid.Deriving.Modifiers
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

data Record = Record
  { total %(Sum Int) :: Int
  , power %(Product Int) :: Int
  , unchanged :: [Bool]
  , backwards %(Dual [Int]) :: [Int]
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via ModifiersOn Record

data Positional where
  Positional :: Int %(Sum Int) -> [Bool] -> Positional
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via ModifiersOn Positional

test_originalExperiment :: TestTree
test_originalExperiment = testGroup "original monoid experiment"
  [ testCase "record modifiers and unmodified fields" $
      Record 2 3 [True] [1] <> Record 4 5 [False] [2] @?= Record 6 15 [True, False] [2, 1]
  , testCase "positional modifiers and unmodified fields" $
      Positional 2 [True] <> Positional 3 [False] @?= Positional 5 [True, False]
  , testCase "record identity" $ do
      let x = Record 2 3 [True] [1]
      mempty <> x @?= x
      x <> mempty @?= x
  , testCase "positional identity" $ do
      let x = Positional 2 [True]
      mempty <> x @?= x
      x <> mempty @?= x
  ]
