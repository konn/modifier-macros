{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Modifiers #-}
{-# OPTIONS_GHC -fplugin=Generics.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

module Generics.Modifier.MonoidFixtures (Record (..), Positional (..), Tagged (..), SemigroupOnly (..)) where

import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (All (..), Dual (..), Product (..), Sum (..))
import GHC.Generics qualified as GHC
import Generics.Modifier (Generic, MGenerically (..), MonoidVia)

data Record a = Record
  { total %(MonoidVia (Sum Int)) :: !Int
  , power %(MonoidVia (Product Int)) :: !Int
  , forwards :: [a]
  , backwards %(MonoidVia (Dual [a])) :: [a]
  }
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass Generic
  deriving (Semigroup, Monoid) via MGenerically (Record a)

data Positional where
  Positional :: Int %(MonoidVia (Sum Int)) -> Bool %(MonoidVia All) -> [Bool] -> Positional
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass Generic
  deriving (Semigroup, Monoid) via MGenerically Positional

data Tagged = Tagged
  { taggedTotal %"before" %(MonoidVia (Sum Int)) %True :: Int
  , taggedList %"unrelated" %42 :: [Int]
  }
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass Generic
  deriving (Semigroup, Monoid) via MGenerically Tagged

-- NonEmpty has no Monoid instance: deriving Semigroup must not require one.
data SemigroupOnly = SemigroupOnly
  { forwardNonEmpty :: NonEmpty Int
  , backwardNonEmpty %(MonoidVia (Dual (NonEmpty Int))) :: NonEmpty Int
  }
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass Generic
  deriving Semigroup via MGenerically SemigroupOnly
