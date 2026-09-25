{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Modifiers #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fplugin=Language.Haskell.TH.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}
-- Shadow deliberately repeats binder names to exercise capture avoidance.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module DatatypeFixtures (Wrapped (..), Empty, (:*:) (..), Indexed (..), Existential (..), Grouped (..), Kinded (..), StrictFields (..), Shadow (..), Nullary (..), Alias) where

import Data.Kind (Type)
import Data.Proxy (Proxy)

%"newtype"
newtype Wrapped a = %"wrap" Wrapped {unWrapped %(Maybe a) :: a}

%"empty"
data Empty

infixr 5 :*:
data a :*: b = a :*: b

data Indexed a where
  %"indexed" %(Maybe b) Indexed :: forall b. Show b => {indexValue %(Maybe b) :: b} -> Indexed b
  %"refined" Refined :: Int %"number" %True %"number" -> Indexed Bool

data Existential a = forall b. Show b => Exists {hidden %(Maybe b) :: b, visible %(Maybe a) :: a}

data Grouped where
  %"group" GroupA, GroupB :: {groupLeft, groupRight %42 %False :: Int} -> Grouped

%(Proxy a)
data Kinded (a :: k) = Kinded {kindedValue %(Proxy a) :: Proxy a}

data StrictFields = StrictFields {strictField %"strict" :: {-# UNPACK #-} !Int, lazyField :: ~Bool}

data Shadow a where
  Shadow :: forall a. Show a => a %(Maybe a) %((forall a. a -> a) :: Type) -> Shadow Int

data Nullary a where
  %(Maybe b) Nullary :: Nullary b

type Alias = Wrapped Int
