{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Modifiers #-}
{-# OPTIONS_GHC -fplugin=Generics.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

module Fixture (Example (..), Positional (..), Wrapped (..), Tree (..), Gadt (..), Empty, (:^:) (..), Dependent (..), KindZoo (..), Phantom (..), Linear (..), ParamGadt (..)) where

import Data.Monoid (Sum)
import GHC.Generics qualified as GHC
import Generics.Modifier (Generic, Generic1)

%"datatype" %True %(Sum Int)
data Example a
  = %"none" None
  | %False %"some" Some
      { first %"first" %True %(Sum Int) %"first" :: Int
      , second, third %42 %False :: a
      }
  | More (Maybe [a])
  deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)
  deriving anyclass (Generic, Generic1)

data Positional where
  %"positional" Positional :: Int %"left" %True -> Bool %42 -> Positional
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass Generic

newtype Wrapped a = Wrapped {unwrap %"wrapped" :: a}
  deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)
  deriving anyclass (Generic, Generic1)

data Tree a = Leaf a | Branch [Tree a]
  deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)
  deriving anyclass (Generic, Generic1)

data Gadt where
  %"group" GadtA, GadtB :: Int %"field" -> Gadt
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass Generic

data Empty
  deriving stock GHC.Generic
  deriving anyclass Generic

infixr 5 :^:
data a :^: b = a :^: b deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)
  deriving anyclass (Generic, Generic1)

data Dependent a = Dependent {dependent %(Maybe a) :: a}
  deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)
  deriving anyclass (Generic, Generic1)

%Maybe %Eq %(Eq Int) %('Just "tag") %'x' %'[True, False]
data KindZoo = KindZoo deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass Generic

data Phantom (a :: k) = Phantom deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)
  deriving anyclass (Generic, Generic1)

data Linear where
  Linear :: Int ⊸ Linear
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass Generic

data ParamGadt a where
  ParamGadt :: {paramField %(Maybe b) :: b} -> ParamGadt b
  deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)
  deriving anyclass (Generic, Generic1)
