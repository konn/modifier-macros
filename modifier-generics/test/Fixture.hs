{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Modifiers #-}
{-# OPTIONS_GHC -fplugin=Generics.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

module Fixture (Example (..), Positional (..), Wrapped (..), Tree (..), Gadt (..), Empty, (:^:) (..), Dependent (..), KindZoo (..), Phantom (..), Linear (..), ParamGadt (..)) where

import Data.Monoid (Sum)
import GHC.Generics (Generic, Generic1)

%"datatype" %True %(Sum Int)
data Example a
  = %"none" None
  | %False %"some" Some
      { first %"first" %True %(Sum Int) %"first" :: Int
      , second, third %42 %False :: a
      }
  | More (Maybe [a])
  deriving (Eq, Show, Generic, Generic1)

data Positional where
  %"positional" Positional :: Int %"left" %True -> Bool %42 -> Positional
  deriving (Eq, Show, Generic)

newtype Wrapped a = Wrapped {unwrap %"wrapped" :: a}
  deriving (Eq, Show, Generic, Generic1)

data Tree a = Leaf a | Branch [Tree a]
  deriving (Eq, Show, Generic, Generic1)

data Gadt where
  %"group" GadtA, GadtB :: Int %"field" -> Gadt
  deriving (Eq, Show, Generic)

data Empty deriving (Generic)

infixr 5 :^:
data a :^: b = a :^: b deriving (Eq, Show, Generic, Generic1)

data Dependent a = Dependent {dependent %(Maybe a) :: a}
  deriving (Eq, Show, Generic, Generic1)

%Maybe %Eq %(Eq Int) %('Just "tag") %'x' %'[True, False]
data KindZoo = KindZoo deriving (Eq, Show, Generic)

data Phantom (a :: k) = Phantom deriving (Eq, Show, Generic, Generic1)

data Linear where
  Linear :: Int ⊸ Linear
  deriving (Eq, Show, Generic)

data ParamGadt a where
  ParamGadt :: {paramField %(Maybe b) :: b} -> ParamGadt b
  deriving (Eq, Show, Generic, Generic1)
