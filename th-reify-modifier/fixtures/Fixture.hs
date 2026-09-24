{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Modifiers #-}
{-# LANGUAGE LinearTypes #-}
{-# OPTIONS_GHC -fplugin=Language.Haskell.TH.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

module Fixture (Tagged (..), Plain (..), Positional (..), Gadt (..), Shared (..)) where

import Data.Monoid (Sum)

%"type" %True %(Sum Int) %"type"
data Tagged a = %"constructor" %False Tagged
  { tagged %"field" %42 %(Maybe a) :: a
  , untouched :: Bool
  }

data Plain = Plain

data Positional where
  %1 %"linear constructor" Positional :: Int %1 %"field" -> Positional

data Gadt where
  %"group" GadtA, GadtB :: Gadt

data Shared
  = First {shared %"first" :: Int}
  | Second {shared %"second" :: Int}
