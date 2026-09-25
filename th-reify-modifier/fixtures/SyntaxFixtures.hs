{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE Modifiers #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fplugin=Language.Haskell.TH.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

module SyntaxFixtures (ArrowTag, NestedModifiers (..), FieldOnly (..), TypeForms) where

import Data.Kind (Type)
import Data.Proxy (Proxy)

data ArrowTag

-- Capture must succeed even though TH cannot express these nested modifiers.
%(Int %ArrowTag %ArrowTag -> Bool)
data NestedModifiers = NestedModifiers
  { nestedField %(Int %ArrowTag -> Bool) :: Int }

data FieldOnly = FieldOnly { onlyNested %(Int %ArrowTag -> Bool) :: Int }

%([Int]) %(Int, Bool) %'(Int, Bool) %'[Int, Bool]
%(Proxy @Type Int) %(forall (a :: Type). Eq a => a -> a)
%(forall a -> a -> a) %(Int %1 -> Bool) %(Int ⊸ Bool)
data TypeForms
