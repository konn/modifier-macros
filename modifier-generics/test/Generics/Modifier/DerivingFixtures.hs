{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Modifiers #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Generics.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

module Generics.Modifier.DerivingFixtures (
  OnlyGeneric (..), OnlyGeneric1 (..), ExistingBoth (..), Standalone (..),
  ExistingStandalone (..), InlineWithStandaloneStock (..), StockOnly (..),
  Unrequested (..), Manual (..), SameName (..), EarlierStock (..), EarlierPlain (..),
  ImportedStock (..), Nested (..), StandaloneNested (..),
  Entity, Json, EncodeWith, EmptyTag, RecordTag, Document (..), NestedAnnotation (..),
) where

import Data.Kind (Type)
import GHC.Generics qualified as GHC
import Generics.Modifier qualified as M

data OnlyGeneric a = OnlyGeneric a
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass M.Generic

data OnlyGeneric1 a = OnlyGeneric1 a
  deriving stock (Eq, Show, GHC.Generic1)
  deriving anyclass M.Generic1

data ExistingBoth a = ExistingBoth a
  deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)
  deriving anyclass (M.Generic, M.Generic1)

data Standalone a = Standalone a deriving stock (Eq, Show)
deriving stock instance GHC.Generic (Standalone a)
deriving stock instance GHC.Generic1 Standalone
deriving anyclass instance M.Generic (Standalone a)
deriving anyclass instance M.Generic1 Standalone

data ExistingStandalone a = ExistingStandalone a
  deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)
deriving anyclass instance M.Generic (ExistingStandalone a)
deriving anyclass instance M.Generic1 ExistingStandalone

data InlineWithStandaloneStock a = InlineWithStandaloneStock a
  deriving stock (Eq, Show)
  deriving anyclass (M.Generic, M.Generic1)
deriving stock instance GHC.Generic (InlineWithStandaloneStock a)
deriving stock instance GHC.Generic1 InlineWithStandaloneStock

data StockOnly a = StockOnly a deriving stock (GHC.Generic, GHC.Generic1)
data ImportedStock a = ImportedStock a
  deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)
data Unrequested a = Unrequested a

data Nested f a = Nested (f (Maybe a))
  deriving stock (GHC.Generic, GHC.Generic1)
  deriving anyclass (M.Generic, M.Generic1)

data StandaloneNested f a = StandaloneNested (f (Maybe a))
deriving stock instance Functor f => GHC.Generic1 (StandaloneNested f)
deriving anyclass instance Functor f => M.Generic1 (StandaloneNested f)

data Manual a = Manual a
  deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)

instance M.Generic (Manual a) where
  type Rep (Manual a) = M.K1 M.R a
  from (Manual a) = M.K1 a
  to (M.K1 a) = Manual a

instance M.Generic1 Manual where
  type Rep1 Manual = M.Par1
  from1 (Manual a) = M.Par1 a
  to1 (M.Par1 a) = Manual a

class Generic a
data SameName = SameName deriving anyclass Generic

-- These declarations also appear in the public examples. Modifiers are
-- ordinary types, including user-defined tags and type applications.
data Entity
data Json
data EncodeWith (codec :: Type)
data EmptyTag
data RecordTag

%Entity
data Document a
  = %EmptyTag EmptyDocument
  | %RecordTag Document {payload %(EncodeWith Json) %(Maybe a) %Maybe :: a}
  deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)
  deriving anyclass (M.Generic, M.Generic1)

data EarlierStock a = EarlierStock a
  deriving stock (Eq, Show, GHC.Generic, GHC.Generic1)
data EarlierPlain a = EarlierPlain a deriving stock (Eq, Show)

$(pure [])

deriving anyclass instance M.Generic (EarlierStock a)
deriving anyclass instance M.Generic1 EarlierStock
deriving stock instance GHC.Generic (EarlierPlain a)
deriving stock instance GHC.Generic1 EarlierPlain
deriving anyclass instance M.Generic (EarlierPlain a)
deriving anyclass instance M.Generic1 EarlierPlain

-- Generic derivation must not be limited by TH type syntax.
data NestedAnnotation = NestedAnnotation
  { annotatedArrow %(Int %Entity %Entity -> Bool) :: Int }
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass M.Generic
