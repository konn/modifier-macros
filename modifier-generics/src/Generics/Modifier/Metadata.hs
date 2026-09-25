{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type-level metadata belonging to modifier-aware generic representations.
module Generics.Modifier.Metadata (Modifier (..), Metadata, ModifierMetadata) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

-- | Package modifiers of arbitrary kinds in a single type-level list.
type data Modifier :: Type where
  Mod :: k -> Modifier

{- | Datatype modifiers, then constructor names, constructor modifiers and
modifiers for each field in declaration order.
-}
type Metadata = ([Modifier], [(Symbol, [Modifier], [[Modifier]])])

-- | Populated by the generic plugin. Parameters remain in scope on the RHS.
type family ModifierMetadata (a :: k) :: Metadata
