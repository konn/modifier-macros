{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

-- | Compiler-independent metadata shared by the plugins and their consumers.
module GHC.Modifiers.Types (
  Modifier (..),
  Metadata,
  ModifierMetadata,
  ModifierAnnotation (..),
  ConstructorFieldsAnnotation (..),
) where

import Data.Data (Data)
import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Language.Haskell.TH.Syntax qualified as TH

{- | An existential kind wrapper. For example, @'[Mod Int, Mod True,
Mod "label"]@ contains modifiers of three different kinds.
-}
type data Modifier :: Type where
  Mod :: k -> Modifier

{- | Datatype modifiers, followed by constructor names, constructor modifiers,
and the modifiers of each field in declaration order.
-}
type Metadata = ([Modifier], [(Symbol, [Modifier], [[Modifier]])])

-- | Populated by the generic plugin. Parameters remain in scope on the RHS.
type family ModifierMetadata (a :: k) :: Metadata

{- | A distinct annotation payload, persisted in interface files. An empty
payload records that an entity was inspected and has no modifiers.
-}
newtype ModifierAnnotation = ModifierAnnotation [TH.Type]
  deriving (Data)

{- | Modifiers for each field of one constructor, in declaration order.
Unlike selector annotations, this includes positional fields and keeps shared
record labels separate. The payload is empty for a nullary constructor.
-}
newtype ConstructorFieldsAnnotation = ConstructorFieldsAnnotation [[TH.Type]]
  deriving (Data)
