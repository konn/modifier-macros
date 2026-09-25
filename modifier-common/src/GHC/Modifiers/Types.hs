{-# LANGUAGE DeriveDataTypeable #-}

{- | Consumer-independent syntax persisted by modifier capture. This module
depends only on base; it contains neither TH types nor generic representations.
-}
module GHC.Modifiers.Types (
  ModifierName (..),
  NameIdentity (..),
  Namespace (..),
  ModifierType (..),
  TupleSort (..),
  Arrow (..),
  Binder (..),
  BinderVisibility (..),
  ModifierSyntaxAnnotation (..),
  ConstructorFieldsSyntaxAnnotation (..),
) where

import Data.Data (Data)

-- | A resolved name, with namespace and identity kept separately from spelling.
data ModifierName = ModifierName Namespace NameIdentity
  deriving (Eq, Ord, Show, Data)

{- | Global names retain their defining unit and module; local names retain
their unique, so shadowed binders are distinct even when spellings coincide.
-}
data NameIdentity
  = GlobalName String String String
  | LocalName String Integer
  deriving (Eq, Ord, Show, Data)

-- | Record fields retain their parent constructor as part of their namespace.
data Namespace
  = TypeVariable
  | TypeConstructor
  | DataConstructor
  | Value
  | Field String
  deriving (Eq, Ord, Show, Data)

{- | Renamed type syntax, without imposing any consumer's representational
restrictions. Arrow modifiers, their order and duplicates are retained.
Parentheses are preserved; documentation and expanded splices are transparent.
-}
data ModifierType
  = NamedType ModifierName
  | AppliedType ModifierType ModifierType
  | KindAppliedType ModifierType ModifierType
  | InfixType ModifierType ModifierType ModifierType
  | ParenthesizedType ModifierType
  | KindSignature ModifierType ModifierType
  | ImplicitParameter String ModifierType
  | ListType ModifierType
  | TupleType TupleSort [ModifierType]
  | SumType [ModifierType]
  | PromotedList [ModifierType]
  | PromotedTuple [ModifierType]
  | StringType String
  | CharacterType Char
  | NaturalType Integer
  | StarType
  | WildcardType
  | QualifiedType [ModifierType] ModifierType
  | ForallType [Binder] ModifierType
  | VisibleForallType [Binder] ModifierType
  | FunctionType Arrow [ModifierType] ModifierType ModifierType
  deriving (Eq, Show, Data)

-- | Boxed tuple syntax may also denote a constraint tuple before kindchecking.
data TupleSort = BoxedOrConstraintTuple | UnboxedTuple
  deriving (Eq, Show, Data)

-- | The written arrow, independently of any modifiers attached to it.
data Arrow = StandardArrow | LinearArrow
  deriving (Eq, Show, Data)

{- | A binder's identity, optional kind and visibility. Nothing denotes a
wildcard binder; consumers decide whether they can represent it.
-}
data Binder = Binder (Maybe ModifierName) (Maybe ModifierType) BinderVisibility
  deriving (Eq, Show, Data)

{- | Required binders use visible forall syntax; invisible binders may be
specified or inferred.
-}
data BinderVisibility = Required | Specified | Inferred
  deriving (Eq, Show, Data)

{- | Per-entity modifier syntax. An empty list records successful capture.
This distinct payload type cannot be confused with the former TH annotations.
-}
newtype ModifierSyntaxAnnotation = ModifierSyntaxAnnotation [ModifierType]
  deriving (Eq, Show, Data)

{- | One list per field occurrence of a constructor, including positional
fields. Shared record labels remain separate; nullary constructors use [].
-}
newtype ConstructorFieldsSyntaxAnnotation = ConstructorFieldsSyntaxAnnotation [[ModifierType]]
  deriving (Eq, Show, Data)
