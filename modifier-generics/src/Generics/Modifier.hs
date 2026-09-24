{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Modifier-aware counterparts of 'GHC.Generics.Generic' and
'GHC.Generics.Generic1'. Derive the stock classes explicitly, then use
@deriving anyclass (Generic, Generic1)@ and enable
@-fplugin=Generics.Modifier.Plugin@ in the defining module.
-}
module Generics.Modifier (
  Generic (..),
  Generic1 (..),
  Meta (..),
  M1 (..),
  D1,
  C1,
  S1,
  Modifier (..),
  ModifierMetadata,
  Parameter,
  module G,
) where

import Data.Kind (Type)
import GHC.Generics as G hiding (C1, D1, Generic (..), Generic1 (..), M1 (..), Meta (..), S1)
import GHC.Generics qualified as GHC
import GHC.Modifiers.Types
import GHC.TypeLits

-- | Standard metadata with an additional heterogeneous modifier list.
type data Meta
  = MetaData Symbol Symbol Symbol Bool [Modifier]
  | MetaCons Symbol FixityI Bool [Modifier]
  | MetaSel (Maybe Symbol) SourceUnpackedness SourceStrictness DecidedStrictness [Modifier]

-- | A metadata wrapper with the same runtime representation as stock 'GHC.M1'.
newtype M1 i (m :: Meta) (f :: k -> Type) (p :: k) = M1 {unM1 :: f p}
  deriving stock (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

-- | Datatype metadata wrapper.
type D1 = M1 D

-- | Constructor metadata wrapper.
type C1 = M1 C

-- | Selector metadata wrapper.
type S1 = M1 S

instance (Datatype ('GHC.MetaData n m p nt)) => Datatype (MetaData n m p nt ms) where
  datatypeName _ = datatypeName (undefined :: GHC.D1 ('GHC.MetaData n m p nt) U1 ())
  moduleName _ = moduleName (undefined :: GHC.D1 ('GHC.MetaData n m p nt) U1 ())
  packageName _ = packageName (undefined :: GHC.D1 ('GHC.MetaData n m p nt) U1 ())
  isNewtype _ = isNewtype (undefined :: GHC.D1 ('GHC.MetaData n m p nt) U1 ())

instance (Constructor ('GHC.MetaCons n f r)) => Constructor (MetaCons n f r ms) where
  conName _ = conName (undefined :: GHC.C1 ('GHC.MetaCons n f r) U1 ())
  conFixity _ = conFixity (undefined :: GHC.C1 ('GHC.MetaCons n f r) U1 ())
  conIsRecord _ = conIsRecord (undefined :: GHC.C1 ('GHC.MetaCons n f r) U1 ())

instance (Selector ('GHC.MetaSel n u s d)) => Selector (MetaSel n u s d ms) where
  selName _ = selName (undefined :: GHC.S1 ('GHC.MetaSel n u s d) U1 ())
  selSourceUnpackedness _ = selSourceUnpackedness (undefined :: GHC.S1 ('GHC.MetaSel n u s d) U1 ())
  selSourceStrictness _ = selSourceStrictness (undefined :: GHC.S1 ('GHC.MetaSel n u s d) U1 ())
  selDecidedStrictness _ = selDecidedStrictness (undefined :: GHC.S1 ('GHC.MetaSel n u s d) U1 ())

{- | Modifier-aware generic conversion, explicitly opted into with
@deriving anyclass Generic@. The defaults require an explicitly provided
'GHC.Generic' instance; the plugin supplies modifier metadata only.
Hand-written instances may override the representation and conversions.
-}
class Generic a where
  type Rep a :: Type -> Type
  type Rep a = Decorate (ModifierMetadata a) (GHC.Rep a)

  from :: a -> Rep a p
  default from :: (GHC.Generic a, Convert (GHC.Rep a) (Rep a)) => a -> Rep a p
  from = decorate . GHC.from
  {-# INLINE from #-}

  to :: Rep a p -> a
  default to :: (GHC.Generic a, Convert (GHC.Rep a) (Rep a)) => Rep a p -> a
  to = GHC.to . undecorate
  {-# INLINE to #-}

{- | A symbolic parameter in 'Rep1' metadata, analogous to @Par1@ in fields.
For example, a field modifier @Maybe a@ becomes @Mod (Maybe Parameter)@.
'Rep' and TH reification retain the actual parameter instead.
-}
type family Parameter :: k

{- | Polykinded generic conversion for a type constructor, opted into with
@deriving anyclass Generic1@ independently of 'Generic'. The defaults require
an explicitly provided 'GHC.Generic1' instance.
-}
class Generic1 (f :: k -> Type) where
  type Rep1 f :: k -> Type
  type Rep1 f = Decorate (ModifierMetadata (f Parameter)) (GHC.Rep1 f)

  from1 :: f a -> Rep1 f a
  default from1 :: (GHC.Generic1 f, Convert (GHC.Rep1 f) (Rep1 f)) => f a -> Rep1 f a
  from1 = decorate . GHC.from1
  {-# INLINE from1 #-}

  to1 :: Rep1 f a -> f a
  default to1 :: (GHC.Generic1 f, Convert (GHC.Rep1 f) (Rep1 f)) => Rep1 f a -> f a
  to1 = GHC.to1 . undecorate
  {-# INLINE to1 #-}

type family AddModifiers (m :: GHC.Meta) (ms :: [Modifier]) :: Meta where
  AddModifiers ('GHC.MetaData n m p nt) ms = MetaData n m p nt ms
  AddModifiers ('GHC.MetaCons n f r) ms = MetaCons n f r ms
  AddModifiers ('GHC.MetaSel n u s d) ms = MetaSel n u s d ms

type family Decorate (info :: Metadata) (f :: k -> Type) :: k -> Type where
  Decorate '(ms, cs) (GHC.D1 m f) = D1 (AddModifiers m ms) (DecorateCons cs f)

type family DecorateCons cs f where
  DecorateCons cs (l :+: r) = DecorateCons cs l :+: DecorateCons cs r
  DecorateCons cs (GHC.C1 ('GHC.MetaCons n fix rec) f) = DecorateCon (FindCon n cs) ('GHC.MetaCons n fix rec) f
  DecorateCons cs V1 = V1

type family FindCon n cs where
  FindCon n ('(n, ms, fs) ': cs) = '(ms, fs)
  FindCon n (c ': cs) = FindCon n cs
  FindCon n '[] = TypeError ('Text "Missing modifier metadata for constructor " ':<>: 'ShowType n)

type family DecorateCon info m f where
  DecorateCon '(ms, fs) m f = C1 (AddModifiers m ms) (DecorateFields 0 fs f)

type family DecorateFields (n :: Nat) (fs :: [[Modifier]]) f where
  DecorateFields n fs (l :*: r) = DecorateFields n fs l :*: DecorateFields (n + FieldCount l) fs r
  DecorateFields n fs (GHC.S1 m f) = S1 (AddModifiers m (At n fs)) f
  DecorateFields n fs U1 = U1

type family FieldCount f :: Nat where
  FieldCount (l :*: r) = FieldCount l + FieldCount r
  FieldCount (GHC.S1 m f) = 1
  FieldCount U1 = 0

type family At (n :: Nat) (xs :: [k]) :: k where
  At 0 (x ': xs) = x
  At n (x ': xs) = At (n - 1) xs
  At n '[] = TypeError ('Text "Missing modifier metadata for field " ':<>: 'ShowType n)

-- Structural conversion preserves the stock representation's branching,
-- recursive positions, composition and unboxed fields.
class Convert f g where
  decorate :: f a -> g a
  undecorate :: g a -> f a

instance (Convert f g) => Convert (GHC.M1 i m f) (M1 i m' g) where
  decorate (GHC.M1 x) = M1 (decorate x)
  undecorate (M1 x) = GHC.M1 (undecorate x)

instance (Convert l l', Convert r r') => Convert (l :+: r) (l' :+: r') where
  decorate (L1 x) = L1 (decorate x)
  decorate (R1 x) = R1 (decorate x)
  undecorate (L1 x) = L1 (undecorate x)
  undecorate (R1 x) = R1 (undecorate x)

instance (Convert l l', Convert r r') => Convert (l :*: r) (l' :*: r') where
  decorate (x :*: y) = decorate x :*: decorate y
  undecorate (x :*: y) = undecorate x :*: undecorate y

instance Convert (K1 i c) (K1 i c) where
  decorate = id
  undecorate = id

instance Convert U1 U1 where
  decorate = id
  undecorate = id

instance Convert V1 V1 where
  decorate x = case x of {}
  undecorate x = case x of {}

instance Convert Par1 Par1 where
  decorate = id
  undecorate = id

instance Convert (Rec1 f) (Rec1 f) where
  decorate = id
  undecorate = id

instance Convert (f :.: g) (f :.: g) where
  decorate = id
  undecorate = id

instance Convert (URec a) (URec a) where
  decorate = id
  undecorate = id
