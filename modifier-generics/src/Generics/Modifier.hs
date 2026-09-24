{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Modifier-aware counterparts of 'GHC.Generics.Generic' and
'GHC.Generics.Generic1'. Derive the stock classes and enable
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
import GHC.Generics qualified as Std
import GHC.Modifiers.Types
import GHC.TypeLits

-- | Standard metadata with an additional heterogeneous modifier list.
type data Meta
  = MetaData Symbol Symbol Symbol Bool [Modifier]
  | MetaCons Symbol FixityI Bool [Modifier]
  | MetaSel (Maybe Symbol) SourceUnpackedness SourceStrictness DecidedStrictness [Modifier]

-- | A metadata wrapper with the same runtime representation as stock 'Std.M1'.
newtype M1 i (m :: Meta) (f :: k -> Type) (p :: k) = M1 {unM1 :: f p}
  deriving stock (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

-- | Datatype metadata wrapper.
type D1 = M1 D

-- | Constructor metadata wrapper.
type C1 = M1 C

-- | Selector metadata wrapper.
type S1 = M1 S

instance (Datatype ('Std.MetaData n m p nt)) => Datatype (MetaData n m p nt ms) where
  datatypeName _ = datatypeName (undefined :: Std.D1 ('Std.MetaData n m p nt) U1 ())
  moduleName _ = moduleName (undefined :: Std.D1 ('Std.MetaData n m p nt) U1 ())
  packageName _ = packageName (undefined :: Std.D1 ('Std.MetaData n m p nt) U1 ())
  isNewtype _ = isNewtype (undefined :: Std.D1 ('Std.MetaData n m p nt) U1 ())

instance (Constructor ('Std.MetaCons n f r)) => Constructor (MetaCons n f r ms) where
  conName _ = conName (undefined :: Std.C1 ('Std.MetaCons n f r) U1 ())
  conFixity _ = conFixity (undefined :: Std.C1 ('Std.MetaCons n f r) U1 ())
  conIsRecord _ = conIsRecord (undefined :: Std.C1 ('Std.MetaCons n f r) U1 ())

instance (Selector ('Std.MetaSel n u s d)) => Selector (MetaSel n u s d ms) where
  selName _ = selName (undefined :: Std.S1 ('Std.MetaSel n u s d) U1 ())
  selSourceUnpackedness _ = selSourceUnpackedness (undefined :: Std.S1 ('Std.MetaSel n u s d) U1 ())
  selSourceStrictness _ = selSourceStrictness (undefined :: Std.S1 ('Std.MetaSel n u s d) U1 ())
  selDecidedStrictness _ = selDecidedStrictness (undefined :: Std.S1 ('Std.MetaSel n u s d) U1 ())

{- | Modifier-aware generic conversion. Stock deriving supplies the value-level
representation; the plugin supplies the modifier metadata.
-}
class Generic a where
  type Rep a :: Type -> Type
  from :: a -> Rep a p
  to :: Rep a p -> a

instance (Std.Generic a, Convert (Std.Rep a) (Decorate (ModifierMetadata a) (Std.Rep a))) => Generic a where
  type Rep a = Decorate (ModifierMetadata a) (Std.Rep a)
  from = decorate . Std.from
  to = Std.to . undecorate

{- | A symbolic parameter in 'Rep1' metadata, analogous to @Par1@ in fields.
For example, a field modifier @Maybe a@ becomes @Mod (Maybe Parameter)@.
'Rep' and TH reification retain the actual parameter instead.
-}
type family Parameter :: k

-- | Polykinded generic conversion for a type constructor.
class Generic1 (f :: k -> Type) where
  type Rep1 f :: k -> Type
  from1 :: f a -> Rep1 f a
  to1 :: Rep1 f a -> f a

instance (Std.Generic1 f, Convert (Std.Rep1 f) (Decorate (ModifierMetadata (f Parameter)) (Std.Rep1 f))) => Generic1 f where
  type Rep1 f = Decorate (ModifierMetadata (f Parameter)) (Std.Rep1 f)
  from1 = decorate . Std.from1
  to1 = Std.to1 . undecorate

type family AddModifiers (m :: Std.Meta) (ms :: [Modifier]) :: Meta where
  AddModifiers ('Std.MetaData n m p nt) ms = MetaData n m p nt ms
  AddModifiers ('Std.MetaCons n f r) ms = MetaCons n f r ms
  AddModifiers ('Std.MetaSel n u s d) ms = MetaSel n u s d ms

type family Decorate (info :: Metadata) (f :: k -> Type) :: k -> Type where
  Decorate '(ms, cs) (Std.D1 m f) = D1 (AddModifiers m ms) (DecorateCons cs f)

type family DecorateCons cs f where
  DecorateCons cs (l :+: r) = DecorateCons cs l :+: DecorateCons cs r
  DecorateCons cs (Std.C1 ('Std.MetaCons n fix rec) f) = DecorateCon (FindCon n cs) ('Std.MetaCons n fix rec) f
  DecorateCons cs V1 = V1

type family FindCon n cs where
  FindCon n ('(n, ms, fs) ': cs) = '(ms, fs)
  FindCon n (c ': cs) = FindCon n cs
  FindCon n '[] = TypeError ('Text "Missing modifier metadata for constructor " ':<>: 'ShowType n)

type family DecorateCon info m f where
  DecorateCon '(ms, fs) m f = C1 (AddModifiers m ms) (DecorateFields 0 fs f)

type family DecorateFields (n :: Nat) (fs :: [[Modifier]]) f where
  DecorateFields n fs (l :*: r) = DecorateFields n fs l :*: DecorateFields (n + FieldCount l) fs r
  DecorateFields n fs (Std.S1 m f) = S1 (AddModifiers m (At n fs)) f
  DecorateFields n fs U1 = U1

type family FieldCount f :: Nat where
  FieldCount (l :*: r) = FieldCount l + FieldCount r
  FieldCount (Std.S1 m f) = 1
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

instance (Convert f g) => Convert (Std.M1 i m f) (M1 i m' g) where
  decorate (Std.M1 x) = M1 (decorate x)
  undecorate (M1 x) = Std.M1 (undecorate x)

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
