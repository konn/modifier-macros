{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Monoid.Deriving.Modifiers.Types (
  ModifiersOn (..),
  GModifierSemigroup,
  GModifierMonoid,
) where

import Data.Coerce
import Data.Function
import Data.Kind
import GHC.Generics
import GHC.TypeLits

newtype ModifiersOn a = ModifiersOn {getModifiersOn :: a}

type data FieldReps
  = Positional [Type]
  | Record [(Symbol, Type)]

type family FieldRepsOf (a :: Type) :: FieldReps

instance (GModifierSemigroup a) => Semigroup (ModifiersOn a) where
  (<>) = fmap (ModifiersOn . to) . (gappend @(FieldRepsOf a) @(Rep a) `on` (from . getModifiersOn))
  {-# INLINE (<>) #-}

instance (GModifierMonoid a) => Monoid (ModifiersOn a) where
  mempty = ModifiersOn $ to $ gmempty @(FieldRepsOf a) @(Rep a)
  {-# INLINE mempty #-}

type GModifierSemigroup a = (Generic a, GMSem (FieldRepsOf a) (Rep a))

type GModifierMonoid a = (Generic a, GMMon (FieldRepsOf a) (Rep a))

type family Lookup f xs where
  Lookup x ('(x, y) ': xs) = Just y
  Lookup x ('(y, z) ': xs) = Lookup x xs
  Lookup x '[] = Nothing

type family Index n xs where
  Index 0 (x ': xs) = Just x
  Index n (x ': xs) = Index (n - 1) xs
  Index n '[] = Nothing

type GMSem :: FieldReps -> (Type -> Type) -> Constraint
class GMSem rs f where
  gappend :: f () -> f () -> f ()

instance {-# OVERLAPPING #-} (GMSem rs f) => GMSem rs (D1 i f) where
  gappend = coerce (gappend @rs @f)

-- Record
instance
  {-# OVERLAPPING #-}
  (GMSem rs f) =>
  GMSem rs (C1 (MetaCons name fix 'True) f)
  where
  gappend = coerce (gappend @rs @f)

instance (GMSem rs l, GMSem rs r) => GMSem rs (l :*: r) where
  gappend (l :*: r) (l' :*: r') =
    gappend @rs l l' :*: gappend @rs r r'

instance
  {-# OVERLAPPING #-}
  ( rs ~ Record fields
  , Just rep ~ Lookup fld fields
  , Coercible c rep
  , Semigroup rep
  ) =>
  GMSem rs (S1 (MetaSel ('Just fld) unp str src) (K1 i c))
  where
  gappend = coerce ((<>) @rep)

-- Positional
instance
  {-# OVERLAPPING #-}
  (rs ~ Positional reps, GMSemPos 0 reps f) =>
  GMSem rs (C1 (MetaCons name fix 'False) f)
  where
  gappend = coerce (gappendN @0 @reps @f)

type family Size f :: Nat where
  Size (S1 i f) = 1
  Size (l :*: r) = Size l + Size r

type GMSemPos :: Nat -> [Type] -> (Type -> Type) -> Constraint
class GMSemPos n reps f where
  gappendN :: f () -> f () -> f ()

instance
  {-# OVERLAPPING #-}
  ( GMSemPos n reps l
  , GMSemPos (n + Size l) reps r
  ) =>
  GMSemPos n reps (l :*: r)
  where
  gappendN (l :*: r) (l' :*: r') =
    gappendN @n @reps l l' :*: gappendN @(n + Size l) @reps r r'

instance
  {-# OVERLAPPING #-}
  ( Just rep ~ Index n reps
  , Coercible c rep
  , Semigroup rep
  ) =>
  GMSemPos n reps (S1 (MetaSel 'Nothing unp str src) (K1 i c))
  where
  gappendN = coerce ((<>) @rep)

type GMMon :: FieldReps -> (Type -> Type) -> Constraint
class (GMSem rs f) => GMMon rs f where
  gmempty :: f ()

instance {-# OVERLAPPING #-} (GMMon rs f) => GMMon rs (D1 i f) where
  gmempty = M1 $ gmempty @rs
  {-# INLINE gmempty #-}

-- Record
instance
  {-# OVERLAPPING #-}
  (GMMon rs f) =>
  GMMon rs (C1 (MetaCons name fix 'True) f)
  where
  gmempty = M1 $ gmempty @rs
  {-# INLINE gmempty #-}

instance (GMMon rs l, GMMon rs r) => GMMon rs (l :*: r) where
  gmempty = gmempty @rs :*: gmempty @rs
  {-# INLINE gmempty #-}

instance
  {-# OVERLAPPING #-}
  ( rs ~ Record fields
  , Just rep ~ Lookup fld fields
  , Coercible c rep
  , Monoid rep
  ) =>
  GMMon rs (S1 (MetaSel ('Just fld) unp str src) (K1 i c))
  where
  gmempty = M1 $ K1 $ coerce $ mempty @rep
  {-# INLINE gmempty #-}

-- Positional
instance
  {-# OVERLAPPING #-}
  (rs ~ Positional reps, GMSemPos 0 reps f, GMMonPos 0 reps f) =>
  GMMon rs (C1 (MetaCons name fix 'False) f)
  where
  gmempty = M1 $ gmemptyN @0 @reps @f
  {-# INLINE gmempty #-}

type GMMonPos :: Nat -> [Type] -> (Type -> Type) -> Constraint
class GMMonPos n reps f where
  gmemptyN :: f ()

instance
  {-# OVERLAPPING #-}
  ( GMMonPos n reps l
  , GMMonPos (n + Size l) reps r
  ) =>
  GMMonPos n reps (l :*: r)
  where
  gmemptyN = gmemptyN @n @reps @l :*: gmemptyN @(n + Size l) @reps @r

instance
  {-# OVERLAPPING #-}
  ( Just rep ~ Index n reps
  , Coercible c rep
  , Monoid rep
  ) =>
  GMMonPos n reps (S1 (MetaSel 'Nothing unp str src) (K1 i c))
  where
  gmemptyN = coerce (mempty @rep)
