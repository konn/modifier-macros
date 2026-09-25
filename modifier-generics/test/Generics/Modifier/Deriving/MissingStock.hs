{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
-- Deliberately omit stock instances. Defer the resulting default-method errors
-- in this fixture alone, so the spec can inspect which instances actually exist.
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -fplugin=Generics.Modifier.Plugin #-}

module Generics.Modifier.Deriving.MissingStock (AnyclassOnly (..)) where

import Generics.Modifier qualified as M

data AnyclassOnly a = AnyclassOnly a

deriving anyclass instance M.Generic (AnyclassOnly a)

deriving anyclass instance M.Generic1 AnyclassOnly
