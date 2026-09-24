{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Modifiers #-}
{-# OPTIONS_GHC -fplugin=Language.Haskell.TH.Modifier.Plugin #-}
{-# OPTIONS_GHC -fplugin=Generics.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

module BothPlugins (Both (..), BothFields (..)) where

import GHC.Generics qualified as GHC
import Generics.Modifier (Generic)

%"both"
data Both = Both
  deriving stock GHC.Generic
  deriving anyclass Generic

data BothFields = BothFields {bothValue %"field" :: Int}
  deriving stock GHC.Generic
  deriving anyclass Generic
