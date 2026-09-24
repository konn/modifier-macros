{-# LANGUAGE Modifiers #-}
{-# OPTIONS_GHC -fplugin=Language.Haskell.TH.Modifier.Plugin #-}
{-# OPTIONS_GHC -fplugin=Generics.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

module BothPlugins (Both (..)) where

import GHC.Generics (Generic)

%"both"
data Both = Both deriving (Generic)
