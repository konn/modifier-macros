{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Modifiers #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fplugin=Language.Haskell.TH.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}

module FieldFixtures (One (..), Two (..)) where

data One a = One {same %"one" %(Maybe a) :: a}
data Two a = Two {same %"two" %(Maybe a) :: a}
