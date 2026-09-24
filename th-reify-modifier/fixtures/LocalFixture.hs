{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Modifiers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Language.Haskell.TH.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}
module LocalFixture (Local (..), localModifiers) where
import Language.Haskell.TH (pprint, stringE)
import Language.Haskell.TH.Modifier (reifyModifier)
%"local"
data Local = Local
$(pure [])
localModifiers :: String
localModifiers = $(reifyModifier ''Local >>= stringE . pprint)
