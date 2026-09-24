{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Modifiers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Language.Haskell.TH.Modifier.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-modifiers #-}
module LocalFixture (Local (..), localModifiers, localDatatypeInfo) where
import Language.Haskell.TH (pprint, stringE)
import Language.Haskell.TH.Modifier (DatatypeInfo, reifyDatatype, reifyModifier)
import Language.Haskell.TH.Syntax (liftData)
%"local"
data Local = Local
$(pure [])
localModifiers :: String
localModifiers = $(reifyModifier ''Local >>= stringE . pprint)
localDatatypeInfo :: DatatypeInfo
localDatatypeInfo = $(reifyDatatype ''Local >>= liftData)
