-- | Capture modifier annotations for Template Haskell reification.
module Language.Haskell.TH.Modifier.Plugin (plugin) where

import GHC.Modifiers (modifierPlugin)
import GHC.Plugins (Plugin)

-- | Capture modifiers during renaming.
plugin :: Plugin
plugin = modifierPlugin
