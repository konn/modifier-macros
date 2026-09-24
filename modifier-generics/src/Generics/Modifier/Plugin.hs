{-# LANGUAGE RecordWildCards #-}

-- | Enable with @-fplugin=Generics.Modifier.Plugin@ in a datatype's module.
module Generics.Modifier.Plugin (plugin) where

import Data.Data (Data, cast, gmapT)
import Data.Maybe (fromMaybe)
import GHC.Hs
import GHC.Iface.Env (lookupOrig)
import GHC.Modifiers
import GHC.Plugins hiding ((<>))
import GHC.Tc.Errors.Types (mkTcRnUnknownMessage)
import GHC.Tc.Types (TcM)
import GHC.Tc.Utils.Monad (failWithTc, getTopEnv)
import GHC.Types.Error (mkPlainError)
import GHC.Types.SourceText (SourceText (..))

-- | Capture modifiers during renaming without changing class derivations.
plugin :: Plugin
plugin =
  defaultPlugin
    { pluginRecompile = purePlugin
    , driverPlugin = \_ env -> do
        (flags, _, _) <-
          parseDynamicFlagsCmdLine
            (hsc_logger env)
            (hsc_dflags env)
            (map noLoc ["-XDataKinds", "-XTypeFamilies", "-XUndecidableInstances"])
        pure $ hscSetFlags flags env
    , renamedResultAction = \_ env group -> do
        let ds = collectModifiers group
        env' <- annotateModifiers env ds
        (family, wrapper) <- metadataNames
        let insts = map (metadataInstance family wrapper) ds
            extra = TyClGroup mempty [] [] [] insts
        pure (env', group {hs_tyclds = hs_tyclds group <> [extra]})
    }

metadataNames :: TcM (Name, Name)
metadataNames = do
  env <- getTopEnv
  case lookupPackageName (hsc_units env) (PackageName (fsLit "modifier-common")) of
    Nothing -> failWithTc $ mkTcRnUnknownMessage $ mkPlainError [] (text "modifier-common is required by Generics.Modifier.Plugin")
    Just unit -> do
      let mdl = mkModule (RealUnit (Definite unit)) (mkModuleName "GHC.Modifiers.Types")
      (,) <$> lookupOrig mdl (mkTcOcc "ModifierMetadata") <*> lookupOrig mdl (mkTcOcc "Mod")

metadataInstance :: Name -> Name -> DatatypeModifiers -> LInstDecl GhcRn
metadataInstance family wrapper DatatypeModifiers {datatypeDeclaration = DataDecl {..}, ..} =
  noLocA $
    TyFamInstD noExtField $
      TyFamInstDecl (NoEpTok, NoEpTok) $
        FamEqn
          { feqn_ext = ([], [], NoEpTok)
          , feqn_tycon = noLocA family
          , feqn_bndrs = HsOuterExplicit noExtField (implicitBinders <> [fmap (\binder -> binder {tvb_flag = ()}) b | b <- hsq_explicit tcdTyVars])
          , feqn_pats = [HsValArg noExtField target]
          , feqn_fixity = Prefix
          , feqn_rhs = tuple [mods datatypeModifiers, list (map con datatypeConstructors)]
          }
  where
    implicitBinders = [noLocA (HsTvb noAnn () (HsBndrVar noExtField (noLocA n)) (HsBndrNoKind noExtField)) | n <- hsq_ext tcdTyVars]
    target = foldl applyBinder (var datatypeName) (hsq_explicit tcdTyVars)
    applyBinder :: LHsType GhcRn -> LHsTyVarBndr (HsBndrVis GhcRn) GhcRn -> LHsType GhcRn
    applyBinder ty (L _ HsTvb {tvb_var = HsBndrVar _ n, tvb_flag = visibility}) =
      case visibility of
        HsBndrRequired _ -> app ty (var (unLoc n))
        HsBndrInvisible _ -> noLocA $ HsAppKindTy noExtField ty (var (unLoc n))
    applyBinder ty _ = ty
    mods = list . map (app (var wrapper))
    con ConstructorModifiers {..} =
      tuple
        [ noLocA $ HsTyLit noExtField $ HsString NoSourceText $ getOccFS constructorName
        , mods (map instantiate constructorModifiers)
        , list (map (mods . map instantiate . fieldModifiers) constructorFields)
        ]
      where
        -- GADT signature variables have distinct Names from the datatype's
        -- parameters, even for vanilla GADTs accepted by stock Generic.
        substitution = maybe [] (matchVariables target) constructorResult
        instantiate = renameVariables substitution
metadataInstance _ _ _ = error "collectModifiers returned a non-datatype declaration"

matchVariables :: LHsType GhcRn -> LHsType GhcRn -> [(Name, Name)]
matchVariables lhs rhs = case (unLoc lhs, unLoc rhs) of
  (HsParTy _ t, _) -> matchVariables t rhs
  (_, HsParTy _ t) -> matchVariables lhs t
  (HsKindSig _ t _, _) -> matchVariables t rhs
  (_, HsKindSig _ t _) -> matchVariables lhs t
  (HsAppTy _ f x, HsAppTy _ g y) -> matchVariables f g <> matchVariables x y
  (HsAppKindTy _ f k, HsAppKindTy _ g l) -> matchVariables f g <> matchVariables k l
  (_, HsAppKindTy _ t _) -> matchVariables lhs t
  (HsAppKindTy _ t _, _) -> matchVariables t rhs
  (HsTyVar _ _ (L _ n), HsTyVar _ _ (L _ m))
    | isTyVarName (unwrapUserRdr m) -> [(unwrapUserRdr m, unwrapUserRdr n)]
  _ -> []

renameVariables :: (Data a) => [(Name, Name)] -> a -> a
renameVariables substitution x = case cast x of
  Just n -> fromMaybe x $ cast $ fromMaybe n (lookup n substitution)
  Nothing -> gmapT (renameVariables substitution) x

var :: Name -> LHsType GhcRn
var n = noLocA $ HsTyVar NoEpTok NotPromoted $ noLocA $ noUserRdr n

app :: LHsType GhcRn -> LHsType GhcRn -> LHsType GhcRn
app f x = noLocA $ HsAppTy noExtField f x

list :: [LHsType GhcRn] -> LHsType GhcRn
list = noLocA . HsExplicitListTy noExtField IsPromoted

tuple :: [LHsType GhcRn] -> LHsType GhcRn
tuple = noLocA . HsExplicitTupleTy noExtField IsPromoted
