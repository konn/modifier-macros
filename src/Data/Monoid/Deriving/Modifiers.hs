{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Monoid.Deriving.Modifiers (
  ModifiersOn (..),
  plugin,
) where

import Control.Lens
import Control.Monad
import Data.Data.Lens
import Data.Generics.Labels ()
import Data.Maybe
import Data.Monoid
import Data.Monoid.Deriving.Modifiers.Types
import GHC
import GHC.Iface.Env
import GHC.Plugins hiding ((<>))
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import GHC.Types.SourceText

plugin :: Plugin
plugin =
  defaultPlugin
    { renamedResultAction = \_ -> renamedSourcePlugin
    , pluginRecompile = purePlugin
    , driverPlugin = enableExtensions
    }

enableExtensions :: [CommandLineOption] -> HscEnv -> IO HscEnv
enableExtensions _ env = do
  (flags, _, _) <-
    parseDynamicFlagsCmdLine
      (hsc_logger env)
      (hsc_dflags env)
      ( map
          noLoc
          [ "-XDataKinds"
          , "-XTypeFamilies"
          ]
      )
  pure (hscSetFlags flags env)

modifier_macros :: FastString
modifier_macros = "modifier-macros"

data RelatedNames = RelatedNames
  { modifiersOnTyName :: !Name
  , modifiersOnConName :: !Name
  , positionalConName :: !Name
  , recordConName :: !Name
  , fieldRepsOfTyName :: !Name
  }

renamedSourcePlugin :: TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
renamedSourcePlugin tcGblEnv hsGroup = do
  hscEnv <- getTopEnv
  let unitId = fromJust $ lookupPackageName (hsc_units hscEnv) (PackageName modifier_macros)
  let types = mkModule (RealUnit (Definite unitId)) (mkModuleName "Data.Monoid.Deriving.Modifiers.Types")
  modifiersOnTyName <- lookupOrig types (mkTcOcc "ModifiersOn")
  modifiersOnConName <- lookupOrig types (mkDataOcc "ModifiersOn")
  positionalConName <- lookupOrig types (mkTcOcc "Positional")
  recordConName <- lookupOrig types (mkTcOcc "Record")
  fieldRepsOfTyName <- lookupOrig types (mkTcOcc "FieldRepsOf")
  let names = RelatedNames {..}
      result = hsGroup ^.. biplate . to (matchModifiedData names) . _Just
      clgrp =
        TyClGroup
          { group_ext = mempty
          , group_tyclds = []
          , group_roles = []
          , group_kisigs = []
          , group_instds = result
          }
  pure (tcGblEnv, hsGroup {hs_tyclds = hsGroup.hs_tyclds <> [clgrp]})

matchModifiedData :: RelatedNames -> TyClDecl GhcRn -> Maybe (LInstDecl GhcRn)
matchModifiedData names DataDecl {..} = do
  targetTy <-
    alaf
      First
      (foldMapOf (biplate @_ @(DerivStrategy GhcRn)))
      ( \strat -> do
          ViaStrategy (unLoc -> HsSig {..}) <- pure strat
          HsAppTy _ (unLoc -> HsTyVar _ NotPromoted tyCon) r <- pure $ unLoc sig_body
          guard $ unwrapUserRdr (unLoc tyCon) == names.modifiersOnTyName
          pure (unLoc r)
      )
      tcdDataDefn.dd_derivs
  let bndrs =
        HsOuterExplicit noExtField $
          map (fmap eraseSpecificityFromTyVarBndr) $
            hsq_explicit tcdTyVars
  -- FIXME: make sure that the target type matches the expected type
  conDec <- unLoc <$> uniqueCons tcdDataDefn.dd_cons
  let fieldReps = encodeFieldInfo names $ parseFieldRepInfo conDec
  pure $
    noLocA $
      TyFamInstD noExtField $
        TyFamInstDecl (NoEpTok, NoEpTok) $
          FamEqn
            { feqn_ext = ([], [], NoEpTok)
            , feqn_tycon = noLocA names.fieldRepsOfTyName
            , feqn_bndrs = eraseSpecificity bndrs
            , feqn_pats =
                [HsValArg noExtField $ noLocA $ targetTy]
            , feqn_fixity = Prefix
            , feqn_rhs = fieldReps
            }
matchModifiedData _ _ = Nothing

data FieldRepInfo
  = IsRecord [(Name, LHsType GhcRn)]
  | IsPositional [LHsType GhcRn]

parseFieldRepInfo :: ConDecl GhcRn -> FieldRepInfo
parseFieldRepInfo ConDeclGADT {..} = 
  let con = case con_g_args of
        PrefixConGADT _ args -> PrefixCon args
        RecConGADT _ flds -> RecCon flds
  in parseConArgs con
parseFieldRepInfo ConDeclH98 {..} = parseConArgs con_args

parseConArgs :: HsConDeclH98Details GhcRn -> FieldRepInfo
parseConArgs con_args = case con_args of
  PrefixCon args -> IsPositional $ map resolveFieldRep args
  InfixCon l r -> IsPositional [resolveFieldRep l, resolveFieldRep r]
  RecCon (L _ flds) ->
    IsRecord $
      concatMap
        ( concatMap \HsConDeclRecField {..} ->
            map
              ( (,resolveFieldRep cdrf_spec)
                  . unLoc
                  . foLabel
                  . unLoc
              )
              cdrf_names
        )
        flds

resolveFieldRep :: HsConDeclField GhcRn -> LHsType GhcRn
resolveFieldRep CDF {cdf_multiplicity = HsModifiedFunArr _ mods _, ..} =
  fromMaybe cdf_type do
    L _ (HsModifier ModifierPrintsAsSelf ty) <- listToMaybe mods
    let isMult = case unLoc ty of
          HsTyVar _ _ (L _ name)
            | unwrapUserRdr name `elem` [oneDataConName, manyDataConName] -> True
          HsKindSig _ _ (L _ (HsTyVar _ _ (L _ name))) -> unwrapUserRdr name == multiplicityTyConName
          _ -> False
    guard $ not isMult
    pure ty

encodeFieldInfo :: RelatedNames -> FieldRepInfo -> LHsType GhcRn
encodeFieldInfo names (IsRecord fields) =
  noLocA
    $ HsAppTy
      noExtField
      ( noLocA $
          HsTyVar NoEpTok NotPromoted $
            noLocA $
              noUserRdr names.recordConName
      )
    $ noLocA
    $ HsExplicitListTy
      noExtField
      IsPromoted
      [ noLocA $
          HsExplicitTupleTy
            noExtField
            IsPromoted
            [ noLocA $ HsTyLit noExtField $ HsString NoSourceText $ getOccFS fld
            , repTy
            ]
      | (fld, repTy) <- fields
      ]
encodeFieldInfo names (IsPositional types) =
  noLocA
    $ HsAppTy
      noExtField
      ( noLocA $
          HsTyVar NoEpTok NotPromoted $
            noLocA $
              noUserRdr names.positionalConName
      )
    $ noLocA
    $ HsExplicitListTy noExtField IsPromoted types

eraseSpecificity :: HsOuterTyVarBndrs a GhcRn -> HsOuterFamEqnTyVarBndrs GhcRn
eraseSpecificity (HsOuterImplicit bndrs) = HsOuterImplicit bndrs
eraseSpecificity (HsOuterExplicit ext bndrs) = HsOuterExplicit ext $ fmap (fmap eraseSpecificityFromTyVarBndr) bndrs

eraseSpecificityFromTyVarBndr :: HsTyVarBndr a GhcRn -> HsTyVarBndr () GhcRn
eraseSpecificityFromTyVarBndr tvb = tvb {tvb_flag = ()}

uniqueCons :: DataDefnCons a -> Maybe a
uniqueCons (NewTypeCon a) = Just a
uniqueCons (DataTypeCons _ [a]) = Just a
uniqueCons _ = Nothing
