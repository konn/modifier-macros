{-# LANGUAGE LambdaCase #-}

{- | Conversion at the renamed syntax boundary. Names retain their defining
package, module and namespace; no pretty-printing/parsing round trip is used.
-}
module GHC.Modifiers.TH (modifierType, modifierName) where

import GHC.Hs
import GHC.Plugins hiding ((<>))
import GHC.Types.SourceText (il_value)
import GHC.Types.Unique (getKey)
import Language.Haskell.TH.Syntax qualified as TH

-- | Preserve global name identity and namespace, and local variable uniques.
modifierName :: Name -> TH.Name
modifierName n = case nameModule_maybe n of
  Nothing -> TH.mkNameU occ (toInteger (getKey (getUnique n)))
  Just m -> make (unitString (moduleUnit m)) (moduleNameString (moduleName m)) occ
  where
    o = nameOccName n
    occ = occNameString o
    make
      | isDataOcc o = TH.mkNameG_d
      | isVarOcc o = TH.mkNameG_v
      | Just con <- fieldOcc_maybe o = TH.mkNameG_fld `withConstructor` unpackFS con
      | otherwise = TH.mkNameG_tc
    withConstructor f con pkg mdl = f pkg mdl con

-- | Convert source type syntax, reporting syntax TH cannot faithfully express.
modifierType :: LHsType GhcRn -> Either String TH.Type
modifierType = go . unLoc
  where
    rec = modifierType
    apps h xs = foldl TH.AppT h <$> traverse rec xs
    go = \case
      HsTyVar _ _ n ->
        let nm = unwrapUserRdr (unLoc n)
            th = modifierName nm
         in pure $ if isTyVarName nm then TH.VarT th else if isDataOcc (nameOccName nm) then TH.PromotedT th else TH.ConT th
      HsAppTy _ f x -> TH.AppT <$> rec f <*> rec x
      HsAppKindTy _ f k -> TH.AppKindT <$> rec f <*> rec k
      HsOpTy _ l op r -> TH.AppT <$> (TH.AppT <$> rec op <*> rec l) <*> rec r
      HsParTy _ t -> rec t
      HsKindSig _ t k -> TH.SigT <$> rec t <*> rec k
      HsIParamTy _ (L _ (HsIPName n)) t -> TH.ImplicitParamT (unpackFS n) <$> rec t
      HsListTy _ t -> TH.AppT TH.ListT <$> rec t
      HsTupleTy _ sort ts -> apps (case sort of HsUnboxedTuple -> TH.UnboxedTupleT (length ts); _ -> TH.TupleT (length ts)) ts
      HsSumTy _ ts -> apps (TH.UnboxedSumT (length ts)) ts
      HsExplicitListTy _ _ ts -> foldr (\x xs -> TH.AppT (TH.AppT TH.PromotedConsT x) xs) TH.PromotedNilT <$> traverse rec ts
      HsExplicitTupleTy _ _ ts -> apps (TH.PromotedTupleT (length ts)) ts
      HsTyLit _ (HsString _ s) -> pure $ TH.LitT $ TH.StrTyLit $ unpackFS s
      HsTyLit _ (HsChar _ c) -> pure $ TH.LitT $ TH.CharTyLit c
      HsTyLit _ (HsNatural _ i) -> pure $ TH.LitT $ TH.NumTyLit $ il_value i
      HsStarTy _ -> pure TH.StarT
      HsWildCardTy _ -> pure TH.WildCardT
      HsDocTy _ t _ -> rec t
      HsSpliceTy HsUntypedSpliceTop {utsplice_result = t} _ -> rec t
      HsQualTy {hst_ctxt = c, hst_body = t} -> TH.ForallT [] <$> traverse rec (unLoc c) <*> rec t
      HsForAllTy {hst_tele = HsForAllInvis {hsf_invis_bndrs = bs}, hst_body = t} -> do
        bs' <- traverse (binder (\case SpecifiedSpec -> TH.SpecifiedSpec; InferredSpec -> TH.InferredSpec) . unLoc) bs
        body <- rec t
        pure $ case body of
          TH.ForallT [] c b -> TH.ForallT bs' c b
          _ -> TH.ForallT bs' [] body
      HsForAllTy {hst_tele = HsForAllVis {hsf_vis_bndrs = bs}, hst_body = t} -> TH.ForallVisT <$> traverse (binder id . unLoc) bs <*> rec t
      HsFunTy _ (HsModifiedFunArr _ ms arr) a b -> do
        h <- case ms of
          [] -> pure $ case arr of HsLinearArr {} -> TH.AppT TH.MulArrowT (TH.PromotedT (modifierName oneDataConName)); _ -> TH.ArrowT
          [L _ (HsModifier _ m)] | isMultiplicity (unLoc m) -> TH.AppT TH.MulArrowT <$> rec m
          _ -> Left "Template Haskell cannot represent arbitrary modifiers inside an arrow type; only a single explicit multiplicity is supported"
        TH.AppT <$> (TH.AppT h <$> rec a) <*> rec b
      t -> Left $ "Unsupported modifier type: " <> showSDocUnsafe (ppr t)
    binder flag HsTvb {tvb_var = HsBndrVar _ n, tvb_kind = k, tvb_flag = f} =
      case k of
        HsBndrNoKind _ -> pure $ TH.PlainTV (modifierName (unLoc n)) (flag f)
        HsBndrKind _ ty -> TH.KindedTV (modifierName (unLoc n)) (flag f) <$> rec ty
    binder _ _ = Left "Wildcard binders in modifiers cannot be reified"
    isMultiplicity (HsParTy _ t) = isMultiplicity (unLoc t)
    isMultiplicity (HsTyVar _ _ (L _ n)) = unwrapUserRdr n `elem` [oneDataConName, manyDataConName]
    isMultiplicity (HsKindSig _ _ (L _ (HsTyVar _ _ (L _ n)))) = unwrapUserRdr n == multiplicityTyConName
    isMultiplicity _ = False
