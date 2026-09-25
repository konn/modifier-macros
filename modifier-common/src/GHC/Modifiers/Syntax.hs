{-# LANGUAGE LambdaCase #-}

-- | Capture renamed syntax without lowering it to a consumer's representation.
module GHC.Modifiers.Syntax (captureType, captureName) where

import GHC.Hs
import GHC.Modifiers.Types qualified as S
import GHC.Plugins hiding ((<>))
import GHC.Types.SourceText (il_value)
import GHC.Types.Unique (getKey)

-- | Retain namespace, global identity and local binder uniques.
captureName :: Name -> S.ModifierName
captureName n = S.ModifierName namespace identity
  where
    o = nameOccName n
    occ = occNameString o
    namespace
      | isTyVarName n = S.TypeVariable
      | isDataOcc o = S.DataConstructor
      | isVarOcc o = S.Value
      | Just con <- fieldOcc_maybe o = S.Field (unpackFS con)
      | otherwise = S.TypeConstructor
    identity = case nameModule_maybe n of
      Nothing -> S.LocalName occ (toInteger (getKey (getUnique n)))
      Just m -> S.GlobalName (unitString (moduleUnit m)) (moduleNameString (moduleName m)) occ

{- | Capture all renamed modifier type forms supported by the pinned compiler.
In particular, function arrows retain arbitrary modifiers rather than just
the multiplicities understood by some consumers.
-}
captureType :: LHsType GhcRn -> Either String S.ModifierType
captureType = go . unLoc
  where
    rec = captureType
    go = \case
      HsTyVar _ _ n -> pure $ S.NamedType $ captureName $ unwrapUserRdr $ unLoc n
      HsAppTy _ f x -> S.AppliedType <$> rec f <*> rec x
      HsAppKindTy _ f k -> S.KindAppliedType <$> rec f <*> rec k
      HsOpTy _ l op r -> S.InfixType <$> rec l <*> rec op <*> rec r
      HsParTy _ t -> S.ParenthesizedType <$> rec t
      HsKindSig _ t k -> S.KindSignature <$> rec t <*> rec k
      HsIParamTy _ (L _ (HsIPName n)) t -> S.ImplicitParameter (unpackFS n) <$> rec t
      HsListTy _ t -> S.ListType <$> rec t
      HsTupleTy _ sort ts -> S.TupleType (case sort of HsUnboxedTuple -> S.UnboxedTuple; HsBoxedOrConstraintTuple -> S.BoxedOrConstraintTuple) <$> traverse rec ts
      HsSumTy _ ts -> S.SumType <$> traverse rec ts
      HsExplicitListTy _ _ ts -> S.PromotedList <$> traverse rec ts
      HsExplicitTupleTy _ _ ts -> S.PromotedTuple <$> traverse rec ts
      HsTyLit _ (HsString _ s) -> pure $ S.StringType $ unpackFS s
      HsTyLit _ (HsChar _ c) -> pure $ S.CharacterType c
      HsTyLit _ (HsNatural _ i) -> pure $ S.NaturalType $ il_value i
      HsStarTy _ -> pure S.StarType
      HsWildCardTy _ -> pure S.WildcardType
      HsDocTy _ t _ -> rec t
      HsSpliceTy HsUntypedSpliceTop {utsplice_result = t} _ -> rec t
      HsQualTy {hst_ctxt = c, hst_body = t} -> S.QualifiedType <$> traverse rec (unLoc c) <*> rec t
      HsForAllTy {hst_tele = HsForAllInvis {hsf_invis_bndrs = bs}, hst_body = t} ->
        S.ForallType <$> traverse (binder (\case SpecifiedSpec -> S.Specified; InferredSpec -> S.Inferred) . unLoc) bs <*> rec t
      HsForAllTy {hst_tele = HsForAllVis {hsf_vis_bndrs = bs}, hst_body = t} ->
        S.VisibleForallType <$> traverse (binder (const S.Required) . unLoc) bs <*> rec t
      HsFunTy _ (HsModifiedFunArr _ ms arr) a b ->
        S.FunctionType (case arr of HsLinearArr {} -> S.LinearArrow; HsStandardArr {} -> S.StandardArrow)
          <$> traverse (\(L _ (HsModifier _ m)) -> rec m) ms
          <*> rec a
          <*> rec b
      t -> Left $ "Unsupported renamed modifier syntax: " <> showSDocUnsafe (ppr t)
    binder visibility HsTvb {tvb_var = v, tvb_kind = k, tvb_flag = f} =
      S.Binder
        (case v of HsBndrVar _ n -> Just $ captureName $ unLoc n; HsBndrWildCard _ -> Nothing)
        <$> (case k of HsBndrNoKind _ -> pure Nothing; HsBndrKind _ ty -> Just <$> rec ty)
        <*> pure (visibility f)
