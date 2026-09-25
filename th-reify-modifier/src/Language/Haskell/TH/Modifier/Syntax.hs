{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Lower shared modifier syntax to Template Haskell only when requested by
a TH consumer. Capture and generic derivation do not impose these restrictions.
-}
module Language.Haskell.TH.Modifier.Syntax (modifierType) where

import GHC.Exts (Multiplicity (..))
import GHC.Modifiers.Types qualified as S
import Language.Haskell.TH.Syntax qualified as TH

modifierName :: S.ModifierName -> TH.Name
modifierName (S.ModifierName namespace identity) = case identity of
  S.LocalName occ unique -> TH.mkNameU occ unique
  S.GlobalName unit mdl occ -> case namespace of
    S.DataConstructor -> TH.mkNameG_d unit mdl occ
    S.Value -> TH.mkNameG_v unit mdl occ
    S.Field con -> TH.mkNameG_fld unit mdl con occ
    _ -> TH.mkNameG_tc unit mdl occ

{- | Convert captured syntax, failing rather than dropping information that TH
cannot express (notably arbitrary modifiers inside an arrow type).
-}
modifierType :: S.ModifierType -> Either String TH.Type
modifierType = go
  where
    apps h xs = foldl TH.AppT h <$> traverse go xs
    go = \case
      S.NamedType n@(S.ModifierName namespace _) -> pure $ case namespace of
        S.TypeVariable -> TH.VarT (modifierName n)
        S.DataConstructor -> TH.PromotedT (modifierName n)
        _ -> TH.ConT (modifierName n)
      S.AppliedType f x -> TH.AppT <$> go f <*> go x
      S.KindAppliedType f k -> TH.AppKindT <$> go f <*> go k
      S.InfixType l op r -> TH.AppT <$> (TH.AppT <$> go op <*> go l) <*> go r
      S.ParenthesizedType t -> go t
      S.KindSignature t k -> TH.SigT <$> go t <*> go k
      S.ImplicitParameter n t -> TH.ImplicitParamT n <$> go t
      S.ListType t -> TH.AppT TH.ListT <$> go t
      S.TupleType sort ts -> apps (case sort of S.UnboxedTuple -> TH.UnboxedTupleT (length ts); S.BoxedOrConstraintTuple -> TH.TupleT (length ts)) ts
      S.SumType ts -> apps (TH.UnboxedSumT (length ts)) ts
      S.PromotedList ts -> foldr (\x xs -> TH.AppT (TH.AppT TH.PromotedConsT x) xs) TH.PromotedNilT <$> traverse go ts
      S.PromotedTuple ts -> apps (TH.PromotedTupleT (length ts)) ts
      S.StringType s -> pure $ TH.LitT $ TH.StrTyLit s
      S.CharacterType c -> pure $ TH.LitT $ TH.CharTyLit c
      S.NaturalType i -> pure $ TH.LitT $ TH.NumTyLit i
      S.StarType -> pure TH.StarT
      S.WildcardType -> pure TH.WildCardT
      S.QualifiedType c t -> TH.ForallT [] <$> traverse go c <*> go t
      S.ForallType bs t -> do
        bs' <- traverse (binder specificity) bs
        body <- go t
        pure $ case body of
          TH.ForallT [] c b -> TH.ForallT bs' c b
          _ -> TH.ForallT bs' [] body
      S.VisibleForallType bs t -> TH.ForallVisT <$> traverse (binder (const (pure ()))) bs <*> go t
      S.FunctionType arrow ms a b -> do
        h <- case (arrow, ms) of
          (S.StandardArrow, []) -> pure TH.ArrowT
          (S.LinearArrow, []) -> pure $ TH.AppT TH.MulArrowT $ TH.PromotedT 'One
          (S.StandardArrow, [m]) | isMultiplicity m -> TH.AppT TH.MulArrowT <$> go m
          _ -> Left "Template Haskell cannot represent arbitrary modifiers inside an arrow type; only a single explicit multiplicity is supported"
        TH.AppT <$> (TH.AppT h <$> go a) <*> go b
    binder :: (S.BinderVisibility -> Either String flag) -> S.Binder -> Either String (TH.TyVarBndr flag)
    binder flag (S.Binder (Just n) k visibility) = do
      f <- flag visibility
      case k of
        Nothing -> pure $ TH.PlainTV (modifierName n) f
        Just ty -> TH.KindedTV (modifierName n) f <$> go ty
    binder _ (S.Binder Nothing _ _) = Left "Template Haskell cannot represent wildcard binders in modifiers"
    specificity S.Specified = pure TH.SpecifiedSpec
    specificity S.Inferred = pure TH.InferredSpec
    specificity S.Required = Left "Invalid required binder in invisible forall modifier syntax"
    isMultiplicity (S.ParenthesizedType t) = isMultiplicity t
    isMultiplicity (S.NamedType n) = modifierName n `elem` ['One, 'Many]
    isMultiplicity (S.KindSignature _ k) = case stripParens k of
      S.NamedType n -> modifierName n == ''Multiplicity
      _ -> False
    isMultiplicity _ = False
    stripParens (S.ParenthesizedType t) = stripParens t
    stripParens t = t
