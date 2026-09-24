{-# LANGUAGE LambdaCase #-}

{- | Reify modifiers captured by @Language.Haskell.TH.Modifier.Plugin@ (or
@Generics.Modifier.Plugin@) in the entity's defining module.
-}
module Language.Haskell.TH.Modifier (reifyModifier) where

import Data.Data (Data, cast, gmapQ, gmapT)
import GHC.Modifiers.Types (ModifierAnnotation (..))
import Language.Haskell.TH

{- | Return all modifiers of a datatype, data constructor or record field, in
source order. Kinds may differ, and repeated modifiers are retained.

Use a declaration-group boundary (@$(pure [])@) before reifying a declaration
in the current module, just as for 'reify'. Imported annotations are read from
interface files, including when the defining source is unavailable.

A shared record selector concatenates the modifiers of its occurrences in
constructor order. Missing capture is an error, distinct from a captured
entity with no modifiers (which returns @[]@).
-}
reifyModifier :: Name -> Q [Type]
reifyModifier name = do
  annotations <- reifyAnnotations (AnnLookupName name)
  case annotations of
    [] -> fail $ "No modifier metadata for " <> show name <> ". Enable -fplugin=Language.Haskell.TH.Modifier.Plugin in its defining module and place local declarations before a declaration splice."
    _ -> do
      -- Interface files freshen type-variable uniques. Align free variables
      -- with TH's current view of the entity, so clients can use the result
      -- with the binders returned by ordinary reify without capture.
      scope <- reify name
      let names = scopeNames scope
      pure $ map (refreshVariables names []) $ concat [ts | ModifierAnnotation ts <- annotations]

scopeNames :: Info -> [Name]
scopeNames = \case
  TyConI (DataD _ _ bs _ _ _) -> concatMap binderScope bs
  TyConI (NewtypeD _ _ bs _ _ _) -> concatMap binderScope bs
  DataConI _ ty _ -> typeScope ty
  VarI _ ty _ -> typeScope ty
  _ -> []
  where
    binderScope b =
      binderName b : case b of
        PlainTV {} -> []
        KindedTV _ _ k -> variables k
    typeScope (ForallT bs _ t) = concatMap binderScope bs <> typeScope t
    typeScope (ForallVisT bs t) = concatMap binderScope bs <> typeScope t
    typeScope _ = []

binderName :: TyVarBndr flag -> Name
binderName (PlainTV n _) = n
binderName (KindedTV n _ _) = n

variables :: (Data a) => a -> [Name]
variables x = case cast x of
  Just (VarT n) -> [n]
  _ -> concat (gmapQ variables x)

refreshVariables :: (Data a) => [Name] -> [Name] -> a -> a
refreshVariables scope bound x = case cast x of
  Just ty -> case cast (refreshType ty) of
    Just y -> y
    Nothing -> x
  Nothing -> gmapT (refreshVariables scope bound) x
  where
    refreshType :: Type -> Type
    refreshType (VarT n)
      | n `notElem` bound
      , Just n' <- lookup (nameBase n) [(nameBase v, v) | v <- scope] =
          VarT n'
    refreshType (ForallT bs ctx ty) =
      let go :: (Data b) => b -> b
          go = refreshVariables scope (map binderName bs <> bound)
       in ForallT (go bs) (go ctx) (go ty)
    refreshType (ForallVisT bs ty) =
      let go :: (Data b) => b -> b
          go = refreshVariables scope (map binderName bs <> bound)
       in ForallVisT (go bs) (go ty)
    refreshType ty = gmapT (refreshVariables scope bound) ty
