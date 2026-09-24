{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

{- | Reify modifiers captured by @Language.Haskell.TH.Modifier.Plugin@ (or
@Generics.Modifier.Plugin@) in the entity's defining module.
-}
module Language.Haskell.TH.Modifier (
  reifyModifier,
  reifyDatatype,
  reifyConstructor,
  DatatypeInfo (..),
  DatatypeVariant (..),
  ConstructorInfo (..),
  ConstructorVariant (..),
  FieldInfo (..),
  FieldStrictness (FieldStrictness),
  Strictness (..),
  Unpackedness (..),
) where

import Control.Monad (unless)
import Data.Data (Data)
import Data.List (find, zipWith4)
import Data.Map.Strict qualified as Map
import GHC.Modifiers.Types (ConstructorFieldsAnnotation (..), ModifierAnnotation (..))
import Language.Haskell.TH
import Language.Haskell.TH.Datatype (ConstructorVariant (..), DatatypeVariant (..), FieldStrictness (FieldStrictness), Strictness (..), Unpackedness (..))
import Language.Haskell.TH.Datatype qualified as D

{- | A modifier-aware view of a @data@ or @newtype@ declaration, normalized
by @th-abstraction@. Datatype and constructor binders, contexts, kinds and
variants have the same meaning as in "Language.Haskell.TH.Datatype".
-}
data DatatypeInfo = DatatypeInfo
  { datatypeName :: Name
  -- ^ Datatype name.
  , datatypeContext :: Cxt
  -- ^ Declaration context, if any.
  , datatypeVars :: [TyVarBndr ()]
  -- ^ Kind and type parameters normalized by @th-abstraction@.
  , datatypeInstTypes :: [Type]
  -- ^ Datatype argument types, as in @th-abstraction@.
  , datatypeReturnKind :: Kind
  -- ^ Normalized return kind.
  , datatypeVariant :: DatatypeVariant
  -- ^ Whether this is a @data@ or @newtype@ declaration.
  , datatypeCons :: [ConstructorInfo]
  -- ^ Constructors in declaration order.
  , datatypeModifiers :: [Type]
  -- ^ Declaration modifiers in source order, retaining kinds and duplicates.
  }
  deriving (Eq, Show, Data)

-- | One constructor, with its modifiers and the modifiers of each field.
data ConstructorInfo = ConstructorInfo
  { constructorName :: Name
  -- ^ Constructor name. Grouped GADT signatures produce one entry per name.
  , constructorVars :: [TyVarBndr ()]
  -- ^ Existential binders after @th-abstraction@ normalization.
  , constructorContext :: Cxt
  -- ^ Normalized constraints, including GADT refinement equalities.
  , constructorFields :: [FieldInfo]
  -- ^ Fields in declaration order, including positional fields.
  , constructorVariant :: ConstructorVariant
  -- ^ The @th-abstraction@ variant, including labels for record constructors.
  , constructorModifiers :: [Type]
  -- ^ Constructor modifiers in source order.
  }
  deriving (Eq, Show, Data)

-- | A field occurrence in a particular constructor.
data FieldInfo = FieldInfo
  { fieldName :: Maybe Name
  {- ^ A record label, including with @NoFieldSelectors@; positional fields
  have 'Nothing'.
  -}
  , fieldType :: Type
  -- ^ Field type, sharing binders with the enclosing constructor and datatype.
  , fieldStrictness :: FieldStrictness
  -- ^ Unpacking and strictness normalized by @th-abstraction@.
  , fieldModifiers :: [Type]
  {- ^ Modifiers of this occurrence only, including explicit multiplicities.
  A shared selector's other occurrences are not combined here.
  -}
  }
  deriving (Eq, Show, Data)

{- | Reify a datatype, one of its constructors or a record selector into a
structured view, using 'D.reifyDatatype' to resolve the name and normalize it.
Requires capture in the defining module, just like 'reifyModifier'. All types,
kinds and modifiers use TH syntax, and modifier variables refer to the binders
in the returned structure. Empty and unmodified declarations are supported.

This simplified interface supports ordinary @data@ and @newtype@ declarations,
including GADTs. Type synonyms, families and @type data@ are not supported by
modifier capture. Modifiers are normalized in the same scope as field types,
so GADT variable renaming and substitution also apply to modifiers.
-}
reifyDatatype :: Name -> Q DatatypeInfo
reifyDatatype name = do
  info <- D.reifyDatatype name
  cons <-
    reify (D.datatypeName info) >>= \case
      TyConI (DataD _ _ _ _ cs _) | D.datatypeVariant info == Datatype -> pure cs
      TyConI (NewtypeD _ _ _ _ c _) | D.datatypeVariant info == Newtype -> pure [c]
      _ -> fail $ "reifyDatatype: modifier capture supports ordinary data/newtype declarations, got " <> show name
  mods <- refreshVariables (concatMap binderScope (D.datatypeVars info)) <$> readModifiers (D.datatypeName info)
  constructors <- concat <$> traverse (constructorInfo info [] []) cons
  pure $
    DatatypeInfo
      (D.datatypeName info)
      (D.datatypeContext info)
      (D.datatypeVars info)
      (D.datatypeInstTypes info)
      (D.datatypeReturnKind info)
      (D.datatypeVariant info)
      constructors
      mods

-- | Reify just one constructor, retaining the same information as 'datatypeCons'.
reifyConstructor :: Name -> Q ConstructorInfo
reifyConstructor name =
  reify name >>= \case
    DataConI {} -> do
      info <- reifyDatatype name
      case find ((== name) . constructorName) (datatypeCons info) of
        Just con -> pure con
        Nothing -> fail $ "reifyConstructor: constructor not found: " <> show name
    _ -> fail $ "reifyConstructor: expected a data constructor, got " <> show name

constructorInfo :: D.DatatypeInfo -> [TyVarBndr Specificity] -> Cxt -> Con -> Q [ConstructorInfo]
constructorInfo info bs ctx = \case
  ForallC more context con -> constructorInfo info (bs <> more) (ctx <> context) con
  NormalC n fs -> build n fs Nothing
  InfixC l n r -> build n [l, r] Nothing
  RecC n fs -> build n (map record fs) Nothing
  GadtC ns fs ret -> concat <$> traverse (\n -> build n fs (Just ret)) ns
  RecGadtC ns fs ret -> concat <$> traverse (\n -> build n (map record fs) (Just ret)) ns
  where
    record (_, strict, ty) = (strict, ty)
    build n fs ret = do
      let scope = concatMap binderScope bs <> concatMap binderScope (D.datatypeVars info) <> D.freeVariables (ctx <> maybe [] pure ret <> map snd fs)
          refresh :: (D.TypeSubstitution a) => a -> a
          refresh = refreshVariables scope
      mods <- refresh <$> readModifiers n
      annotations <- reifyAnnotations (AnnLookupName n)
      fieldMods <- case annotations of
        [ConstructorFieldsAnnotation ms] -> pure $ refresh ms
        [] -> fail $ "No constructor field metadata for " <> show n <> ". Recompile its defining module with -fplugin=Language.Haskell.TH.Modifier.Plugin."
        _ -> fail $ "Multiple constructor field metadata annotations for " <> show n
      unless (length fs == length fieldMods) $
        fail $
          "Constructor field metadata does not match " <> show n <> ". Recompile its defining module."
      original <- case find ((== n) . D.constructorName) (D.datatypeCons info) of
        Just con -> pure con
        Nothing -> fail $ "reifyDatatype: constructor not found: " <> show n
      -- Treat modifier types as extra fields only during normalization. This
      -- lets th-abstraction apply exactly the same capture-avoiding GADT
      -- substitutions to field types, constructor modifiers and field modifiers.
      -- Remove the extra fields afterwards; recover syntax from the original
      -- normalized constructor, so infix and record variants are unaffected.
      let payload = mods <> concat fieldMods
          augmented = fs <> [(Bang NoSourceUnpackedness NoSourceStrictness, ty) | ty <- payload]
          con = maybe (NormalC n augmented) (GadtC [n] augmented) ret
      normalized <- D.normalizeCon (D.datatypeName info) (D.datatypeVars info) (D.datatypeInstTypes info) (D.datatypeReturnKind info) (D.datatypeVariant info) (ForallC bs ctx con)
      case normalized of
        [result] -> do
          let (types, normalizedPayload) = splitAt (length fs) (D.constructorFields result)
              (conMods, fieldPayload) = splitAt (length mods) normalizedPayload
              labels = case D.constructorVariant original of
                RecordConstructor names -> map Just names
                _ -> replicate (length fs) Nothing
              fields = zipWith4 FieldInfo labels types (D.constructorStrictness result) (splitFields (map length fieldMods) fieldPayload)
          unless (length (D.constructorFields result) == length fs + length payload) $
            fail $
              "reifyDatatype: normalization changed the field count for " <> show n
          pure [ConstructorInfo n (D.constructorVars result) (D.constructorContext result) fields (D.constructorVariant original) conMods]
        _ -> fail $ "reifyDatatype: expected one normalized constructor for " <> show n

splitFields :: [Int] -> [Type] -> [[Type]]
splitFields [] _ = []
splitFields (count : counts) ts = let (field, rest) = splitAt count ts in field : splitFields counts rest

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
  modifiers <- readModifiers name
  -- Interface files freshen type-variable uniques. Align free variables
  -- with TH's current view of the entity, so clients can use the result
  -- with the binders returned by ordinary reify without capture.
  scope <- reify name
  pure $ refreshVariables (scopeNames scope) modifiers

readModifiers :: Name -> Q [Type]
readModifiers name = do
  annotations <- reifyAnnotations (AnnLookupName name)
  case annotations of
    [] -> fail $ "No modifier metadata for " <> show name <> ". Enable -fplugin=Language.Haskell.TH.Modifier.Plugin in its defining module and place local declarations before a declaration splice."
    _ -> pure $ concat [ts | ModifierAnnotation ts <- annotations]

scopeNames :: Info -> [Name]
scopeNames = \case
  TyConI (DataD _ _ bs _ _ _) -> concatMap binderScope bs
  TyConI (NewtypeD _ _ bs _ _ _) -> concatMap binderScope bs
  DataConI _ ty _ -> typeScope ty
  VarI _ ty _ -> typeScope ty
  _ -> []
  where
    typeScope (ForallT bs _ t) = concatMap binderScope bs <> typeScope t
    typeScope (ForallVisT bs t) = concatMap binderScope bs <> typeScope t
    typeScope _ = []

binderScope :: TyVarBndr flag -> [Name]
binderScope b = D.tvName b : D.freeVariables (D.tvKind b)

refreshVariables :: (D.TypeSubstitution a) => [Name] -> a -> a
refreshVariables scope value =
  D.applySubstitution
    ( Map.fromList
        [ (n, VarT current)
        | n <- D.freeVariables value
        , Just current <- [lookup (nameBase n) [(nameBase v, v) | v <- scope]]
        ]
    )
    value
