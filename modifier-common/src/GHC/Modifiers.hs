{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Shared collection of modifiers from renamed declarations.
module GHC.Modifiers (
  DatatypeModifiers (..),
  ConstructorModifiers (..),
  FieldModifiers (..),
  collectModifiers,
  annotateModifiers,
  modifierPlugin,
) where

import Data.Foldable (toList)
import Data.List (groupBy, sortOn)
import GHC.Hs
import GHC.Modifiers.Syntax (captureType)
import GHC.Modifiers.Types (ConstructorFieldsSyntaxAnnotation (..), ModifierSyntaxAnnotation (..))
import GHC.Plugins hiding ((<>))
import GHC.Tc.Errors.Types (mkTcRnUnknownMessage)
import GHC.Tc.Types (TcGblEnv (..), TcM)
import GHC.Tc.Utils.Monad (failWithTc)
import GHC.Types.Error (mkPlainError)

-- | A datatype and its modifiers, with all constructors in declaration order.
data DatatypeModifiers = DatatypeModifiers
  { datatypeDeclaration :: TyClDecl GhcRn
  , datatypeName :: Name
  , datatypeModifiers :: [LHsType GhcRn]
  , datatypeConstructors :: [ConstructorModifiers]
  }

-- | One constructor, including each field in declaration order.
data ConstructorModifiers = ConstructorModifiers
  { constructorName :: Name
  , constructorModifiers :: [LHsType GhcRn]
  , constructorFields :: [FieldModifiers]
  , constructorResult :: Maybe (LHsType GhcRn)
  }

-- | A named record field or an anonymous positional field.
data FieldModifiers = FieldModifiers
  { fieldName :: Maybe Name
  , fieldModifiers :: [LHsType GhcRn]
  }

-- | Traverse declarations without selecting particular modifier kinds.
collectModifiers :: HsGroup GhcRn -> [DatatypeModifiers]
collectModifiers group =
  [ DatatypeModifiers decl (unLoc tcdLName) (types tcdModifiers) (concatMap (constructors . unLoc) (toList (dd_cons tcdDataDefn)))
  | L _ decl@DataDecl {..} <- concatMap group_tyclds (hs_tyclds group)
  ]
  where
    constructors = \case
      ConDeclH98 {..} -> [ConstructorModifiers (unLoc con_name) (types con_modifiers) (fields con_args) Nothing]
      ConDeclGADT {..} ->
        [ ConstructorModifiers
            (unLoc n)
            (types con_modifiers)
            ( fields $ case con_g_args of
                PrefixConGADT _ args -> PrefixCon args
                RecConGADT _ fs -> RecCon fs
            )
            (Just con_res_ty)
        | n <- toList con_names
        ]
    fields = \case
      PrefixCon args -> map (FieldModifiers Nothing . fieldTypes) args
      InfixCon a b -> map (FieldModifiers Nothing . fieldTypes) [a, b]
      RecCon fs ->
        [ FieldModifiers (Just (unLoc (foLabel (unLoc n)))) (fieldTypes cdrf_spec)
        | L _ HsConDeclRecField {..} <- unLoc fs
        , n <- cdrf_names
        ]
    fieldTypes CDF {cdf_multiplicity = HsModifiedFunArr _ ms arr} =
      types ms <> case arr of
        HsLinearArr {} -> [noLocA $ HsTyVar NoEpTok IsPromoted $ noLocA $ noUserRdr oneDataConName]
        _ -> []
    types = map (\(L _ (HsModifier _ t)) -> t)

{- | Save annotations in both the current typechecking environment and the
interface-file payload. Repeated activation by the two public plugins is safe.
-}
annotateModifiers :: TcGblEnv -> [DatatypeModifiers] -> TcM TcGblEnv
annotateModifiers env datatypes = do
  anns <- traverse annotate fresh
  fieldAnns <- traverse annotateFields freshFields
  let allAnns = anns <> fieldAnns
  pure
    env
      { tcg_anns = tcg_anns env <> allAnns
      , tcg_ann_env = extendAnnEnvList (tcg_ann_env env) allAnns
      }
  where
    entities = concatMap (\DatatypeModifiers {..} -> (datatypeName, datatypeModifiers) : concatMap conEntities datatypeConstructors) datatypes
    conEntities ConstructorModifiers {..} = (constructorName, constructorModifiers) : [(n, fieldModifiers) | FieldModifiers {fieldName = Just n, ..} <- constructorFields]
    -- A record selector shared by several constructors has one Name. Preserve
    -- all occurrences, in source order, rather than overwrite an earlier one.
    grouped = [(n, concatMap snd xs) | xs@((n, _) : _) <- groupBy (\a b -> fst a == fst b) (sortOn fst entities)]
    fresh = [(n, ts) | (n, ts) <- grouped, null (findAnns deserializeWithData (tcg_ann_env env) (NamedTarget n) :: [ModifierSyntaxAnnotation])]
    annotate (n, ts) = case traverse captureType ts of
      Left message -> failWithTc $ mkTcRnUnknownMessage $ mkPlainError [] (text message)
      Right tys -> pure $ Annotation (NamedTarget n) (toSerialized serializeWithData (ModifierSyntaxAnnotation tys))
    freshFields =
      [ (constructorName, map fieldModifiers constructorFields)
      | DatatypeModifiers {..} <- datatypes
      , ConstructorModifiers {..} <- datatypeConstructors
      , null (findAnns deserializeWithData (tcg_ann_env env) (NamedTarget constructorName) :: [ConstructorFieldsSyntaxAnnotation])
      ]
    annotateFields (n, fields) = case traverse (traverse captureType) fields of
      Left message -> failWithTc $ mkTcRnUnknownMessage $ mkPlainError [] (text message)
      Right tys -> pure $ Annotation (NamedTarget n) (toSerialized serializeWithData (ConstructorFieldsSyntaxAnnotation tys))

-- | Capture shared syntax annotations without producing consumer-specific data.
modifierPlugin :: Plugin
modifierPlugin =
  defaultPlugin
    { pluginRecompile = purePlugin
    , renamedResultAction = \_ env group -> do
        env' <- annotateModifiers env (collectModifiers group)
        pure (env', group)
    }
