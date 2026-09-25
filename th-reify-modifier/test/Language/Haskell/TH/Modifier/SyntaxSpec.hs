{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Modifier.SyntaxSpec (test_sharedSyntax) where

import Data.Kind qualified as Kind
import Data.Proxy (Proxy)
import GHC.Exts (Multiplicity (One))
import GHC.Modifiers.Types qualified as S
import Language.Haskell.TH
import Language.Haskell.TH.Modifier
import Language.Haskell.TH.Syntax (liftData)
import SyntaxFixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

test_sharedSyntax :: TestTree
test_sharedSyntax =
  testGroup
    "shared syntax and TH lowering"
    [ testCase "interface payload retains nested arrow modifiers and duplicates" $
        assertNestedModifiers $((reifyAnnotations (AnnLookupName ''NestedModifiers) :: Q [S.ModifierSyntaxAnnotation]) >>= liftData)
    , testCase "field occurrence payload retains nested modifiers" $
        assertNestedFields $((reifyAnnotations (AnnLookupName 'NestedModifiers) :: Q [S.ConstructorFieldsSyntaxAnnotation]) >>= liftData)
    , testCase "TH rejects unsupported datatype modifiers only when queried" $
        $(recover [|pure ()|] (reifyModifier ''NestedModifiers >> [|assertFailure "TH silently discarded nested modifiers"|]))
    , testCase "TH rejects unsupported field modifiers only when queried" $
        $(recover [|pure ()|] (reifyModifier 'nestedField >> [|assertFailure "TH silently discarded nested field modifiers"|]))
    , testCase "structured TH rejects unsupported field occurrence modifiers" $
        $(recover [|pure ()|] (reifyConstructor 'FieldOnly >> [|assertFailure "structured TH discarded nested modifiers"|]))
    , testCase "other entities remain reifiable after capture" $
        ($(reifyModifier 'NestedModifiers >>= liftData) :: [Type]) @?= []
    , testCase "type syntax lowers to TH after interface loading" $
        $( do
             actual <- reifyModifier ''TypeForms
             expected <- sequence [[t|[Int]|], [t|(Int, Bool)|], [t|'(Int, Bool)|], [t|'[Int, Bool]|]]
             -- TH quotations normalize Type to StarT; capture retains the
             -- resolved name that was written in the visible kind argument.
             let applied = AppT (AppKindT (ConT ''Proxy) (ConT ''Kind.Type)) (ConT ''Int)
             if take 5 actual == expected <> [applied] then [|pure ()|] else fail (show actual <> " /= " <> show (expected <> [applied]))
         )
    , testCase "invisible forall retains binder identity, kind and constraints" $
        $( do
             actual <- reifyModifier ''TypeForms
             case actual !! 5 of
               ForallT [KindedTV a SpecifiedSpec k] [AppT (ConT eq) (VarT b)] (AppT (AppT ArrowT (VarT c)) (VarT d))
                 | a == b, b == c, c == d, eq == ''Eq, k == ConT ''Kind.Type -> [|pure ()|]
               other -> fail (show other)
         )
    , testCase "visible forall retains required binders" $
        $( do
             actual <- reifyModifier ''TypeForms
             case actual !! 6 of
               ForallVisT [PlainTV a ()] (AppT (AppT ArrowT (VarT b)) (VarT c))
                 | a == b, b == c -> [|pure ()|]
               other -> fail (show other)
         )
    , testCase "explicit multiplicity and linear arrow both lower correctly" $
        $( do
             actual <- reifyModifier ''TypeForms
             let expected = AppT (AppT (AppT MulArrowT (PromotedT 'One)) (ConT ''Int)) (ConT ''Bool)
             if drop 7 actual == [expected, expected] then [|pure ()|] else fail (show actual)
         )
    ]

assertNestedModifiers :: [S.ModifierSyntaxAnnotation] -> Assertion
assertNestedModifiers = \case
  [S.ModifierSyntaxAnnotation [S.ParenthesizedType (S.FunctionType S.StandardArrow [tag, duplicate] _ _)]] -> do
    tag @?= duplicate
    case tag of
      S.NamedType (S.ModifierName S.TypeConstructor (S.GlobalName unit mdl occ)) -> do
        assertBool "defining package identity is retained" (not (null unit))
        mdl @?= "SyntaxFixtures"
        occ @?= "ArrowTag"
      _ -> assertFailure (show tag)
  other -> assertFailure (show other)

assertNestedFields :: [S.ConstructorFieldsSyntaxAnnotation] -> Assertion
assertNestedFields = \case
  [S.ConstructorFieldsSyntaxAnnotation [[S.ParenthesizedType (S.FunctionType S.StandardArrow [_] _ _)]]] -> pure ()
  other -> assertFailure (show other)
