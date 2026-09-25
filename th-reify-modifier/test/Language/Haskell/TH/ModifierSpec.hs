{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.ModifierSpec (test_reification) where

import Data.Monoid (Sum)
import FieldFixtures
import Fixture
import Language.Haskell.TH
import Language.Haskell.TH.Modifier
import LocalFixture (localModifiers)
import Test.Tasty
import Test.Tasty.HUnit

test_reification :: TestTree
test_reification =
  testGroup
    "reification"
    [ testCase "imported datatype: heterogeneous modifiers, order, duplicates" $
        $( do
             actual <- reifyModifier ''Tagged
             expected <- sequence [[t|"type"|], [t|True|], [t|Sum Int|], [t|"type"|]]
             if actual == expected then [|pure ()|] else fail (show actual <> " /= " <> show expected)
         )
    , testCase "constructor namespace" $
        $( do
             actual <- reifyModifier 'Tagged
             expected <- sequence [[t|"constructor"|], [t|False|]]
             if actual == expected then [|pure ()|] else fail (show actual)
         )
    , testCase "record field types retain variables and global names" $
        $( do
             ts <- reifyModifier 'tagged
             VarI _ (ForallT binders _ _) _ <- reify 'tagged
             let binderName (PlainTV n _) = n
                 binderName (KindedTV n _ _) = n
             case ts of
               [LitT (StrTyLit "field"), LitT (NumTyLit 42), AppT (ConT maybeName) (VarT a)]
                 | maybeName == ''Maybe, a `elem` map binderName binders -> [|pure ()|]
               _ -> fail (show ts)
         )
    , testCase "empty captured modifiers" $
        $( do
             a <- reifyModifier ''Plain
             b <- reifyModifier 'Plain
             c <- reifyModifier 'untouched
             if null (a <> b <> c) then [|pure ()|] else fail (show (a, b, c))
         )
    , testCase "local declaration before group boundary" $
        localModifiers @?= "\"local\""
    , testCase "multiple GADT constructor names" $
        $( do
             a <- reifyModifier 'GadtA
             b <- reifyModifier 'GadtB
             if a == [LitT (StrTyLit "group")] && a == b then [|pure ()|] else fail (show (a, b))
         )
    , testCase "shared selector keeps both occurrences" $
        $( do
             ts <- reifyModifier 'shared
             if ts == [LitT (StrTyLit "first"), LitT (StrTyLit "second")] then [|pure ()|] else fail (show ts)
         )
    , testCase "multiplicity modifiers are retained" $
        $( do
             ts <- reifyModifier 'Positional
             case ts of
               [PromotedT n, LitT (StrTyLit "linear constructor")] | nameBase n == "One" -> [|pure ()|]
               _ -> fail (show ts)
         )
    , testCase "duplicate record labels without selectors" $
        $( do
             TyConI (DataD _ _ _ _ [RecC _ [(oneField, _, _)]] _) <- reify ''One
             TyConI (DataD _ _ _ _ [RecC _ [(twoField, _, _)]] _) <- reify ''Two
             oneMods <- reifyModifier oneField
             twoMods <- reifyModifier twoField
             case (oneMods, twoMods) of
               (LitT (StrTyLit "one") : _, LitT (StrTyLit "two") : _) -> [|pure ()|]
               _ -> fail (show (oneMods, twoMods))
         )
    , testCase "missing capture fails instead of reporting an empty list" $
        $(recover [|pure ()|] (reifyModifier ''Bool >> [|assertFailure "unexpected metadata for Bool"|]))
    ]
