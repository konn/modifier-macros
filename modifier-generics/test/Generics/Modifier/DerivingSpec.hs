{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- Intentionally derive instances outside the defining module in this regression.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=Generics.Modifier.Plugin #-}

module Generics.Modifier.DerivingSpec (test_deriving) where

import Data.Type.Equality ((:~:) (Refl))
import GHC.Generics qualified as GHC
import Generics.Modifier qualified as M
import Generics.Modifier.Deriving.MissingStock
import Generics.Modifier.DerivingFixtures
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Modifier qualified as THM
import Language.Haskell.TH.Syntax (lift)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

deriving anyclass instance M.Generic (ImportedStock a)

deriving anyclass instance M.Generic1 ImportedStock

type family DocumentFieldModifiers rep where
  DocumentFieldModifiers (M.D1 _ (empty M.:+: M.C1 _ (M.S1 (M.MetaSel _ _ _ _ ms) _))) = ms

exampleRep :: DocumentFieldModifiers (M.Rep (Document Int)) :~: '[M.Mod (EncodeWith Json), M.Mod (Maybe Int), M.Mod Maybe]
exampleRep = Refl

exampleRep1 :: DocumentFieldModifiers (M.Rep1 Document) :~: '[M.Mod (EncodeWith Json), M.Mod (Maybe M.Parameter), M.Mod Maybe]
exampleRep1 = Refl

test_deriving :: TestTree
test_deriving =
  testGroup
    "explicit anyclass derivation"
    [ testCase "Generic can be requested independently" $ do
        M.to (M.from (OnlyGeneric (1 :: Int))) @?= OnlyGeneric 1
        $(TH.isInstance ''M.Generic1 [TH.ConT ''OnlyGeneric] >>= lift) @?= False
        $(TH.isInstance ''GHC.Generic1 [TH.ConT ''OnlyGeneric] >>= lift) @?= False
    , testCase "Generic1 can be requested independently" $ do
        M.to1 (M.from1 (OnlyGeneric1 (1 :: Int))) @?= OnlyGeneric1 1
        $(TH.isInstance ''M.Generic [TH.AppT (TH.ConT ''OnlyGeneric1) (TH.ConT ''Int)] >>= lift) @?= False
        $(TH.isInstance ''GHC.Generic [TH.AppT (TH.ConT ''OnlyGeneric1) (TH.ConT ''Int)] >>= lift) @?= False
    , testCase "anyclass deriving does not create stock instances" $ do
        $(TH.isInstance ''GHC.Generic [TH.AppT (TH.ConT ''AnyclassOnly) (TH.ConT ''Int)] >>= lift) @?= False
        $(TH.isInstance ''GHC.Generic1 [TH.ConT ''AnyclassOnly] >>= lift) @?= False
    , testCase "stock-only deriving does not opt into modifier classes" $ do
        $(TH.isInstance ''M.Generic [TH.AppT (TH.ConT ''StockOnly) (TH.ConT ''Int)] >>= lift) @?= False
        $(TH.isInstance ''M.Generic1 [TH.ConT ''StockOnly] >>= lift) @?= False
    , testCase "capture alone does not add stock or modifier instances" $ do
        $(TH.isInstance ''M.Generic [TH.AppT (TH.ConT ''Unrequested) (TH.ConT ''Int)] >>= lift) @?= False
        $(TH.isInstance ''M.Generic1 [TH.ConT ''Unrequested] >>= lift) @?= False
        $(TH.isInstance ''GHC.Generic [TH.AppT (TH.ConT ''Unrequested) (TH.ConT ''Int)] >>= lift) @?= False
        $(TH.isInstance ''GHC.Generic1 [TH.ConT ''Unrequested] >>= lift) @?= False
    , testCase "an unrelated class named Generic is not rewritten" $
        $(TH.isInstance ''GHC.Generic [TH.ConT ''SameName] >>= lift) @?= False
    , testCase "inline stock and anyclass deriving work together" $ do
        M.to (M.from (ExistingBoth (1 :: Int))) @?= ExistingBoth 1
        M.to1 (M.from1 (ExistingBoth (1 :: Int))) @?= ExistingBoth 1
    , testCase "standalone stock and anyclass deriving work together" $ do
        M.to (M.from (Standalone (1 :: Int))) @?= Standalone 1
        M.to1 (M.from1 (Standalone (1 :: Int))) @?= Standalone 1
    , testCase "standalone anyclass deriving uses explicit inline stock instances" $ do
        M.to (M.from (ExistingStandalone (1 :: Int))) @?= ExistingStandalone 1
        M.to1 (M.from1 (ExistingStandalone (1 :: Int))) @?= ExistingStandalone 1
    , testCase "inline anyclass deriving uses explicit standalone stock instances" $ do
        M.to (M.from (InlineWithStandaloneStock (1 :: Int))) @?= InlineWithStandaloneStock 1
        M.to1 (M.from1 (InlineWithStandaloneStock (1 :: Int))) @?= InlineWithStandaloneStock 1
    , testCase "anyclass deriving uses stock instances from previous declaration groups" $ do
        M.to (M.from (EarlierStock (1 :: Int))) @?= EarlierStock 1
        M.to1 (M.from1 (EarlierStock (1 :: Int))) @?= EarlierStock 1
    , testCase "anyclass deriving after a declaration-group boundary" $ do
        M.to (M.from (EarlierPlain (1 :: Int))) @?= EarlierPlain 1
        M.to1 (M.from1 (EarlierPlain (1 :: Int))) @?= EarlierPlain 1
    , testCase "anyclass deriving uses explicitly provided imported stock instances" $ do
        M.to (M.from (ImportedStock (1 :: Int))) @?= ImportedStock 1
        M.to1 (M.from1 (ImportedStock (1 :: Int))) @?= ImportedStock 1
    , testCase "Generic1 composition infers the required Functor context" $ do
        case nestedRoundTrip (Nested [Just (1 :: Int), Nothing]) of
          Nested xs -> xs @?= [Just 1, Nothing]
        case standaloneNestedRoundTrip (StandaloneNested [Just (1 :: Int), Nothing]) of
          StandaloneNested xs -> xs @?= [Just 1, Nothing]
    , testCase "hand-written representations coexist with derived instances" $ do
        M.from (Manual (1 :: Int)) @?= M.K1 1
        M.to (M.K1 (1 :: Int)) @?= Manual 1
        M.from1 (Manual (1 :: Int)) @?= M.Par1 1
        M.to1 (M.Par1 (1 :: Int)) @?= Manual 1
    , testCase "generic derivation accepts nested modifiers that TH cannot express" $
        M.to (M.from (NestedAnnotation 7)) @?= NestedAnnotation 7
    , testCase "TH rejects nested modifiers captured by the generic plugin only when queried" $
        $(TH.recover [|pure ()|] (THM.reifyModifier 'annotatedArrow >> [|assertFailure "TH silently discarded nested modifiers"|]))
    , testCase "ordinary type modifiers are retained in Rep and Rep1" $ do
        case exampleRep of { Refl -> pure () }
        case exampleRep1 of { Refl -> pure () }
        M.to (M.from (Document (1 :: Int))) @?= Document 1
        M.to1 (M.from1 (Document (1 :: Int))) @?= Document 1
        M.to (M.from (EmptyDocument :: Document Int)) @?= EmptyDocument
    , testCase "ordinary declaration and constructor tags are reified" $ do
        $(THM.reifyModifier ''Document >>= lift . (== [TH.ConT ''Entity])) @?= True
        $(THM.reifyModifier 'Document >>= lift . (== [TH.ConT ''RecordTag])) @?= True
        $(THM.reifyModifier 'EmptyDocument >>= lift . (== [TH.ConT ''EmptyTag])) @?= True
    ]

nestedRoundTrip :: (Functor f) => Nested f a -> Nested f a
nestedRoundTrip = M.to1 . M.from1

standaloneNestedRoundTrip :: (Functor f) => StandaloneNested f a -> StandaloneNested f a
standaloneNestedRoundTrip = M.to1 . M.from1
