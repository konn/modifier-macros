{-# LANGUAGE OverloadedStrings #-}

module Generics.Modifier.MonoidSpec (test_monoidDeriving, test_monoidLaws) where

import Data.List.NonEmpty (NonEmpty (..))
-- The direct generic helpers need the newtype constructors for Coercible.
import Data.Monoid (Dual (..), Product (..), Sum (..))
import Generics.Modifier (genericMappend, genericMempty)
import Generics.Modifier.MonoidFixtures
import Test.Falsify
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

test_monoidDeriving :: TestTree
test_monoidDeriving =
  testGroup
    "modifier-aware Semigroup and Monoid deriving"
    [ testCase "record fields use Sum, Product, ordinary append and Dual" $
        Record 2 3 [True] [True, False] <> Record 4 5 [False] [False, True]
          @?= Record 6 15 [True, False] [False, True, True, False]
    , testCase "positional fields use their specified and ordinary instances" $
        Positional 2 True [True] <> Positional 3 False [False]
          @?= Positional 5 False [True, False]
    , testCase "unrelated modifiers before and after MonoidVia are ignored" $
        Tagged 2 [1] <> Tagged 3 [2] @?= Tagged 5 [1, 2]
    , testCase "record mempty uses each field's identity" $
        mempty @(Record Bool) @?= Record 0 1 [] []
    , testCase "positional mempty uses each field's identity" $
        mempty @Positional @?= Positional 0 True []
    , testCase "unrelated modifiers do not affect mempty" $
        mempty @Tagged @?= Tagged 0 []
    , testCase "Semigroup deriving supports fields without Monoid instances" $
        SemigroupOnly (1 :| [2]) (3 :| [4]) <> SemigroupOnly (5 :| [6]) (7 :| [8])
          @?= SemigroupOnly (1 :| [2, 5, 6]) (7 :| [8, 3, 4])
    , testCase "genericMappend applies field modifiers directly" $
        genericMappend (Record 2 3 [True] [True]) (Record 4 5 [False] [False])
          @?= Record 6 15 [True, False] [False, True]
    , testCase "genericMempty applies field modifiers directly" $
        genericMempty @(Record Bool) @?= Record 0 1 [] []
    , testCase "mconcat combines all fields in order" $
        mconcat [Record 2 3 [1] [1], Record 4 5 [2] [2], Record 6 7 [3] [3]]
          @?= Record 12 105 [1, 2, 3 :: Int] [3, 2, 1]
    , testCase "mconcat of an empty list uses the derived identity" $
        mconcat [] @?= Record 0 1 [] ([] :: [Int])
    ]

genRecord :: Gen (Record Int)
genRecord = Record <$> number <*> number <*> values <*> values
  where
    number = Gen.inRange (Range.inclusive (-10, 10))
    values = Gen.list (Range.inclusive (0, 10)) number

genPositional :: Gen Positional
genPositional =
  Positional
    <$> Gen.inRange (Range.inclusive (-10, 10))
    <*> Gen.bool False
    <*> Gen.list (Range.inclusive (0, 10)) (Gen.bool False)

monoidLaws :: (Eq a, Show a, Monoid a) => String -> Gen a -> TestTree
monoidLaws name values =
  testGroup
    name
    [ testProperty "associativity" $ do
        x <- gen values
        y <- gen values
        z <- gen values
        assert $ P.eq .$ ("(x <> y) <> z", (x <> y) <> z) .$ ("x <> (y <> z)", x <> (y <> z))
    , testProperty "left identity" $ do
        x <- gen values
        assert $ P.eq .$ ("mempty <> x", mempty <> x) .$ ("x", x)
    , testProperty "right identity" $ do
        x <- gen values
        assert $ P.eq .$ ("x <> mempty", x <> mempty) .$ ("x", x)
    ]

test_monoidLaws :: TestTree
test_monoidLaws =
  testGroup
    "derived monoid laws"
    [ monoidLaws "record" genRecord
    , monoidLaws "positional" genPositional
    , testProperty "record append agrees with fieldwise operations" $ do
        x <- gen genRecord
        y <- gen genRecord
        let expected =
              Record
                (total x + total y)
                (power x * power y)
                (forwards x <> forwards y)
                (backwards y <> backwards x)
        assert $ P.eq .$ ("derived append", x <> y) .$ ("fieldwise append", expected)
    ]
