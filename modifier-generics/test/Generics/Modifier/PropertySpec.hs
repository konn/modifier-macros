{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Generics.Modifier.PropertySpec (test_laws) where

import Data.List.NonEmpty (NonEmpty (..))
import Fixture
import Generics.Modifier
import Test.Falsify
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)

genExample :: Gen (Example Int)
genExample =
  Gen.oneof
    ( pure None
        :| [ Some <$> number <*> number <*> number
           , More <$> Gen.oneof (pure Nothing :| [Just <$> Gen.list (Range.inclusive (0, 20)) number])
           ]
    )
  where
    number = Gen.inRange (Range.inclusive (-1000, 1000))

genTree :: Int -> Gen (Tree Bool)
genTree 0 = Leaf <$> Gen.bool False
genTree depth =
  Gen.oneof
    ((Leaf <$> Gen.bool False) :| [Branch <$> Gen.list (Range.inclusive (0, 4)) (genTree (depth - 1))])

test_laws :: TestTree
test_laws =
  testGroup
    "falsify laws"
    [ testProperty "to . from = id for sums, records and nested composition" $ do
        x <- gen genExample
        assert $ P.eq .$ ("round trip", to (from x)) .$ ("original", x)
    , testProperty "to1 . from1 = id" $ do
        x <- gen genExample
        assert $ P.eq .$ ("round trip", to1 (from1 x)) .$ ("original", x)
    , testProperty "from . to = id" $ do
        x <- gen genExample
        let r = from x :: Rep (Example Int) ()
        assert $ P.eq .$ ("round trip", from @(Example Int) (to @(Example Int) r)) .$ ("representation", r)
    , testProperty "from1 . to1 = id" $ do
        x <- gen genExample
        let r = from1 x
        assert $ P.eq .$ ("round trip", from1 (to1 r :: Example Int)) .$ ("representation", r)
    , testProperty "recursive Generic1 round trip" $ do
        x <- gen (genTree 4)
        assert $ P.eq .$ ("round trip", to1 (from1 x)) .$ ("original", x)
    ]
