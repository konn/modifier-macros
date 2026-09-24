{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.ModifierSpec (test_roundTrips, test_metadata) where

import BothPlugins
import Data.Monoid (Sum)
import Data.Type.Equality ((:~:) (Refl))
import Fixture
import GHC.Exts (Multiplicity (One))
import Generics.Modifier
import Language.Haskell.TH (pprint, stringE)
import Language.Haskell.TH.Modifier (reifyModifier)
import Language.Haskell.TH.Modifier qualified as THM
import Test.Tasty
import Test.Tasty.HUnit

type family DatatypeMods r where
  DatatypeMods (D1 (MetaData n m p nt ms) f) = ms

type family ConstructorMods r where
  ConstructorMods (C1 (MetaCons n fix rec ms) f) = ms

type family SelectorMods r where
  SelectorMods (S1 (MetaSel n u s d ms) f) = ms

type family Body r where
  Body (M1 i m f) = f

type family Constructors r where
  Constructors (D1 m f) = f

type family FirstConstructor r where
  FirstConstructor (l :+: r) = FirstConstructor l
  FirstConstructor (C1 m f) = C1 m f

type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

type family AllConstructors r where
  AllConstructors (l :+: r) = Append (AllConstructors l) (AllConstructors r)
  AllConstructors (C1 (MetaCons n fix rec ms) f) = '[ '(n, ms, AllFields f)]
  AllConstructors V1 = '[]

type family AllFields r where
  AllFields (l :*: r) = Append (AllFields l) (AllFields r)
  AllFields (S1 (MetaSel n u s d ms) f) = '[ms]
  AllFields U1 = '[]

allFieldsWitness ::
  AllConstructors (Body (Rep (Example Bool)))
    :~: '[ '("None", '[Mod "none"], '[])
         , '("Some", '[Mod False, Mod "some"], '[ '[Mod "first", Mod True, Mod (Sum Int), Mod "first"], '[Mod 42, Mod False], '[Mod 42, Mod False]])
         , '("More", '[], '[ '[]])
         ]
allFieldsWitness = Refl

emptyWitness :: Body (Rep Empty) :~: V1
emptyWitness = Refl

gadtParameterWitness :: SelectorMods (Body (Body (Rep (ParamGadt Int)))) :~: '[Mod (Maybe Int)]
gadtParameterWitness = Refl

linearWitness :: SelectorMods (Body (Body (Rep Linear))) :~: '[Mod One]
linearWitness = Refl

kindWitness :: DatatypeMods (Rep KindZoo) :~: '[Mod Maybe, Mod Eq, Mod (Eq Int), Mod (Just "tag"), Mod 'x', Mod '[True, False]]
kindWitness = Refl

datatypeWitness :: DatatypeMods (Rep (Example Int)) :~: '[Mod "datatype", Mod True, Mod (Sum Int)]
datatypeWitness = Refl

constructorWitness :: ConstructorMods (FirstConstructor (Constructors (Rep (Example Int)))) :~: '[Mod "none"]
constructorWitness = Refl

positionalWitness ::
  Body (Rep Positional)
    :~: ( C1
            (MetaCons "Positional" PrefixI False '[Mod "positional"])
            ( S1 (MetaSel Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy '[Mod "left", Mod True]) (Rec0 Int)
                :*: S1 (MetaSel Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy '[Mod 42]) (Rec0 Bool)
            )
        )
positionalWitness = Refl

parameterWitness :: SelectorMods (Body (Body (Rep1 Dependent))) :~: '[Mod (Maybe Parameter)]
parameterWitness = Refl

instantiatedWitness :: SelectorMods (Body (Body (Rep (Dependent Bool)))) :~: '[Mod (Maybe Bool)]
instantiatedWitness = Refl

test_roundTrips :: TestTree
test_roundTrips =
  testGroup
    "round trips"
    [ testCase "all sum branches and nested Generic1 composition" $
        mapM_
          ( \x -> do
              to (from x) @?= x
              to1 (from1 x) @?= x
          )
          [None, Some 4 True False, More (Just [True, False])]
    , testCase "polykinded Generic1" $ to1 (from1 (Phantom :: Phantom True)) @?= Phantom
    , testCase "positional modifiers" $ to (from (Positional 4 True)) @?= Positional 4 True
    , testCase "newtypes" $ to1 (from1 (Wrapped True)) @?= Wrapped True
    , testCase "recursive Generic1" $ let t = Branch [Leaf True, Branch []] in to1 (from1 t) @?= t
    , testCase "multiple names in a GADT declaration" $ mapM_ (\x -> to (from x) @?= x) [GadtA 1, GadtB 2]
    , testCase "infix constructors" $ to1 (from1 (True :^: False)) @?= True :^: False
    ]

test_metadata :: TestTree
test_metadata =
  testGroup
    "metadata"
    [ testCase "all constructor and field slots" $ case allFieldsWitness of Refl -> pure ()
    , testCase "empty datatype" $ case emptyWitness of Refl -> pure ()
    , testCase "heterogeneous datatype modifiers" $ case datatypeWitness of Refl -> pure ()
    , testCase "GADT parameter binding" $ case gadtParameterWitness of Refl -> pure ()
    , testCase "linear arrow metadata" $ case linearWitness of Refl -> pure ()
    , testCase "higher and constraint kinds" $ case kindWitness of Refl -> pure ()
    , testCase "constructor modifiers" $ case constructorWitness of Refl -> pure ()
    , testCase "positional selector metadata" $ case positionalWitness of Refl -> pure ()
    , testCase "Generic1 parameter metadata" $ case parameterWitness of Refl -> pure ()
    , testCase "instantiated parameter metadata" $ case instantiatedWitness of Refl -> pure ()
    , testCase "standard metadata queries" $ do
        datatypeName (from (Wrapped True)) @?= "Wrapped"
        isNewtype (from (Wrapped True)) @?= True
        conName (unM1 (from (Wrapped True))) @?= "Wrapped"
        selName (unM1 (unM1 (from (Wrapped True)))) @?= "unwrap"
    , testCase "generic plugin also makes TH metadata available" $
        $(reifyModifier ''Example >>= stringE . show . length) @?= "3"
    , testCase "both plugins preserve a single copy of annotations" $
        $(reifyModifier ''Both >>= stringE . show . length) @?= "1"
    , testCase "generic plugin captures positional fields for structured TH" $
        $(THM.reifyConstructor 'Positional >>= stringE . show . map (length . THM.fieldModifiers) . THM.constructorFields) @?= "[2,1]"
    , testCase "both plugins preserve a single copy of structured field annotations" $
        $(THM.reifyConstructor 'BothFields >>= stringE . pprint . concatMap THM.fieldModifiers . THM.constructorFields) @?= "\"field\""
    , testCase "all repeated field modifiers retained" $
        $(reifyModifier 'first >>= stringE . show . length) @?= "4"
    , testCase "grouped record fields" $
        $(reifyModifier 'second >>= stringE . pprint) @?= $(reifyModifier 'third >>= stringE . pprint)
    ]
