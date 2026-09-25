{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Modifier.DatatypeSpec (test_datatypes) where

import Data.List (find)
import Data.Monoid (Sum)
import Data.Proxy (Proxy)
import DatatypeFixtures
import FieldFixtures qualified as Fields
import Fixture
import GHC.Exts (Multiplicity (One))
import Language.Haskell.TH
import Language.Haskell.TH.Datatype qualified as D
import Language.Haskell.TH.Modifier
import Language.Haskell.TH.Syntax (liftData)
import LocalFixture (Local (..), localDatatypeInfo)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

test_datatypes :: TestTree
test_datatypes =
  testGroup
    "structured reification"
    [ testCase "datatype and constructor modifiers retain order, kinds and duplicates" $ do
        let info = $(reifyDatatype ''Tagged >>= liftData)
        datatypeName info @?= ''Tagged
        datatypeVariant info @?= Datatype
        datatypeContext info @?= []
        datatypeModifiers info @?= [str "type", PromotedT 'True, AppT (ConT ''Sum) (ConT ''Int), str "type"]
        withConstructor 'Tagged info $ \con -> do
          constructorModifiers con @?= [str "constructor", PromotedT 'False]
          constructorVariant con @?= RecordConstructor ['tagged, 'untouched]
          case (datatypeVars info, constructorFields con) of
            ([a], [tag, plain]) -> do
              fieldName tag @?= Just 'tagged
              fieldType tag @?= VarT (binderName a)
              fieldModifiers tag @?= [str "field", LitT (NumTyLit 42), AppT (ConT ''Maybe) (fieldType tag)]
              fieldName plain @?= Just 'untouched
              fieldType plain @?= ConT ''Bool
              fieldModifiers plain @?= []
            other -> assertFailure (show other)
    , testCase "shared record selectors keep modifiers per occurrence" $ do
        let info = $(reifyDatatype ''Shared >>= liftData)
        map constructorName (datatypeCons info) @?= ['First, 'Second]
        withConstructor 'First info $ \con -> withField con $ \field -> do
          fieldName field @?= Just 'shared
          fieldModifiers field @?= [str "first"]
        withConstructor 'Second info $ \con -> withField con $ \field -> do
          fieldName field @?= Just 'shared
          fieldModifiers field @?= [str "second"]
    , testCase "positional fields include explicit linear multiplicities" $ do
        let con = $(reifyConstructor 'Positional >>= liftData)
        constructorName con @?= 'Positional
        constructorVariant con @?= NormalConstructor
        constructorModifiers con @?= [PromotedT 'One, str "linear constructor"]
        withField con $ \field -> do
          fieldName field @?= Nothing
          fieldType field @?= ConT ''Int
          fieldModifiers field @?= [PromotedT 'One, str "field"]
    , testCase "GADT modifiers use th-abstraction's normalized datatype binders" $ do
        let info = $(reifyDatatype ''Indexed >>= liftData)
        withConstructor 'Indexed info $ \con -> withField con $ \field -> do
          constructorModifiers con @?= [str "indexed", AppT (ConT ''Maybe) (fieldType field)]
          fieldModifiers field @?= [AppT (ConT ''Maybe) (fieldType field)]
          constructorContext con @?= [AppT (ConT ''Show) (fieldType field)]
          constructorVars con @?= []
          case fieldType field of
            VarT v -> assertBool "GADT variable is bound" (v `elem` map binderName (datatypeVars info))
            other -> assertFailure (show other)
    , testCase "GADT refinements and positional modifier duplicates are retained" $ do
        let con = $(reifyConstructor 'Refined >>= liftData)
        case constructorContext con of
          [AppT (AppT EqualityT (VarT _)) (ConT bool)] -> bool @?= ''Bool
          other -> assertFailure (show other)
        constructorModifiers con @?= [str "refined"]
        withField con $ \field -> do
          fieldName field @?= Nothing
          fieldModifiers field @?= [str "number", PromotedT 'True, str "number"]
    , testCase "existential and datatype variables remain distinct" $ do
        let info = $(reifyDatatype ''Existential >>= liftData)
        withConstructor 'Exists info $ \con ->
          case (datatypeVars info, constructorVars con, constructorFields con) of
            ([a], [b], [hiddenField, visibleField]) -> do
              assertBool "distinct variables" (binderName a /= binderName b)
              fieldType hiddenField @?= VarT (binderName b)
              fieldType visibleField @?= VarT (binderName a)
              fieldModifiers hiddenField @?= [AppT (ConT ''Maybe) (fieldType hiddenField)]
              fieldModifiers visibleField @?= [AppT (ConT ''Maybe) (fieldType visibleField)]
              constructorContext con @?= [AppT (ConT ''Show) (fieldType hiddenField)]
            other -> assertFailure (show other)
    , testCase "GADT existential shadowing and quantified modifiers avoid capture" $ do
        let info = $(reifyDatatype ''Shadow >>= liftData)
        withConstructor 'Shadow info $ \con -> withField con $ \field ->
          case (fieldType field, fieldModifiers field) of
            (VarT outer, [maybeOuter, SigT (ForallT [inner] [] (AppT (AppT ArrowT (VarT x)) (VarT y))) _]) -> do
              maybeOuter @?= AppT (ConT ''Maybe) (VarT outer)
              assertBool "existential retained" (outer `elem` map binderName (constructorVars con))
              assertBool "datatype binder is distinct" (outer `notElem` map binderName (datatypeVars info))
              x @?= binderName inner
              y @?= binderName inner
              assertBool "quantified modifier is not captured" (outer /= binderName inner)
            other -> assertFailure (show other)
    , testCase "nullary GADT constructor modifiers are normalized" $ do
        let info = $(reifyDatatype ''Nullary >>= liftData)
        withConstructor 'Nullary info $ \con -> do
          constructorFields con @?= []
          constructorVars con @?= []
          case (reverse (datatypeVars info), constructorModifiers con) of
            (a : _, [modifier]) -> modifier @?= AppT (ConT ''Maybe) (VarT (binderName a))
            other -> assertFailure (show other)
    , testCase "grouped GADT names and grouped record fields" $ do
        let info = $(reifyDatatype ''Grouped >>= liftData)
        map constructorName (datatypeCons info) @?= ['GroupA, 'GroupB]
        mapM_
          ( \con -> do
              constructorModifiers con @?= [str "group"]
              map fieldName (constructorFields con) @?= [Just 'groupLeft, Just 'groupRight]
              map fieldModifiers (constructorFields con) @?= replicate 2 [LitT (NumTyLit 42), PromotedT 'False]
          )
          (datatypeCons info)
    , testCase "duplicate labels with NoFieldSelectors" $ do
        let one = $(reifyConstructor 'Fields.One >>= liftData)
            two = $(reifyConstructor 'Fields.Two >>= liftData)
        withField one $ \a -> withField two $ \b -> do
          fmap nameBase (fieldName a) @?= Just "same"
          fmap nameBase (fieldName b) @?= Just "same"
          assertBool "labels retain identity" (fieldName a /= fieldName b)
          fieldModifiers a @?= [str "one", AppT (ConT ''Maybe) (fieldType a)]
          fieldModifiers b @?= [str "two", AppT (ConT ''Maybe) (fieldType b)]
    , testCase "newtype and its field metadata" $ do
        let info = $(reifyDatatype ''Wrapped >>= liftData)
        datatypeVariant info @?= Newtype
        datatypeModifiers info @?= [str "newtype"]
        withConstructor 'Wrapped info $ \con -> withField con $ \field -> do
          constructorModifiers con @?= [str "wrap"]
          fieldModifiers field @?= [AppT (ConT ''Maybe) (fieldType field)]
    , testCase "empty datatype" $ do
        let info = $(reifyDatatype ''Empty >>= liftData)
        datatypeCons info @?= []
        datatypeModifiers info @?= [str "empty"]
    , testCase "unmodified nullary constructor" $ do
        let info = $(reifyDatatype ''Plain >>= liftData)
        datatypeModifiers info @?= []
        withConstructor 'Plain info $ \con -> do
          constructorFields con @?= []
          constructorModifiers con @?= []
    , testCase "infix constructor" $ do
        let con = $(reifyConstructor '(:*:) >>= liftData)
        constructorVariant con @?= InfixConstructor
        map fieldName (constructorFields con) @?= [Nothing, Nothing]
        map fieldModifiers (constructorFields con) @?= [[], []]
    , testCase "polykinded datatype modifiers share the returned binders" $ do
        let info = $(reifyDatatype ''Kinded >>= liftData)
        case reverse (datatypeVars info) of
          KindedTV a _ (VarT _) : _ -> do
            datatypeModifiers info @?= [AppT (ConT ''Proxy) (VarT a)]
            withConstructor 'Kinded info $ \con -> withField con $ \field ->
              fieldModifiers field @?= [fieldType field]
          other -> assertFailure (show other)
    , testCase "strictness and unpacking annotations" $ do
        let con = $(reifyConstructor 'StrictFields >>= liftData)
        map fieldStrictness (constructorFields con)
          @?= [FieldStrictness Unpack D.Strict, FieldStrictness UnspecifiedUnpackedness Lazy]
    , testCase "datatype lookup through a constructor" $ do
        let info = $(reifyDatatype 'Second >>= liftData)
        datatypeName info @?= ''Shared
        map constructorName (datatypeCons info) @?= ['First, 'Second]
    , testCase "datatype lookup through a shared record selector" $ do
        let info = $(reifyDatatype 'shared >>= liftData)
        datatypeName info @?= ''Shared
        map constructorName (datatypeCons info) @?= ['First, 'Second]
    , testCase "ordinary metadata agrees with th-abstraction" $ do
        let info = $(reifyDatatype ''Tagged >>= liftData)
            reference = $(D.reifyDatatype ''Tagged >>= liftData)
        datatypeName info @?= D.datatypeName reference
        datatypeContext info @?= D.datatypeContext reference
        datatypeVars info @?= D.datatypeVars reference
        datatypeInstTypes info @?= D.datatypeInstTypes reference
        datatypeReturnKind info @?= D.datatypeReturnKind reference
        datatypeVariant info @?= D.datatypeVariant reference
        case D.datatypeCons reference of
          [expected] -> withConstructor 'Tagged info $ \con -> do
            constructorVars con @?= D.constructorVars expected
            constructorContext con @?= D.constructorContext expected
            constructorVariant con @?= D.constructorVariant expected
            map fieldType (constructorFields con) @?= D.constructorFields expected
            map fieldStrictness (constructorFields con) @?= D.constructorStrictness expected
          other -> assertFailure (show other)
    , testCase "local declaration-group boundary" $ do
        datatypeName localDatatypeInfo @?= ''Local
        datatypeModifiers localDatatypeInfo @?= [str "local"]
        map constructorName (datatypeCons localDatatypeInfo) @?= ['Local]
    , testCase "uncaptured datatypes fail" $
        $(recover [|True|] (reifyDatatype ''Bool >> [|False|])) @?= True
    , testCase "type synonyms fail" $
        $(recover [|True|] (reifyDatatype ''Alias >> [|False|])) @?= True
    , testCase "reifyConstructor rejects datatype names" $
        $(recover [|True|] (reifyConstructor ''Tagged >> [|False|])) @?= True
    ]

str :: String -> Type
str = LitT . StrTyLit

binderName :: TyVarBndr flag -> Name
binderName (PlainTV n _) = n
binderName (KindedTV n _ _) = n

withConstructor :: Name -> DatatypeInfo -> (ConstructorInfo -> Assertion) -> Assertion
withConstructor name info check = case find ((== name) . constructorName) (datatypeCons info) of
  Just con -> check con
  Nothing -> assertFailure ("Missing constructor " <> show name)

withField :: ConstructorInfo -> (FieldInfo -> Assertion) -> Assertion
withField con check = case constructorFields con of
  [field] -> check field
  fields -> assertFailure ("Expected one field, got " <> show fields)
