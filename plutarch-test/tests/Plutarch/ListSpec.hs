module Plutarch.ListSpec (spec, integerList) where

import Data.List (find)

import Plutarch.List (pconvertLists, pfoldl')
import Plutarch.Prelude

import Hedgehog (Property)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (Property (propertyTest))
import qualified Hedgehog.Range as Range
import Plutarch.Test
import Plutarch.Test.Property
import Plutarch.Test.Property.Gen (genInteger, genList)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

integerList :: [Integer] -> Term s (PList PInteger)
integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs

spec :: Spec
spec = do
  describe "list" $ do
    describe "properties" $ do
      describe "find" $ do
        it "plutarch level find mirrors haskell level find" . hedgehog . propertyTest $ prop_pfindEquiv
      describe "elemAt" $ do
        it "plutarch level elemAt mirrors haskell level elemAt" . hedgehog . propertyTest $ prop_pelemAtEquiv
    pgoldenSpec $ do
      let xs10 :: Term _ (PList PInteger)
          xs10 = integerList [1 .. 10]
          numList :: Term _ (PBuiltinList PInteger)
          numList = pconstant [1 .. 5]
      "pmatch" @| pmatch (integerList [1, 3, 1]) (const perror) @-> pfails
      "phead" @| 1 #== (phead # xs10) @-> passert
      "ptail" @| integerList [2 .. 10] #== ptail # xs10 @-> passert
      "pnull" @\ do
        "empty" @| (pnull # integerList []) @-> passert
        "nonempty" @| (pnot #$ pnull # xs10) @-> passert
      "pconcat" @\ do
        "identity" @| (pconcat # xs10 # pnil #== pconcat # pnil # xs10) #&& (pconcat # pnil # xs10 #== xs10) @-> passert
      "pmap" @\ do
        "eg" @| pmap # (plam $ \x -> x + x) # xs10 #== (integerList $ fmap (* 2) [1 .. 10]) @-> passert
        "identity" @| pmap @PList # (plam $ \(x :: Term _ PInteger) -> x) # pnil #== pnil @-> passert
      "pfilter" @\ do
        "evens" @| pfilter # (plam $ \x -> pmod # x # 2 #== 0) # xs10 #== integerList [2, 4, 6, 8, 10] @-> passert
        "gt5" @| pfilter # (plam $ \x -> 5 #< x) # xs10 #== integerList [6 .. 10] @-> passert
      "pzipWith" @\ do
        "double" @| pzipWith' (+) # xs10 # xs10 #== integerList (fmap (* 2) [1 .. 10]) @-> passert
      "pfoldl" @\ do
        "nonempty" @| pfoldl # plam (-) # 0 # xs10 #== pconstant (foldl (-) 0 [1 .. 10]) @-> passert
        "nonempty-primed" @| pfoldl' (-) # 0 # xs10 #== pconstant (foldl (-) 0 [1 .. 10]) @-> passert
        "empty" @| pfoldl # plam (-) # 0 # integerList [] #== pconstant 0 @-> passert
        "empty-primed" @| pfoldl' (-) # 0 # integerList [] #== pconstant 0 @-> passert
      "elemAt" @\ do
        "elemAt_3_[1..10]" @| pelemAt # 3 # integerList [1 .. 10]
        "elemAt_0_[1..10]" @| pelemAt # 0 # integerList [1 .. 10]
        "elemAt_9_[1..10]" @| pelemAt # 9 # integerList [1 .. 10]
      "find" @\ do
        "find_(==3)_[1..4]" @| pfind # plam (#== 3) #$ integerList [1 .. 4]
        "find_(==5)_[1..4]" @| pfind # plam (#== 5) #$ integerList [1 .. 4]
      -- Two ways of matching on a list
      "x1+x2" @\ do
        -- Via HeadList and TailList only.
        "builtin" @| (phead #$ ptail # numList) + (phead # numList)
        -- Via ChooseList (twice invoked)
        "pmatch"
          @| pmatch numList
          $ \case
            PNil -> perror
            PCons x xs ->
              pmatch xs $ \case
                PNil -> perror
                PCons y _ ->
                  x + y
      -- Various ways of uncons'ing a list
      "uncons" @\ do
        -- ChooseList builtin, like uncons but fails on null lists
        "ChooseList"
          @| pmatch numList
          $ \case
            PNil -> perror
            PCons _x xs ->
              xs
        -- Retrieving head and tail of a list
        "head-and-tail"
          @| plet (phead # numList)
          $ \_x ->
            ptail # numList
        -- Retrieve head and tail using builtins, but fail on null lists.
        "head-and-tail-and-null"
          @| plet (pnull # numList)
          $ \isEmpty ->
            pmatch isEmpty $ \case
              PTrue -> perror
              PFalse -> plet (phead # numList) $ \_x ->
                ptail # numList

-- plutarch level find mirrors haskell level find
prop_pfindEquiv :: Property
prop_pfindEquiv =
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (find @[] @Integer even)
    (pfind # peven)
    (genList genInteger :* Nil)
  where
    pevenPPlutus' s => Term s (PInteger #-> PBool)
    peven = plam $ \n -> pmod # n # 2 #== 0

-- plutarch level elemAt mirrors haskell level elemAt
prop_pelemAtEquiv :: Property
prop_pelemAtEquiv =
  prop_haskEquiv
    @( 'OnBoth)
    @( 'PartialFun)
    elemAt
    pelemAt
    $ Gen.integral (Range.linear (-10) 100)
      :* Gen.list (Range.linear 0 100) genInteger
      :* Nil

elemAt :: Integer -> [Integer] -> Integer
elemAt n xs = xs !! fromInteger n
