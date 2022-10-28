module Plutarch.Extra.ListSpec (spec) where

import Plutarch.Extra.List (pcheckSorted, pfindJust, plookupAssoc, pmapMaybe, preplicate, preverse)
import Plutarch.Extra.Maybe (pjust, pnothing)
import Plutarch.Prelude

import Data.Maybe (mapMaybe)
import Hedgehog (Property)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (propertyTest)
import qualified Hedgehog.Range as Range
import Plutarch.Test
import Plutarch.Test.Property
import Plutarch.Test.Property.Gen (genInteger, genList)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "extra.listutils" $ do
    describe "properties" $ do
      it "plutarch level reversing behaves like haskell level reversing" . hedgehog . propertyTest $ prop_preverseEquiv
      it "plutarch level mapMaybe behaves like haskell level mapMaybe" . hedgehog . propertyTest $ prop_pmapMaybe
      it "plutarch level replicate behaves like haskell level preplicate" . hedgehog . propertyTest $ prop_preplicate
      it "plutarch level findJust behaves like haskell level findJust" . hedgehog . propertyTest $ prop_pfindJust
    pgoldenSpec $ do
      "reverse" @\ do
        "reverse_[1..5]" @| preverse # marshal [1 .. 5 :: Integer]
      "isSorted" @\ do
        "[1..10]" @| pcheckSorted # marshal [1 .. 10 :: Integer] @-> passert
        "reverse_[1..10]" @| (pnot #$ pcheckSorted #$ marshal $ reverse [1 .. 10 :: Integer]) @-> passert
        "reverse_[]" @| preverse # marshal ([] :: [Integer])
      "pmapMaybe" @\ do
        "mapMaybe_[1..5]" @| pmapMaybe # pjust # marshal [1 .. 5 :: Integer]
        "mapMaybe_[]" @| pmapMaybe # pjust # marshal ([] :: [Integer])
        "mapMaybe_[1..5]_Nothing"
          @| (0 #== (plength #$ pmapMaybe # plam (const $ pcon PNothing) # marshal [1 .. 5 :: Integer])) @-> passert
      "preplicate" @\ do
        "preplicate_5_0"
          @| (5 #== (plength #$ preplicate @PBuiltinList # marshal (5 :: Integer) # marshal (0 :: Integer))) @-> passert
        "preplicate_0_0"
          @| (0 #== (plength #$ preplicate @PBuiltinList # marshal (0 :: Integer) # marshal (0 :: Integer))) @-> passert
      "pfindJust" @\ do
        "pfindJust_2_[1..10]"
          @| (pjust # 4 #== pfindJust # plam (\x -> pif (x #== 2) (pjust #$ x + 2) pnothing) # marshal [1 .. 10 :: Integer]) @-> passert
        "pfindJust_20_[1..10]"
          @| (pnothing #== pfindJust # plam (\x -> pif (x #== 20) (pjust # x) pnothing) # marshal [1 .. 10 :: Integer]) @-> passert
        "pfindJust_[]"
          @| (pnothing #== pfindJust # plam (\x -> pjust # x) # (pnil :: Term s (PBuiltinList PInteger))) @-> passert
      "plookupAssoc" @\ do
        "plookupAssoc_3_[(1..10, 1..10)]"
          @| ( pjust # 8
                #== plookupAssoc
                # (plam $ flip pmatch \(PPair k _) -> k)
                # (plam $ flip pmatch \(PPair _ v) -> v)
                # 3
                # (marshal $ zip [1 .. 10 :: Integer] $ reverse [1 .. 10 :: Integer])
             )
            @-> passert
        "plookupAssoc_20_[(1..10, 1..10)]"
          @| ( pnothing
                #== plookupAssoc
                # (plam $ flip pmatch \(PPair k _) -> k)
                # (plam $ flip pmatch \(PPair _ v) -> v)
                # 20
                # (marshal $ zip [1 .. 10 :: Integer] $ reverse [1 .. 10 :: Integer])
             )
            @-> passert
        "plookupAssoc_[]"
          @| ( pnothing
                #== plookupAssoc
                # (plam $ flip pmatch \(PPair k _) -> k)
                # (plam $ flip pmatch \(PPair _ v) -> v)
                # 20
                # (pnil :: Term s (PList (PPair PInteger PInteger)))
             )
            @-> passert

-- plutarch level reversing behaves like haskell level reversing
prop_preverseEquiv :: Property
prop_preverseEquiv = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (reverse :: [Integer] -> [Integer])
    preverse
    (genList genInteger :* Nil)

prop_pmapMaybe :: Property
prop_pmapMaybe = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (mapMaybe hcond :: [Integer] -> [Integer])
    (pmapMaybe # pcond)
    (genList genInteger :* Nil)
  where
    pcond = plam \x ->
      pif
        (x #< 5)
        (pcon PNothing)
        (pcon . PJust $ x + 10)
    hcond x = if x < 5 then Nothing else Just $ x + 10

prop_preplicate :: Property
prop_preplicate = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (replicate . fromInteger :: Integer -> Integer -> [Integer])
    preplicate
    (manageable :* genInteger :* Nil)
  where
    manageable = Gen.integral (Range.linear 0 1_000)

prop_pfindJust :: Property
prop_pfindJust = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (findJust hcond :: [Integer] -> Maybe Integer)
    (pfindJust # pcond)
    (genList genInteger :* Nil)
  where
    findJust f (x : xs) =
      case f x of
        Nothing -> findJust f xs
        Just v -> Just v
    findJust _ [] = Nothing
    pcond = plam \x -> pif (x #< 5) (pnothing) (pjust # 300)
    hcond x = if x < 5 then Nothing else Just 300
