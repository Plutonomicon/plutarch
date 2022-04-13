module Plutarch.ListUtilsSpec (spec) where

import Plutarch.Extra.ListUtils (pcheckSorted, preverse)
import Plutarch.Prelude

import Hedgehog (Property)
import Hedgehog.Internal.Property (propertyTest)
import Plutarch.Test
import Plutarch.Test.Property
import Plutarch.Test.Property.Gen (genInteger, genList)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "extra.listutils" $ do
    describe "properties" $ do
      describe "reverse" $ do
        it "plutarch level reversing behaves like haskell level reversing" . hedgehog . propertyTest $ prop_preverseEquiv
    pgoldenSpec $ do
      "reverse" @\ do
        "reverse_[1..5]" @| preverse # marshal [1 .. 5 :: Integer]
      "isSorted" @\ do
        "[1..10]" @| pcheckSorted # marshal [1 .. 10 :: Integer] @-> passert
        "reverse_[1..10]" @| (pnot #$ pcheckSorted #$ marshal $ reverse [1 .. 10 :: Integer]) @-> passert
        "reverse_[]" @| preverse # marshal ([] :: [Integer])

-- plutarch level reversing behaves like haskell level reversing
prop_preverseEquiv :: Property
prop_preverseEquiv = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (reverse :: [Integer] -> [Integer])
    preverse
    (genList genInteger :* Nil)
