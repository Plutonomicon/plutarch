module Plutarch.Extra.OrdSpec (spec) where

import Plutarch.Extra.Maybe (pjust, pnothing)
import Plutarch.Extra.Ord
import Plutarch.Prelude

import Data.List (nub, sort, sortBy)
import Hedgehog (Property)
import Hedgehog.Internal.Property (propertyTest)
import Plutarch.Test
import Plutarch.Test.Property
import Plutarch.Test.Property.Gen (genInteger, genList)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "extra.ordutils" $ do
    describe "properties" $ do
      it "plutarch level sort behaves like haskell level sort" . hedgehog . propertyTest $ prop_psortEquiv
      it "plutarch level sortBy behaves like haskell level sortBy" . hedgehog . propertyTest $ prop_psortByEquiv
      it "plutarch level nub, sort behaves like haskell level nub, sort" . hedgehog . propertyTest $ prop_pnubSortEquiv
      it "plutarch level merge behaves like haskell level merge" . hedgehog . propertyTest $ prop_ptryMergeEquiv
      it "plutarch level allUnique behaves like haskell level allUnique" . hedgehog . propertyTest $ prop_pallUnique
    pgoldenSpec $ do
      "psort" @\ do
        "psort_[5..1]" @| psort # (marshal $ reverse [1 .. 5 :: Integer]) #== marshal [1 .. 5 :: Integer] @-> passert
        "psort_[1..5]" @| psort # (marshal $ reverse [1 .. 5 :: Integer]) #== marshal [1 .. 5 :: Integer] @-> passert
        "psort_[3,2,5,4,1]" @| psort # (marshal $ [3, 2, 5, 4, 1 :: Integer]) #== marshal [1 .. 5 :: Integer] @-> passert
        "psort_[]" @| psort # (marshal ([] :: [Integer])) #== marshal ([] :: [Integer]) @-> passert
      "psortBy" @\ do
        "psortBy_preverseComparator_[1..5]"
          @| ( psortBy
                # (preverseComparator # pfromOrd)
                # (marshal [1 .. 5 :: Integer])
                #== (marshal $ reverse [1 .. 5 :: Integer])
             )
            @-> passert
      "pnubSort" @\ do
        "pnubSort_[3,2,5,4,1,2,4,2,5,3]"
          @| pnubSort # (marshal $ [3, 2, 5, 4, 1, 2, 4, 2, 5, 3 :: Integer]) #== marshal [1 .. 5 :: Integer] @-> passert
        "pnubSort_[]" @| pnubSort # (marshal ([] :: [Integer])) #== marshal ([] :: [Integer]) @-> passert
      "pnubSortBy" @\ do
        "pnubSortBy_preverseComparator_[3,2,5,4,1,2,4,2,5,3]"
          @| ( pnubSortBy
                # (preverseComparator # pfromOrd)
                # (marshal [3, 2, 5, 4, 1, 2, 4, 2, 5, 3 :: Integer])
                #== (marshal $ reverse [1 .. 5 :: Integer])
             )
            @-> passert
      "pallUnique" @\ do
        "pallUnique_[1,2,3,4]"
          @| pallUnique # (marshal [1, 2, 3, 4 :: Integer])
          #== (pjust #$ pconstant True) @-> passert
        "pallUnique_[1,1,2,3]"
          @| pallUnique # (marshal [1, 1, 2, 3 :: Integer])
          #== (pjust #$ pconstant False) @-> passert
        "pallUnique_[1,3,2,3]"
          @| pallUnique # (marshal [1, 3, 2, 3 :: Integer])
          #== pnothing @-> passert
        "pallUnique_[]"
          @| pallUnique # (marshal ([] :: [Integer]))
          #== (pjust #$ pconstant True) @-> passert
      "ptryMerge" @\ do
        "ptryMerge_[1,3,5,7]_[2,4,6,8]"
          @| ptryMerge
          # (marshal ([x | x <- [1 .. 8], odd x] :: [Integer]))
          # (marshal ([x | x <- [1 .. 8], even x] :: [Integer]))
          #== (marshal ([1 .. 8] :: [Integer])) @-> passert
        "ptryMerge_[1,3,5,7]_[8,6,4,2]"
          @| ptryMerge
          # (marshal ([x | x <- [1 .. 8], odd x] :: [Integer]))
          # (marshal (reverse [x | x <- [1 .. 8], even x] :: [Integer])) @-> pfails
        "ptryMerge_[7,5,3,1]_[2,4,6,8]"
          @| ptryMerge
          # (marshal (reverse [x | x <- [1 .. 8], odd x] :: [Integer]))
          # (marshal ([x | x <- [1 .. 8], even x] :: [Integer])) @-> pfails
        "ptryMerge_[1..8]_[]"
          @| ptryMerge
          # (marshal ([1 .. 8] :: [Integer]))
          # (marshal ([] :: [Integer]))
          #== (marshal ([1 .. 8] :: [Integer])) @-> passert
        "ptryMerge_[]_[]"
          @| ptryMerge
          # (marshal ([] :: [Integer]))
          # (marshal ([] :: [Integer]))
          #== (marshal ([] :: [Integer])) @-> passert

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ xs [] = xs
mergeBy _ [] ys = ys
mergeBy f (x : xs) (y : ys)
  | f x y /= GT = x : mergeBy f xs (y : ys)
  | otherwise = y : mergeBy f (x : xs) ys

merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

prop_psortEquiv :: Property
prop_psortEquiv = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (sort :: [Integer] -> [Integer])
    psort
    (genList genInteger :* Nil)

prop_psortByEquiv :: Property
prop_psortByEquiv = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (sortBy (\x y -> reverseComp $ compare x y) :: [Integer] -> [Integer])
    (psortBy # pcond)
    (genList genInteger :* Nil)
  where
    reverseComp LT = GT
    reverseComp GT = LT
    reverseComp EQ = EQ
    pcond = preverseComparator # pfromOrd

prop_ptryMergeEquiv :: Property
prop_ptryMergeEquiv = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    ((\x y -> merge (sort x) (sort y)) :: [Integer] -> [Integer] -> [Integer])
    (plam $ \x y -> ptryMerge # (psort # x) # (psort # y))
    (genList genInteger :* genList genInteger :* Nil)

prop_pnubSortEquiv :: Property
prop_pnubSortEquiv = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    ((sort . nub) :: [Integer] -> [Integer])
    pnubSort
    (genList genInteger :* Nil)

prop_pallUnique :: Property
prop_pallUnique = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (hallUnique :: [Integer] -> Maybe Bool)
    pallUnique
    (genList genInteger :* Nil)
  where
    hallUnique x =
      if x == sort x
        then Just $ length x == length (nub x)
        else Nothing
