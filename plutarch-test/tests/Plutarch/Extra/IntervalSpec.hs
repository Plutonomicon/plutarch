module Plutarch.Extra.IntervalSpec (spec) where

import Plutarch.Api.V1.Interval (PInterval)
import Plutarch.Extra.Interval (
  pafter,
  palways,
  pbefore,
  pcontains,
  pfrom,
  phull,
  pintersection,
  pinterval,
  pmember,
  pnever,
  psingleton,
  pto,
 )
import Plutarch.Prelude hiding (psingleton, pto)

import Hedgehog (Property, PropertyT, assert, forAll, property)
import qualified Hedgehog.Gen as Gen (int, list)
import Hedgehog.Internal.Property (propertyTest)
import qualified Hedgehog.Range as Range (constantBounded, singleton)
import Plutarch.Test (passert, passertNot, pgoldenSpec, psucceeds, (@->), (@\), (@|))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "extra.intervalutils" $ do
    describe "fixtures" $ do
      let i1PPlutus' s => Term s (PInterval PInteger)
          i1 = mkInterval 1 2
          i2PPlutus' s => Term s (PInterval PInteger)
          i2 = mkInterval 3 5
          i3PPlutus' s => Term s (PInterval PInteger)
          i3 = mkInterval 2 4
          i4PPlutus' s => Term s (PInterval PInteger)
          i4 = mkInterval 4 4
          i5PPlutus' s => Term s (PInterval PInteger)
          i5 = mkInterval 3 4
      pgoldenSpec $ do
        "constants" @\ do
          "always" @| palways @PInteger @-> psucceeds
          "never" @| pnever @PInteger @-> psucceeds
        "contains" @\ do
          "in interval" @| pcontains # i2 # i4 @-> passert
          "out interval" @| pcontains # i4 # i2 @-> passertNot
          "always" @| pcontains # palways @PInteger # i1 @-> passert
          "never" @| pcontains # pnever @PInteger # i1 @-> passertNot
        "member" @\ do
          "[b,c], a < b" @| pmember # pconstantData 1 # i3 @-> passertNot
          "[b,c], a = b" @| pmember # pconstantData 2 # i3 @-> passert
          "[b,c], a > b, a < c" @| pmember # pconstantData 3 # i3 @-> passert
          "[b,c], a = c" @| pmember # pconstantData 4 # i3 @-> passert
          "[b,c], a > c" @| pmember # pconstantData 5 # i3 @-> passertNot
        "hull" @\ do
          let theHullPPlutus' s => Term s (PInterval PInteger)
              theHull = phull # (psingleton # pconstantData 3) # (psingleton # pconstantData 5)
          "hull 3 5 contains 3 5" @| pcontains # theHull # i2 @-> passert
          "2 not member of hull 3 5" @| pmember # pconstantData 2 # theHull @-> passertNot
          "6 not member of hull 3 5" @| pmember # pconstantData 2 # theHull @-> passertNot
        "intersection" @\ do
          "intesection [2,4] [3,5] contains [3,4]"
            @| pcontains # (pintersection # i3 # i2) # i5
          "intesection [3,5] [2,4] contains [3,4]"
            @| pcontains # (pintersection # i2 # i3) # i5

    describe "member" $ do
      it "a is a member of [b, c] iff b <= a and a <= c" . hedgehog
        . propertyTest
        $ prop_member
    describe "always" $ do
      it "always contains everything" . hedgehog . propertyTest $ prop_always
    describe "never" $ do
      it "never contains nothing" . hedgehog . propertyTest $ prop_never
    describe "hull" $ do
      it "hull of a and b contains a and b" . hedgehog . propertyTest $
        prop_hull
    describe "intersection" $ do
      it "intersection of a and b is contained in a and b" . hedgehog
        . propertyTest
        $ prop_intersection
    describe "contains" $ do
      describe "contains on bounded intervals" $ do
        it "[a, b] contains [c, d] iff a <= c and d <= b" . hedgehog
          . propertyTest
          $ prop_containsBounded
      describe "contains on unbounded (from above) intervals" $ do
        it "[a, inf] contains [c, d] iff a <= c" . hedgehog
          . propertyTest
          $ prop_containsUnboundedUpper
      describe "contains on unbounded (from below) intervals" $ do
        it "[-inf, b] contains [c, d] iff d <= b" . hedgehog
          . propertyTest
          $ prop_containsUnboundedLower
    describe "before" $ do
      it "a is before [b, c] iff a < b" . hedgehog
        . propertyTest
        $ prop_before
    describe "after" $ do
      it "a is after [b, c] iff c < a" . hedgehog
        . propertyTest
        $ prop_after

prop_member :: Property
prop_member = property $ do
  [a, b, c] <- genIntegerList 3
  assert $ checkMember a b c

prop_always :: Property
prop_always = property $ do
  [a, b] <- genIntegerList 2
  assert $ checkAlways a b

prop_never :: Property
prop_never = property $ do
  [a, b] <- genIntegerList 2
  assert $ checkNever a b

prop_hull :: Property
prop_hull = property $ do
  [a, b, c, d] <- genIntegerList 4
  assert $ checkHull a b c d

prop_intersection :: Property
prop_intersection = property $ do
  [a, b, c, d] <- genIntegerList 4
  assert $ checkIntersection a b c d

prop_containsBounded :: Property
prop_containsBounded = property $ do
  [a, b, c, d] <- genIntegerList 4
  assert $ checkBoundedContains a b c d

prop_containsUnboundedUpper :: Property
prop_containsUnboundedUpper = property $ do
  [a, b, c] <- genIntegerList 3
  assert $ checkUnboundedUpperContains a b c

prop_containsUnboundedLower :: Property
prop_containsUnboundedLower = property $ do
  [a, b, c] <- genIntegerList 3
  assert $ checkUnboundedLowerContains a b c

prop_before :: Property
prop_before = property $ do
  [a, b, c] <- genIntegerList 3
  assert $ checkBefore a b c

prop_after :: Property
prop_after = property $ do
  [a, b, c] <- genIntegerList 3
  assert $ checkAfter a b c

checkMember :: Integer -> Integer -> Integer -> Bool
checkMember a b c = actual == expected
  where
    iPPlutus' s => Term s (PInterval PInteger)
    i = mkInterval b c

    actual = plift $ pmember # pconstantData a # i
    expected = (min b c <= a) && (a <= max b c)

checkAlways :: Integer -> Integer -> Bool
checkAlways a b = plift $ pcontains # palways # i
  where
    iPPlutus' s => Term s (PInterval PInteger)
    i = mkInterval a b

checkNever :: Integer -> Integer -> Bool
checkNever a b = not (plift $ pcontains # pnever # i)
  where
    iPPlutus' s => Term s (PInterval PInteger)
    i = mkInterval a b

checkHull :: Integer -> Integer -> Integer -> Integer -> Bool
checkHull a b c d = plift $ (pcontains # i3 # i1) #&& (pcontains # i3 # i2)
  where
    i1PPlutus' s => Term s (PInterval PInteger)
    i1 = mkInterval a b

    i2PPlutus' s => Term s (PInterval PInteger)
    i2 = mkInterval c d

    i3PPlutus' s => Term s (PInterval PInteger)
    i3 = phull # i1 # i2

checkIntersection :: Integer -> Integer -> Integer -> Integer -> Bool
checkIntersection a b c d = plift $ (pcontains # i1 # i3) #&& (pcontains # i2 # i3)
  where
    i1PPlutus' s => Term s (PInterval PInteger)
    i1 = mkInterval a b

    i2PPlutus' s => Term s (PInterval PInteger)
    i2 = mkInterval c d

    i3PPlutus' s => Term s (PInterval PInteger)
    i3 = pintersection # i1 # i2

checkBoundedContains :: Integer -> Integer -> Integer -> Integer -> Bool
checkBoundedContains a b c d = actual == expected
  where
    i1PPlutus' s => Term s (PInterval PInteger)
    i1 = mkInterval a b
    i2PPlutus' s => Term s (PInterval PInteger)
    i2 = mkInterval c d

    expected :: Bool
    expected = (min a b <= min c d) && (max c d <= max a b)

    actual' :: ClosedTerm PBool
    actual' = pcontains # i1 # i2
    actual = plift actual'

checkUnboundedUpperContains :: Integer -> Integer -> Integer -> Bool
checkUnboundedUpperContains a b c = actual == expected
  where
    i1PPlutus' s => Term s (PInterval PInteger)
    i1 = pfrom # pconstantData a
    i2PPlutus' s => Term s (PInterval PInteger)
    i2 = mkInterval b c

    expected :: Bool
    expected = a <= min b c

    actual' :: ClosedTerm PBool
    actual' = pcontains # i1 # i2
    actual = plift actual'

checkUnboundedLowerContains :: Integer -> Integer -> Integer -> Bool
checkUnboundedLowerContains a b c = actual == expected
  where
    i1PPlutus' s => Term s (PInterval PInteger)
    i1 = pto # pconstantData a
    i2PPlutus' s => Term s (PInterval PInteger)
    i2 = mkInterval b c

    expected :: Bool
    expected = a >= max b c

    actual' :: ClosedTerm PBool
    actual' = pcontains # i1 # i2
    actual = plift actual'

checkBefore :: Integer -> Integer -> Integer -> Bool
checkBefore a b c = actual == expected
  where
    iPPlutus' s => Term s (PInterval PInteger)
    i = mkInterval b c

    expected :: Bool
    expected = a < min b c

    actual' :: ClosedTerm PBool
    actual' = pbefore # pconstant a # i
    actual = plift actual'

checkAfter :: Integer -> Integer -> Integer -> Bool
checkAfter a b c = actual == expected
  where
    iPPlutus' s => Term s (PInterval PInteger)
    i = mkInterval b c

    expected :: Bool
    expected = max b c < a

    actual' :: ClosedTerm PBool
    actual' = pafter # pconstant a # i
    actual = plift actual'

mkInterval :: forall s. Integer -> Integer -> Term s (PInterval PInteger)
mkInterval a' b' = pinterval # pconstantData a # pconstantData b
  where
    a = min a' b'
    b = max a' b'

genIntegerList :: Monad m => Int -> PropertyT m [Integer]
genIntegerList n =
  (fmap . fmap) toInteger $
    forAll $
      Gen.list
        (Range.singleton n)
        (Gen.int Range.constantBounded)
