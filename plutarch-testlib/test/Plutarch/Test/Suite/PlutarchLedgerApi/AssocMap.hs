module Plutarch.Test.Suite.PlutarchLedgerApi.AssocMap (tests) where

import Data.Bifunctor (bimap)
import Plutarch.LedgerApi.AssocMap (KeyGuarantees (Sorted, Unsorted), PMap)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (pmaybeToMaybeData)
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)
import Plutarch.Maybe (pjust, pmapMaybe, pnothing)
import Plutarch.Num ((#+), (#-))
import Plutarch.Prelude
import Plutarch.Test.Laws (checkLedgerPropertiesAssocMap)
import Plutarch.Test.QuickCheck (checkHaskellEquivalent2, propEval, propEvalEqual)
import Plutarch.Test.Utils (fewerTests, prettyEquals, prettyShow)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1.Orphans (UnsortedAssocMap, getUnsortedAssocMap)
import PlutusTx.AssocMap qualified as PlutusMap
import Prettyprinter (Pretty)
import Test.QuickCheck (Arbitrary, arbitrary, shrink)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (Property, forAllShrinkShow, testProperty)

tests :: TestTree
tests =
  testGroup
    "AssocMap"
    [ checkLedgerPropertiesAssocMap
    , propEval "Ledger AssocMap is sorted (sanity check for punsafeCoerce below)" $
        \(m :: PlutusMap.Map Integer Integer) -> AssocMap.passertSorted # pconstant m
    , adjustOption (fewerTests 4) $
        propEval "passertSorted . psortedMapFromFoldable" $
          \(m :: UnsortedAssocMap Integer Integer) ->
            AssocMap.passertSorted
              #$ AssocMap.psortedMapFromFoldable
                (map (bimap pconstant pconstant) $ PlutusMap.toList $ getUnsortedAssocMap m)
    , testProperty "null = pnull" $ checkHaskellUnsortedPMapEquivalent PlutusMap.null AssocMap.pnull
    , testProperty "lookup = plookup" $
        checkHaskellUnsortedPMapEquivalent2
          PlutusMap.lookup
          (plam $ \k m -> pmaybeToMaybeData #$ AssocMap.plookup # k # m)
    , testProperty "lookup = plookupData" $
        checkHaskellUnsortedPMapEquivalent2
          PlutusMap.lookup
          ( plam $ \k m ->
              pmaybeToMaybeData
                #$ (pmapMaybe # plam pfromData)
                #$ AssocMap.plookupData
                # pdata k
                # m
          )
    , testProperty "singleton = psingleton" $
        checkHaskellEquivalent2 @PInteger @PInteger
          PlutusMap.singleton
          (plam $ \k v -> AssocMap.pforgetSorted $ AssocMap.psingleton # k # v)
    , propEvalEqual @(Integer, Integer)
        "plookup k (psingleton k v) = PJust v"
        (\(k, v) -> AssocMap.plookup # pconstant k #$ AssocMap.psingleton # pconstant k # pconstant v)
        (\(_, v) -> pjust # pconstant v)
    , testProperty "foldl . toList = pfoldlWithKey" $
        forAllShrinkShow arbitrary shrink show $
          \(a :: Integer, m :: PlutusMap.Map Integer Integer) ->
            foldl (\acc (k, v) -> acc + k + v) a (PlutusMap.toList m)
              `prettyEquals` plift
                ( AssocMap.pfoldlWithKey
                    # plam (\acc k v -> acc + k + v)
                    # pconstant a
                    # (AssocMap.passertSorted # pconstant m)
                )
    , testProperty "all = pall" $
        checkHaskellUnsortedPMapEquivalent (PlutusMap.all even) (AssocMap.pall # peven)
    , testProperty "any = pany" $
        checkHaskellUnsortedPMapEquivalent (any even . PlutusMap.elems) (AssocMap.pany # peven)
    , testProperty "insert = pinsert" $
        checkHaskellSortedPMapEquivalent2
          (\(k, v) m -> PlutusMap.insert k v m)
          ( plam
              ( \kv m ->
                  AssocMap.pforgetSorted $
                    AssocMap.pinsert # (pfstBuiltin # kv) # (psndBuiltin # kv) # m
              )
          )
    , testProperty "delete = pdelete" $
        checkHaskellSortedPMapEquivalent2
          PlutusMap.delete
          (plam (\k m -> AssocMap.pforgetSorted $ AssocMap.pdelete # k # m))
    , testProperty "unionWith (+) = punionResolvingCollisionsWith Commutative (#+)" $
        forAllShrinkShow arbitrary shrink show $
          \(m1 :: PlutusMap.Map Integer Integer, m2 :: PlutusMap.Map Integer Integer) ->
            PlutusMap.unionWith (+) m1 m2
              `prettyEquals` plift
                ( AssocMap.pforgetSorted $
                    AssocMap.punionResolvingCollisionsWith
                      AssocMap.Commutative
                      # plam (#+)
                      # punsafeCoerce (pconstant m1)
                      # punsafeCoerce (pconstant m2)
                )
    , testProperty "unionWith (+) = punionResolvingCollisionsWith NonCommutative (#+)" $
        forAllShrinkShow arbitrary shrink show $
          \(m1 :: PlutusMap.Map Integer Integer, m2 :: PlutusMap.Map Integer Integer) ->
            PlutusMap.unionWith (+) m1 m2
              `prettyEquals` plift
                ( AssocMap.pforgetSorted $
                    AssocMap.punionResolvingCollisionsWith
                      AssocMap.NonCommutative
                      # plam (#+)
                      # punsafeCoerce (pconstant m1)
                      # punsafeCoerce (pconstant m2)
                )
    , testProperty "unionWith (-) = punionResolvingCollisionsWith NonCommutative (#-)" $
        forAllShrinkShow arbitrary shrink show $
          \(m1 :: PlutusMap.Map Integer Integer, m2 :: PlutusMap.Map Integer Integer) ->
            PlutusMap.unionWith (-) m1 m2
              `prettyEquals` plift
                ( AssocMap.pforgetSorted $
                    AssocMap.punionResolvingCollisionsWith
                      AssocMap.NonCommutative
                      # plam (#-)
                      # punsafeCoerce (pconstant m1)
                      # punsafeCoerce (pconstant m2)
                )
    , testProperty "mapMaybe mkEven = pmapMaybe pmkEven" $
        forAllShrinkShow arbitrary shrink show $
          \(m :: PlutusMap.Map Integer Integer) ->
            PlutusMap.mapMaybe mkEven m
              `prettyEquals` plift
                (AssocMap.pforgetSorted $ AssocMap.pmapMaybe # pmkEven # punsafeCoerce (pconstant m))
    ]

checkHaskellUnsortedPMapEquivalent ::
  forall (plutarchOutput :: S -> Type).
  ( PUnsafeLiftDecl plutarchOutput
  , Pretty (PLifted plutarchOutput)
  , Eq (PLifted plutarchOutput)
  ) =>
  (PlutusMap.Map Integer Integer -> PLifted plutarchOutput) ->
  ClosedTerm (PMap 'Unsorted PInteger PInteger :--> plutarchOutput) ->
  Property
checkHaskellUnsortedPMapEquivalent goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input :: UnsortedAssocMap Integer Integer) -> goHaskell (getUnsortedAssocMap input) `prettyEquals` plift (goPlutarch # pconstant (getUnsortedAssocMap input))

checkHaskellUnsortedPMapEquivalent2 ::
  forall (plutarchInput :: S -> Type) (plutarchOutput :: S -> Type).
  ( PUnsafeLiftDecl plutarchInput
  , Pretty (PLifted plutarchInput)
  , Arbitrary (PLifted plutarchInput)
  , PUnsafeLiftDecl plutarchOutput
  , Pretty (PLifted plutarchOutput)
  , Eq (PLifted plutarchOutput)
  ) =>
  (PLifted plutarchInput -> PlutusMap.Map Integer Integer -> PLifted plutarchOutput) ->
  ClosedTerm (plutarchInput :--> PMap 'Unsorted PInteger PInteger :--> plutarchOutput) ->
  Property
checkHaskellUnsortedPMapEquivalent2 goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input1 :: PLifted haskellInput, input2 :: UnsortedAssocMap Integer Integer) ->
      goHaskell input1 (getUnsortedAssocMap input2)
        `prettyEquals` plift (goPlutarch # pconstant input1 # pconstant (getUnsortedAssocMap input2))

checkHaskellSortedPMapEquivalent2 ::
  forall (plutarchInput :: S -> Type) (plutarchOutput :: S -> Type).
  ( PUnsafeLiftDecl plutarchInput
  , Pretty (PLifted plutarchInput)
  , Arbitrary (PLifted plutarchInput)
  , PUnsafeLiftDecl plutarchOutput
  , Pretty (PLifted plutarchOutput)
  , Eq (PLifted plutarchOutput)
  ) =>
  (PLifted plutarchInput -> PlutusMap.Map Integer Integer -> PLifted plutarchOutput) ->
  ClosedTerm (plutarchInput :--> PMap 'Sorted PInteger PInteger :--> plutarchOutput) ->
  Property
checkHaskellSortedPMapEquivalent2 goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input1 :: PLifted haskellInput, input2 :: PlutusMap.Map Integer Integer) ->
      goHaskell input1 input2
        `prettyEquals` plift (goPlutarch # pconstant input1 # punsafeCoerce (pconstant input2))

peven :: Term s (PInteger :--> PBool)
peven = plam $ \n -> pmod # n # 2 #== 0

mkEven :: Integer -> Maybe Integer
mkEven n
  | even n = Just n
  | otherwise = Nothing

pmkEven :: ClosedTerm (PInteger :--> PMaybe PInteger)
pmkEven = plam $ \n -> pif (peven # n) (pjust # n) pnothing