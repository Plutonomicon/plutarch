{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.POrdSpec (spec) where

import Data.ByteString (ByteString)

import PlutusLedgerApi.V1 (
  Address (Address),
  Credential (PubKeyCredential, ScriptCredential),
  PubKeyHash (PubKeyHash),
  StakingCredential (StakingHash, StakingPtr),
  ValidatorHash (ValidatorHash),
 )
import qualified PlutusTx
import qualified PlutusTx.Builtins as PlutusTx

import Test.QuickCheck.Instances ()

import Test.Tasty.QuickCheck (Arbitrary, arbitrary, oneof, property)

import Plutarch.Api.V1 (PAddress, PCredential (PPubKeyCredential, PScriptCredential), PMaybeData)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude

import Plutarch.SpecTypes (PTriplet (PTriplet), Triplet (Triplet))
import Plutarch.Test
import Test.Hspec (Spec, describe, shouldBe, specify)

spec :: Spec
spec = do
  describe "pisdata" $ do
    describe "ord.property" $ do
      propertySet @PBool "PBool"
      propertySet @(PMaybeData PInteger) "PMaybeData PInteger"
      propertySet @(PTriplet PInteger) "PMaybeData PInteger"
      propertySet @PAddress' "PAddress"
    describe "lt" . pgoldenSpec $ do
      "PCredential" @\ do
        let c1 = PubKeyCredential ""
            c2 = ScriptCredential "41"
        "derived" @\ ltWith (#<) c1 c2
        "pmatch" @\ ltWith ltCred c1 c2
        "pmatch-pdatarecord" @\ ltWith ltCred' c1 c2
      "PTriplet" @\ do
        let c1 = Triplet @Integer 1 2 3
            c2 = Triplet 1 3 5
        "derived" @\ ltWith (#<) c1 c2
        "pmatch" @\ ltWith ltTrip c1 c2
        "pmatch-pdatarecord" @\ ltWith ltTrip' c1 c2
    describe "lte" . pgoldenSpec $ do
      "PCredential" @\ do
        let c1 = PubKeyCredential ""
            c2 = ScriptCredential "41"
        "derived" @\ lteWith (#<=) c1 c2
        "pmatch" @\ lteWith lteCred c1 c2
        "pmatch-pdatarecord" @\ lteWith lteCred' c1 c2
      "PTriplet" @\ do
        let c1 = Triplet @Integer 1 2 3
            c2 = Triplet 1 3 5
        "derived" @\ lteWith (#<=) c1 c2
        "pmatch" @\ lteWith lteTrip c1 c2
        "pmatch-pdatarecord" @\ lteWith lteTrip' c1 c2
  where
    ltWith ::
      PLift p =>
      (forall s. Term s p -> Term s p -> Term s PBool) ->
      PLifted p ->
      PLifted p ->
      PlutarchGoldens
    ltWith f x y = do
      "true"
        @| (pconstant x `f` pconstant y)
        @-> passert
      "false"
        @| (pconstant y `f` pconstant x)
        @-> passertNot
    lteWith ::
      PLift p =>
      (forall s. Term s p -> Term s p -> Term s PBool) ->
      PLifted p ->
      PLifted p ->
      PlutarchGoldens
    lteWith f x y = do
      "true"
        @\ do
          "eq"
            @| (pconstant x `f` pconstant x)
            @-> passert
          "less"
            @| (pconstant x `f` pconstant y)
            @-> passert
      "false"
        @| (pconstant y `f` pconstant x)
        @-> passertNot

propertySet ::
  forall p.
  ( PLiftData p
  , POrd p
  , Ord (PLifted p)
  , Show (PLifted p)
  , Arbitrary (PLifted p)
  ) =>
  String ->
  Spec
propertySet typeName' = do
  describe typeName' $ do
    let typeName = '(' : typeName' ++ ")"
    specify ("(#<) @" <> typeName <> " ≡ (<) @" <> typeName) $
      property $ pltIso @p
    specify ("(#<=) @" <> typeName <> " ≡ (<=) @" <> typeName) $
      property $ plteIso @p
    specify ("(#==) @" <> typeName <> " ≡ (==) @" <> typeName) $
      property $ peqIso @p

pltIso :: forall p. (PLift p, POrd p, Ord (PLifted p)) => PLifted p -> PLifted p -> IO ()
pltIso a b = plift (pconstant @p a #< pconstant b) `shouldBe` (a < b)

plteIso :: forall p. (PLift p, POrd p, Ord (PLifted p)) => PLifted p -> PLifted p -> IO ()
plteIso a b = plift (pconstant @p a #<= pconstant b) `shouldBe` (a <= b)

peqIso :: forall p. (PLift p, PEq p, Eq (PLifted p)) => PLifted p -> PLifted p -> IO ()
peqIso a b = plift (pconstant @p a #== pconstant b) `shouldBe` (a == b)

newtype PAddress' s = PAddress' (Term s PAddress)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd)
instance DerivePlutusType PAddress' where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PAddress' where type PLifted PAddress' = Address'

newtype Address' = Address' Address
  deriving stock (Show, Eq, Ord)
  deriving newtype (PlutusTx.FromData, PlutusTx.ToData)
  deriving (PConstantDecl) via (DerivePConstantViaNewtype Address' PAddress' PAddress)

instance Arbitrary Address' where
  arbitrary = Address' <$> arbitraryAddr
    where
      arbitraryAddr = Address <$> arbitraryCred <*> arbitraryMaybeStakingCred
      arbitraryCred =
        oneof
          [ PubKeyCredential . PubKeyHash . PlutusTx.toBuiltin @ByteString <$> arbitrary
          , ScriptCredential . ValidatorHash . PlutusTx.toBuiltin @ByteString <$> arbitrary
          ]
      arbitraryStakingCred =
        oneof
          [ StakingHash <$> arbitraryCred
          , StakingPtr <$> arbitrary <*> arbitrary <*> arbitrary
          ]
      arbitraryMaybeStakingCred = oneof [pure Nothing, Just <$> arbitraryStakingCred]

-- A bunch of boilerplate purely for a bit more informative benchmarks.

-- manual 'pmatch' + manual field extraction impl.
_pmatchHelperCred ::
  (Term s PByteString -> Term s PByteString -> Term s PBool) ->
  Term s PCredential ->
  Term s PCredential ->
  Term s PBool
_pmatchHelperCred f cred1 cred2 = unTermCont $ do
  x <- tcont $ pmatch cred1
  y <- tcont $ pmatch cred2
  pure $ case (x, y) of
    (PPubKeyCredential a, PPubKeyCredential b) ->
      pto (pfromData $ pfield @"_0" # a) `f` pto (pfromData $ pfield @"_0" # b)
    (PPubKeyCredential _, PScriptCredential _) -> pconstant True
    (PScriptCredential _, PPubKeyCredential _) -> pconstant False
    (PScriptCredential a, PScriptCredential b) ->
      pto (pfromData $ pfield @"_0" # a) `f` pto (pfromData $ pfield @"_0" # b)

ltCredPPlutus' s => Term s PCredential -> Term s PCredential -> Term s PBool
ltCred = _pmatchHelperCred (#<)

lteCredPPlutus' s => Term s PCredential -> Term s PCredential -> Term s PBool
lteCred = _pmatchHelperCred (#<=)

-- manual 'pmatch' + 'PDataRecord' Ord impl.
_pmatchDataRecHelperCred ::
  (forall l. POrd (PDataRecord l) => Term s (PDataRecord l) -> Term s (PDataRecord l) -> Term s PBool) ->
  Term s PCredential ->
  Term s PCredential ->
  Term s PBool
_pmatchDataRecHelperCred f cred1 cred2 = unTermCont $ do
  x <- tcont $ pmatch cred1
  y <- tcont $ pmatch cred2
  pure $ case (x, y) of
    (PPubKeyCredential a, PPubKeyCredential b) -> a `f` b
    (PPubKeyCredential _, PScriptCredential _) -> pconstant True
    (PScriptCredential _, PPubKeyCredential _) -> pconstant False
    (PScriptCredential a, PScriptCredential b) -> a `f` b

ltCred'PPlutus' s => Term s PCredential -> Term s PCredential -> Term s PBool
ltCred' = _pmatchDataRecHelperCred (#<)

lteCred'PPlutus' s => Term s PCredential -> Term s PCredential -> Term s PBool
lteCred' = _pmatchDataRecHelperCred (#<=)

-- manual 'pmatch' + manual field extraction impl.
ltTripPPlutus' s => Term s (PTriplet PInteger) -> Term s (PTriplet PInteger) -> Term s PBool
ltTrip trip1 trip2 = unTermCont $ do
  a <- tcont $ pletFields @'["x", "y", "z"] trip1
  b <- tcont $ pletFields @'["x", "y", "z"] trip2

  x <- tcont . plet . pfromData $ getField @"x" a
  x' <- tcont . plet . pfromData $ getField @"x" b
  pure $
    x #< x'
      #|| ( x #== x'
              #&& unTermCont
                ( do
                    y <- tcont . plet . pfromData $ getField @"y" a
                    y' <- tcont . plet . pfromData $ getField @"y" b
                    pure $ y #< y' #|| (y #== y' #&& pfromData (getField @"z" a) #< pfromData (getField @"z" b))
                )
          )

lteTripPPlutus' s => Term s (PTriplet PInteger) -> Term s (PTriplet PInteger) -> Term s PBool
lteTrip trip1 trip2 = unTermCont $ do
  a <- tcont $ pletFields @'["x", "y", "z"] trip1
  b <- tcont $ pletFields @'["x", "y", "z"] trip2

  x <- tcont . plet . pfromData $ getField @"x" a
  x' <- tcont . plet . pfromData $ getField @"x" b
  pure $
    x #< x'
      #|| ( x #== x'
              #&& unTermCont
                ( do
                    y <- tcont . plet . pfromData $ getField @"y" a
                    y' <- tcont . plet . pfromData $ getField @"y" b
                    pure $ y #< y' #|| (y #== y' #&& pfromData (getField @"z" a) #<= pfromData (getField @"z" b))
                )
          )

-- manual 'pmatch' + 'PDataRecord' Ord impl.
_pmatchDataRecHelperTrip ::
  (forall l. POrd (PDataRecord l) => Term s (PDataRecord l) -> Term s (PDataRecord l) -> Term s PBool) ->
  Term s (PTriplet PInteger) ->
  Term s (PTriplet PInteger) ->
  Term s PBool
_pmatchDataRecHelperTrip f trip1 trip2 = unTermCont $ do
  PTriplet a <- tcont $ pmatch trip1
  PTriplet b <- tcont $ pmatch trip2
  pure $ a `f` b

ltTrip'PPlutus' s => Term s (PTriplet PInteger) -> Term s (PTriplet PInteger) -> Term s PBool
ltTrip' = _pmatchDataRecHelperTrip (#<)

lteTrip'PPlutus' s => Term s (PTriplet PInteger) -> Term s (PTriplet PInteger) -> Term s PBool
lteTrip' = _pmatchDataRecHelperTrip (#<=)
