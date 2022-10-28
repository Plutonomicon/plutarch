{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.PIsDataSpec (spec) where

import Data.Text.Encoding (encodeUtf8)

import Data.Functor.Compose (Compose (Compose))
import Data.String (fromString)
import PlutusLedgerApi.V1 (
  Address (Address),
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol,
  ScriptPurpose (Minting, Rewarding, Spending),
  StakingCredential (StakingHash),
  TxOutRef (TxOutRef),
 )

import Data.SOP (NS (S, Z))
import qualified PlutusTx

import Test.Tasty.QuickCheck (Arbitrary, property)

import Plutarch.Api.V1
import Plutarch.Api.V1.Tuple (pbuiltinPairFromTuple, ptupleFromBuiltin)
import Plutarch.Builtin (pforgetData, ppairDataBuiltin)
import Plutarch.DataRepr (PDataSum (PDataSum))
import Plutarch.Lift (PLifted)
import Plutarch.Prelude
import Plutarch.SpecTypes (PTriplet (PTriplet))
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "pisdata" $ do
    propertySet @PBool "PBool"
    propertySet @PInteger "PInteger"
    propertySet @PUnit "PUnit"
    describe "equality" . pgoldenSpec $ do
      "PData" @\ do
        "1"
          @| (let dat = pconstant @PData (PlutusTx.List [PlutusTx.Constr 1 [PlutusTx.I 0]]) in dat #== dat)
          @-> passert
        "2"
          @| (pnot #$ pconstant @PData (PlutusTx.Constr 0 []) #== pconstant @PData (PlutusTx.I 42))
          @-> passert
      "PAsData" @\ do
        "1"
          @| let dat = pdata @PInteger 42
              in dat #== dat
                  @-> passert
        "1"
          @| (pnot #$ pdata (phexByteStr "12") #== pdata (phexByteStr "ab"))
          @-> passert
    describe "ppair" . pgoldenSpec $ do
      -- pfromData (pdata (I 1, B 0x41)) ≡ (I 1, I A)
      "simple"
        @| ( ppairDataBuiltin @_ @PInteger @PByteString
              # pconstantData @PInteger 1
              #$ pdata (pconstant $ encodeUtf8 "A")
           )
          @-> \p ->
            pfromData (pdata p) `pshouldBe` p
      -- pfromdata (pdata (ptxid 0x41, pscriptcredential 0x82)) ≡ (ptxid 0x41, pscriptcredential 0x82)
      let scPair =
            ppairDataBuiltin
              # pconstantData @PTxId "41"
              #$ pconstantData (ScriptCredential "82")
      "scriptcredential" @| scPair @-> \p ->
        pfromData (pdata p) `pshouldBe` p
      let scTuple = pdata $ ptuple # pconstantData @PTxId "41" #$ pconstantData $ ScriptCredential "82"
      "isomorphism" @\ do
        "pforgetData" @| pforgetData (pdata scPair) @== pforgetData scTuple
        "pbuiltinPairFromTuple" @| pfromData (pbuiltinPairFromTuple scTuple) @== scPair
        "ptupleFromBuiltin" @| ptupleFromBuiltin (pdata scPair) @== scTuple
    -- Data construction tests
    describe "constr" . pgoldenSpec $ do
      -- Sum of products construction
      "sop" @\ do
        "4wheeler" @\ do
          let datrec =
                pdcons
                  # pconstantData @PInteger 2 #$ pdcons
                  # pconstantData @PInteger 5 #$ pdcons
                  # pconstantData @PInteger 42 #$ pdcons
                  # pconstantData @PInteger 0
                  # pdnil
              expected =
                pconstant $
                  PlutusTx.Constr 0 [PlutusTx.I 2, PlutusTx.I 5, PlutusTx.I 42, PlutusTx.I 0]
          "normal"
            @| pcon (PFourWheeler datrec) @== expected
          "pdatasum"
            @| pcon @(PInner PVehicle) (PDataSum . Z $ Compose datrec) @== expected
        "2wheeler" @\ do
          let datrec = pdcons # pconstantData @PInteger 5 #$ pdcons # pconstantData @PInteger 0 # pdnil
              expected = pconstant $ PlutusTx.Constr 1 [PlutusTx.I 5, PlutusTx.I 0]
          "normal"
            @| pcon (PTwoWheeler datrec) @== expected
          "pdatasum"
            @| pcon @(PInner PVehicle) (PDataSum . S . Z $ Compose datrec) @== expected
        "immovable" @\ do
          let datrec = pdnil
              expected = pconstant $ PlutusTx.Constr 2 []
          "normal"
            @| pcon (PImmovableBox datrec) @== expected
          "pdatasum"
            @| pcon @(PInner PVehicle) (PDataSum . S . S . Z $ Compose datrec) @== expected
      -- Product construction
      "prod" @\ do
        "1" @\ do
          let datrec =
                pdcons
                  # pconstantData @PCurrencySymbol "ab" #$ pdcons
                  # pconstantData @PCurrencySymbol "41" #$ pdcons
                  # pconstantData @PCurrencySymbol "0e"
                  # pdnil
              expected =
                pconstant $
                  PlutusTx.Constr
                    0
                    [ PlutusTx.toData @CurrencySymbol "ab"
                    , PlutusTx.toData @CurrencySymbol "41"
                    , PlutusTx.toData @CurrencySymbol "0e"
                    ]
          "normal"
            @| pcon (PTriplet datrec) @== expected
          "pdatasum"
            @| pcon @(PInner (PTriplet PCurrencySymbol)) (PDataSum . Z $ Compose datrec)
              @== expected
        "2" @\ do
          let minting = Minting ""
              spending = Spending $ TxOutRef "ab" 0
              rewarding = Rewarding . StakingHash $ PubKeyCredential "da"

              datrec =
                pdcons
                  # pconstantData minting #$ pdcons
                  # pconstantData spending #$ pdcons
                  # pconstantData rewarding
                  # pdnil
              expected =
                pconstant $
                  PlutusTx.Constr
                    0
                    [PlutusTx.toData minting, PlutusTx.toData spending, PlutusTx.toData rewarding]
          "normal"
            @| pcon (PTriplet datrec) @== expected
          "datasum"
            @| pcon @(PInner (PTriplet PScriptPurpose)) (PDataSum . Z $ Compose datrec)
              @== expected
      -- Enumerable sum type construction
      "enum" @\ do
        "PA" @| pcon (PA pdnil) @== pconstant (PlutusTx.Constr 0 [])
        "PB" @| pcon (PB pdnil) @== pconstant (PlutusTx.Constr 1 [])
      -- Relation between pconstant and pcon
      "pconstant-pcon-rel"
        @| ( let valHash = "01"
                 addr = Address (ScriptCredential $ fromString valHash) Nothing
                 pscriptCredentialPPlutus' s => Term s PCredential
                 pscriptCredential =
                  pcon $
                    PScriptCredential $
                      pdcons # pdata (pcon $ PValidatorHash $ phexByteStr valHash) # pdnil
              in pconstant addr
                  @== pcon (PAddress $ pdcons # pdata pscriptCredential #$ pdcons # pdata (pcon $ PDNothing pdnil) # pdnil)
           )

propertySet ::
  forall p.
  ( PIsData p
  , PLift p
  , PlutusTx.ToData (PLifted p)
  , PlutusTx.FromData (PLifted p)
  , Eq (PLifted p)
  , Show (PLifted p)
  , Arbitrary (PLifted p)
  ) =>
  String ->
  Spec
propertySet typeName = do
  describe typeName $ do
    specify ("x ~ " <> typeName <> ": pfromData (pdata x) ≡ x") $
      property $ ptoFromEqual @p
    specify ("x ~ " <> typeName <> ": pfromData (PlutusTx.toData x) ≡ x") $
      property $ pfromDataCompat @p
    specify ("x ~ " <> typeName <> ": PlutusTx.fromData (pdata x) ≡ Just x") $
      property $ pdataCompat @p

ptoFromEqual ::
  forall p.
  ( PIsData p
  , PLift p
  ) =>
  PLifted p ->
  _
ptoFromEqual t = pfromData (pdata $ pconstant @p t) `pshouldBe` pconstant @p t

pfromDataCompat ::
  forall p.
  ( PIsData p
  , PlutusTx.ToData (PLifted p)
  , PLift p
  , Eq (PLifted p)
  , Show (PLifted p)
  ) =>
  PLifted p ->
  IO ()
pfromDataCompat x = plift (pfromData $ pconstantData @p x) `shouldBe` x

pdataCompat ::
  forall p.
  ( PLift p
  , PIsData p
  , PlutusTx.FromData (PLifted p)
  , Eq (PLifted p)
  , Show (PLifted p)
  ) =>
  PLifted p ->
  IO ()
pdataCompat x = PlutusTx.fromData @(PLifted p) (plift $ pforgetData $ pdata $ pconstant @p x) `shouldBe` Just x

data PVehicle (s :: S)
  = PFourWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger, "_2" ':= PInteger, "_3" ':= PInteger]))
  | PTwoWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger]))
  | PImmovableBox (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)
instance DerivePlutusType PVehicle where type DPTStrat _ = PlutusTypeData

data PEnumType (s :: S)
  = PA (Term s (PDataRecord '[]))
  | PB (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)
instance DerivePlutusType PEnumType where type DPTStrat _ = PlutusTypeData
