{-# LANGUAGE OverloadedRecordDot #-}

module Plutarch.FieldSpec (spec) where

import qualified PlutusCore as PLC
import PlutusLedgerApi.V1.Address (Address (Address))
import PlutusLedgerApi.V1.Credential (Credential (PubKeyCredential))
import qualified PlutusTx
import Test.Tasty.HUnit

import Plutarch.Api.V1 (PAddress (PAddress))
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Prelude
import Plutarch.SpecTypes (PTriplet)
import Plutarch.Test
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import Test.Hspec

spec :: Spec
spec = do
  describe "field" $ do
    -- example: Trips
    describe "trips" . pgoldenSpec $ do
      -- compilation
      "lam" @\ do
        "tripSum" @| tripSum
        "getY" @| getY
        "tripYZ" @| tripYZ
      "tripSum" @\ do
        "A" @| tripSum # tripA @-> \p ->
          plift p @?= 1000
        "B" @| tripSum # tripB @-> \p ->
          plift p @?= 100
        "C" @| tripSum # tripC @-> \p ->
          plift p @?= 10
      "tripYZ=tripZY" @| tripZY @== tripYZ
    -- rangeFields
    describe "rangeFields" . pgoldenSpec $ do
      -- compilation
      "lam" @| rangeFields
      "app" @| rangeFields # someFields @-> \p -> plift p @?= 11
    -- dropFields
    describe "dropFields" . pgoldenSpec $ do
      -- compilation
      "lam" @| dropFields
      "app" @| dropFields # someFields @-> \p -> plift p @?= 17
    -- pletFields
    describe "pletFields" . pgoldenSpec $ do
      -- compilation
      "letSomeFields" @\ do
        "lam" @| letSomeFields
        "order" @| letSomeFields' @== letSomeFields
        "app" @| letSomeFields # someFields @-> \p -> plift p @?= 14
      "nFields" @\ do
        "lam" @| nFields
        "app" @| nFields # someFields @-> \p -> plift p @?= 1
    describe "other" . pgoldenSpec $ do
      "by" @| by @-> \p -> plift p @?= 10
      "dotPlus" @| dotPlus @-> \p -> plift p @?= 19010
    describe "data" . pgoldenSpec $ do
      "pmatch-pfield" @\ do
        -- These two should ideally have the exact same efficiency.
        "pmatch" @\ do
          "newtype"
            @| let addr = pconstant $ Address (PubKeyCredential "ab") Nothing
                in pmatch addr $ \(PAddress addrFields) ->
                    pletFields @'["credential", "stakingCredential"] addrFields $ \y ->
                      ppairDataBuiltin # getField @"credential" y # getField @"stakingCredential" y
        "pfield" @\ do
          "newtype"
            @| let addr = pconstant $ Address (PubKeyCredential "ab") Nothing
                in pletFields @'["credential", "stakingCredential"] addr $ \y ->
                    ppairDataBuiltin # getField @"credential" y # getField @"stakingCredential" y
      "pfield-pletFields" @\ do
        "pfield" @\ do
          "single"
            @| let addr = pconstant $ Address (PubKeyCredential "ab") Nothing
                in pfromData $ pfield @"credential" # addr
        "pletFields" @\ do
          "single"
            @| let addr = pconstant $ Address (PubKeyCredential "ab") Nothing
                in pletFields @'["credential"] addr $ \y ->
                    pfromData $ getField @"credential" y

--------------------------------------------------------------------------------

mkTrip ::
  forall a s. (PIsData a) => Term s a -> Term s a -> Term s a -> Term s (PTriplet a)
mkTrip x y z =
  punsafeBuiltin PLC.ConstrData # (0 :: Term _ PInteger)
    # ( ( pcons # (pdata x)
            #$ pcons # (pdata y)
            #$ pcons # (pdata z)
              # pnil
        ) ::
          Term _ (PBuiltinList (PAsData a))
      )

-- | An example term
tripAPPlutus' s => Term s (PTriplet PInteger)
tripA = mkTrip 150 750 100

-- | Another
tripBPPlutus' s => Term s (PTriplet PInteger)
tripB = mkTrip 50 10 40

-- | Another
tripCPPlutus' s => Term s (PTriplet PInteger)
tripC = mkTrip 1 8 1

-- | Nested PTriplet
tripTripPPlutus' s => Term s (PTriplet (PTriplet PInteger))
tripTrip = mkTrip tripA tripB tripC

{- |
  'pletFields' generates efficient bindings for the specified fields,
  as a 'HRec' of fields.

  The fields in the 'HRec' can them be accessed with
  RecordDotSyntax.
-}
tripSumPPlutus' s => Term s ((PTriplet PInteger) #-> PInteger)
tripSum =
  plam $ \x -> pletFields @["x", "y", "z"] x $
    \fs ->
      pfromData fs.x
        + pfromData fs.y
        + pfromData fs.z

{- |
   A subset of fields can be specified.
-}
tripYZPPlutus' s => Term s ((PTriplet PInteger) #-> PInteger)
tripYZ =
  plam $ \x -> pletFields @["y", "z"] x $
    \fs ->
      pfromData fs.y + pfromData fs.z

{- |
  The ordering of fields specified is irrelevant,
  this is equivalent to 'tripYZ'.
-}
tripZYPPlutus' s => Term s ((PTriplet PInteger) #-> PInteger)
tripZY =
  plam $ \x -> pletFields @["z", "y"] x $
    \fs ->
      pfromData fs.y + pfromData fs.z

{- |
  When accessing only a single field, we can use 'pfield'.

  This should be used carefully - if more than one field is needed,
  'pletFields' is more efficient.
-}
byPPlutus' s => Term s PInteger
by = pfield @"y" # tripB

getYPPlutus' s => Term s (PTriplet PInteger #-> PAsData PInteger)
getY = pfield @"y"

{- |
  Due to the instance @(PDataFields a) -> PDataFields (PAsData a)@,

  we can conveniently chain 'pletAllFields' & 'pfield' within
  nested structures:
-}
dotPlusPPlutus' s => Term s PInteger
dotPlus =
  pletFields @["x", "y", "z"] tripTrip $ \ts ->
    pletFields @["x", "y", "z"] (ts.x) $ \a ->
      pletFields @["x", "y", "z"] (ts.y) $ \b ->
        pletFields @["x", "y", "z"] (ts.z) $ \c ->
          (pfromData a.x * pfromData b.x)
            + (pfromData a.y * pfromData b.y)
            + (pfromData a.z * pfromData b.z)
            + pfromData c.x
            + pfromData c.y
            + pfromData c.z

type SomeFields =
  '[ "_0" ':= PInteger
   , "_1" ':= PInteger
   , "_2" ':= PInteger
   , "_3" ':= PInteger
   , "_4" ':= PInteger
   , "_5" ':= PInteger
   , "_6" ':= PInteger
   , "_7" ':= PInteger
   , "_8" ':= PInteger
   , "_9" ':= PInteger
   ]

someFieldsPPlutus' s => Term s (PDataRecord SomeFields)
someFields =
  punsafeCoerce $
    pconstant $
      fmap (PlutusTx.toData @Integer) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

{- |
  We can also bind over a 'PDataRecord' directly.
-}
nFieldsPPlutus' s => Term s (PDataRecord SomeFields #-> PInteger)
nFields =
  plam $ \r -> pletFields @["_0", "_1"] r $ \fs ->
    pfromData fs._0
      + pfromData fs._1

dropFieldsPPlutus' s => Term s (PDataRecord SomeFields #-> PInteger)
dropFields =
  plam $ \r -> pletFields @["_8", "_9"] r $ \fs ->
    pfromData fs._8
      + pfromData fs._9

rangeFieldsPPlutus' s => Term s (PDataRecord SomeFields #-> PInteger)
rangeFields =
  plam $ \r -> pletFields @["_5", "_6"] r $ \fs ->
    pfromData fs._5
      + pfromData fs._6

{- |
  'pletFields' makes it convenient to pick out
  any amount of desired fields, efficiently.
-}
letSomeFieldsPPlutus' s => Term s (PDataRecord SomeFields #-> PInteger)
letSomeFields =
  plam $ \r -> pletFields @["_3", "_4", "_7"] r $ \fs ->
    pfromData fs._3
      + pfromData fs._4
      + pfromData fs._7

{- |
  Ordering of fields is irrelevant
-}
letSomeFields'PPlutus' s => Term s (PDataRecord SomeFields #-> PInteger)
letSomeFields' =
  plam $ \r -> pletFields @["_7", "_3", "_4"] r $ \fs ->
    pfromData fs._3
      + pfromData fs._4
      + pfromData fs._7
