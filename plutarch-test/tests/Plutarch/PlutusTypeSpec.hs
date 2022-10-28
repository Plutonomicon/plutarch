{-# LANGUAGE CPP #-}

module Plutarch.PlutusTypeSpec (spec) where

import Data.Functor.Compose (Compose (Compose))
import Data.SOP.NS (NS (S, Z))

import Plutarch.Api.V1 (
  PAddress (PAddress),
  PCredential (PPubKeyCredential, PScriptCredential),
  PScriptPurpose (PCertifying, PMinting, PRewarding, PSpending),
 )
import Plutarch.Builtin (pasByteStr, pasConstr)
import Plutarch.DataRepr (PDataSum (PDataSum))
import Plutarch.Prelude
import Plutarch.Test
import Plutarch.Unit ()
import PlutusLedgerApi.V1 (DCert (DCertGenesis), toData)
import PlutusLedgerApi.V1.Address (Address (Address))
import PlutusLedgerApi.V1.Contexts (ScriptPurpose (Certifying, Minting, Rewarding, Spending), TxOutRef (TxOutRef))
import PlutusLedgerApi.V1.Credential (
  Credential (PubKeyCredential, ScriptCredential),
  StakingCredential (StakingPtr),
 )

import Test.Hspec

spec :: Spec
spec = do
  describe "plutustype" $ do
    describe "example" . pgoldenSpec $ do
      "swap" @\ do
        "A" @| swap (pcon A) @== pcon B
        "B" @| swap (pcon B) @== pcon A
      "scottenc" @\ do
        "PMaybe"
          @| ( let a = 42PPlutus' s => Term s PInteger
                in pmatch (pcon $ PJust a) $ \case
                    PJust x -> x
                    -- We expect this perror not to be evaluated eagerly when mx
                    -- is a PJust.
                    PNothing -> perror
             )
        "PPair"
          @| ( let a = 42PPlutus' s => Term s PInteger
                   b = "Universe"PPlutus' s => Term s PString
                in pmatch (pcon (PPair a b)PPlutus' s => Term s (PPair PInteger PString)) $ \(PPair _ y) -> y
             )
    describe "instances-sanity" $ do
      it "PBuiltinList" $ do
        pmatchTargetEval $ pconstant [1 :: Integer, 2, 3, 4]
    deconstrSpec

{- | For comparing typed and untyped data deconstruction approaches.

We ideally want the typed and raw versions to have as little deviation as possible.
-}
deconstrSpec :: Spec
deconstrSpec = do
  describe "deconstr" . pgoldenSpec $ do
    "matching" @\ do
      "typed" @\ do
        "newtype" @\ do
          "normal"
            @| plam
              ( \x -> pmatch x $ \(PAddress addrFields) ->
                  addrFields
              )
              # pconstant addrPC
          "datasum"
            @| ( plam
                  ( \x -> pmatch (pupcast @(PInner PAddress) x) $ \(PDataSum datsum) -> case datsum of
                      Z (Compose addrFields) -> addrFields
                      _ -> perror
                  )
                  # pconstant addrPC
               )
        "sumtype(ignore-fields)" @\ do
          "normal"
            @| plam
              ( \x -> pmatch x $ \case
                  PMinting _ -> pconstant ()
                  _ -> perror
              )
              # pconstant minting
          "datasum"
            @| plam
              ( \x -> pmatch (pupcast @(PInner PScriptPurpose) x) $ \(PDataSum datsum) -> case datsum of
                  Z _ -> pconstant ()
                  _ -> perror
              )
              # pconstant minting
        "sumtype(partial-match)" @\ do
          "normal"
            @| plam
              ( \x -> pmatch x $ \case
                  PMinting hs -> hs
                  _ -> perror
              )
              # pconstant minting
          "datasum"
            @| plam
              ( \x -> pmatch (pupcast @(PInner PScriptPurpose) x) $ \(PDataSum datsum) -> case datsum of
                  Z (Compose hs) -> hs
                  _ -> perror
              )
              # pconstant minting
        "sumtype(exhaustive)" @\ do
          ("normal" @\) $
            benchPurpose $
              plam
                ( \x -> pmatch x $ \case
                    PMinting f -> plet f $ const $ phexByteStr "01"
                    PSpending f -> plet f $ const $ phexByteStr "02"
                    PRewarding f -> plet f $ const $ phexByteStr "03"
                    PCertifying f -> plet f $ const $ phexByteStr "04"
                )
          ("datasum" @\) $
            benchPurpose $
              plam
                ( \x -> pmatch (pupcast @(PInner PScriptPurpose) x) $ \(PDataSum datsum) -> case datsum of
                    Z (Compose f) -> plet f $ const $ phexByteStr "01"
                    S (Z (Compose f)) -> plet f $ const $ phexByteStr "02"
                    S (S (Z (Compose f))) -> plet f $ const $ phexByteStr "03"
                    S (S (S (Z (Compose f)))) -> plet f $ const $ phexByteStr "04"
                    _ -> perror
                )
        "sumtype(exhaustive)(ignore-fields)" @\ do
          ("normal" @\) $
            benchPurpose $
              plam
                ( \x -> pmatch x $ \case
                    PMinting _ -> phexByteStr "01"
                    PSpending _ -> phexByteStr "02"
                    PRewarding _ -> phexByteStr "03"
                    PCertifying _ -> phexByteStr "04"
                )
          ("datasum" @\) $
            benchPurpose $
              plam
                ( \x -> pmatch (pupcast @(PInner PScriptPurpose) x) $ \(PDataSum datsum) -> case datsum of
                    Z _ -> phexByteStr "01"
                    S (Z _) -> phexByteStr "02"
                    S (S (Z _)) -> phexByteStr "03"
                    S (S (S (Z _))) -> phexByteStr "04"
                    _ -> perror
                )
      "raw" @\ do
        "newtype"
          @| plam
            ( \x ->
                psndBuiltin #$ pasConstr # x
            )
            #$ pconstant
              (toData addrPC)
        "sumtype(ignore-fields)"
          @| plam
            ( \x ->
                pif
                  ((pfstBuiltin #$ pasConstr # x) #== 0)
                  (pconstant ())
                  perror
            )
            #$ pconstant (toData minting)
        "sumtype(partial-match)"
          @| plam
            ( \x ->
                plet (pasConstr # x) $ \d ->
                  pif
                    (pfstBuiltin # d #== 0)
                    (psndBuiltin # d)
                    perror
            )
            #$ pconstant (toData minting)
        "sumtype(exhaustive)" @\ do
          benchPurpose' $
            plam
              ( \x ->
                  plet (pasConstr # x) $ \d ->
                    plet (pfstBuiltin # d) $ \constr ->
                      plet (psndBuiltin # d) $ \_ ->
                        pif
                          (constr #== 1)
                          (phexByteStr "02")
                          $ pif
                            (constr #== 2)
                            (phexByteStr "03")
                            $ pif
                              (constr #== 3)
                              (phexByteStr "04")
                              $ phexByteStr "01"
              )
        "sumtype(exhaustive)(ignore-fields)" @\ do
          benchPurpose' $
            plam
              ( \x -> do
                  plet (pfstBuiltin #$ pasConstr # x) $ \constr ->
                    pif
                      (constr #== 1)
                      (phexByteStr "02")
                      $ pif
                        (constr #== 2)
                        (phexByteStr "03")
                        $ pif
                          (constr #== 3)
                          (phexByteStr "04")
                          $ phexByteStr "01"
              )
    "fields" @\ do
      "typed" @\ do
        "extract-single"
          @| plam
            ( \x ->
                pfield @"credential" # x
            )
            # pconstant addrSC
      "raw" @\ do
        "extract-single"
          @| plam
            ( \x ->
                phead #$ psndBuiltin #$ pasConstr # x
            )
            #$ pconstant
          $ toData addrSC
    "combined" @\ do
      "typed" @\ do
        "toValidatorHash"
          @| plam
            ( \x ->
                pmatch (pfromData $ pfield @"credential" # x) $ \case
                  PPubKeyCredential _ ->
                    pcon PNothing
                  PScriptCredential credFields ->
                    pcon . PJust $ pto $ pfromData $ pfield @"_0" # credFields
            )
            # pconstant addrSC
      "raw" @\ do
        "toValidatorHash"
          @| plam
            ( \x ->
                let cred = phead #$ psndBuiltin #$ pasConstr # x
                 in plet (pasConstr # cred) $ \deconstrCred ->
                      pif
                        (pfstBuiltin # deconstrCred #== 0)
                        (pcon PNothing)
                        $ pcon . PJust $ pasByteStr #$ phead #$ psndBuiltin # deconstrCred
            )
            # pconstant (toData addrSC)
  where
    addrSC = Address (ScriptCredential "ab") Nothing
    addrPC = Address (PubKeyCredential "ab") Nothing
    minting :: ScriptPurpose
    minting = Minting ""
    spending = Spending (TxOutRef "ab" 0)
    rewarding = Rewarding (StakingPtr 42 0 7)
    certifying = Certifying DCertGenesis
    -- Bench given function feeding in all 4 types of script purpose (typed).
    benchPurpose :: ClosedTerm (PScriptPurpose #-> PByteString) -> PlutarchGoldens
    benchPurpose f = do
      "minting" @| f # pconstant minting
      "spending" @| f # pconstant spending
      "rewarding" @| f # pconstant rewarding
      "certifying" @| f # pconstant certifying
    -- Bench given function feeding in all 4 types of script purpose (untyped).
    benchPurpose' :: ClosedTerm (PData #-> PByteString) -> PlutarchGoldens
    benchPurpose' f = do
      "minting" @| f #$ pconstant $ toData minting
      "spending" @| f #$ pconstant $ toData spending
      "rewarding" @| f #$ pconstant $ toData rewarding
      "certifying" @| f #$ pconstant $ toData certifying

-- | Make sure the target of 'pmatch' is only evaluated once.
pmatchTargetEval :: PlutusType p => ClosedTerm p -> Expectation
pmatchTargetEval target =
  pmatch (ptrace (pconstant tag) target) (\x -> plet (pcon x) $ \_ -> pconstant ())
    `ptraces` replicate 1 tag
  where
    tag = "evaluating"

{- TODO:
    - move over the testcase with pmatchTargetEval
    - add more sanity checks
describe "sanity checks" $ do
  describe "PBuiltinList" $ do
    let pPPlutus' s => Term s (PBuiltinList PInteger)
        p = pconstant [1,2,3,4]
    it "works" $
 -}

data AB (s :: S)
  = A
  | B
  deriving stock (Generic)
  deriving anyclass (PlutusType)
instance DerivePlutusType AB where type DPTStrat _ = PlutusTypeScott

{- |
  Instead of using `pcon'` and `pmatch'` directly,
  use 'pcon' and 'pmatch', to hide the `PInner` type.
-}
swapPPlutus' s => Term s AB -> Term s AB
swap x = pmatch x $ \case
  A -> pcon B
  B -> pcon A
