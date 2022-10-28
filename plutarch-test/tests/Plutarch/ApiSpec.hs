{-# LANGUAGE AllowAmbiguousTypes #-}

-- NOTE: This module also contains ScriptContext mocks, which should ideally
-- moved to a module of its own after cleaning up to expose a easy to reason
-- about API.
module Plutarch.ApiSpec (
  spec,
  ctx,
  validContext0,
  validOutputs0,
  invalidContext1,
  d0Dat,
  d0DatValue,
  inp,
) where

import Test.Tasty.HUnit

import Control.Monad (forM_)
import Control.Monad.Trans.Cont (cont, runCont)
import Data.String (fromString)
import Numeric (showHex)
import PlutusLedgerApi.V1
import qualified PlutusLedgerApi.V1.Interval as Interval
import qualified PlutusLedgerApi.V1.Value as Value
import PlutusTx.Monoid (inv)

import Plutarch.Api.V1 (
  AmountGuarantees (NoGuarantees, NonZero, Positive),
  KeyGuarantees (Sorted),
  PCredential,
  PCurrencySymbol,
  PMaybeData,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose (PMinting, PSpending),
  PStakingCredential,
  PTxInInfo,
  PTxInfo,
  PValue,
 )
import qualified Plutarch.Api.V1.AssocMap as AssocMap
import qualified Plutarch.Api.V1.Value as PValue
import Plutarch.Builtin (pasConstr, pforgetData)
import Plutarch.Prelude
import Plutarch.Test
import Plutarch.Test.Property.Gen ()

import Test.Hspec
import Test.Tasty.QuickCheck (Property, property, (===))

import Plutarch.Lift (PConstanted, PLifted, PUnsafeLiftDecl (PLifted))

newtype EnclosedTerm (p :: PType) = EnclosedTerm {getEnclosedTerm :: ClosedTerm p}

spec :: Spec
spec = do
  describe "api" $ do
    describe "ctx" $ do
      pgoldenSpec $ do
        "term" @| ctx
        "get" @\ do
          "txInfo" @| pfromData (getTxInfo # ctx) @-> \p ->
            plift p @?= info
          "mint" @| pforgetData (getMint #$ getTxInfo # ctx) @-> \p ->
            plift p @?= toData mint
          "credentials" @| getCredentials ctx @-> \p ->
            plift p @?= [toData validator]
          "sym"
            @| pfromData (getSym #$ PValue.pnormalize #$ pfromData $ getMint #$ getTxInfo # ctx)
            @-> \p -> plift p @?= sym
        "ScriptPurpose" @\ do
          "literal" @| pconstant @PScriptPurpose (Minting dummyCurrency)
          "decode"
            @| pmatch (pconstant @PScriptPurpose (Minting dummyCurrency))
            $ \case
              PMinting c -> popaque c
              _ -> perror
    describe "value" $ do
      pgoldenSpec $ do
        let pmint = PValue.pconstantPositiveSingleton (pconstant "c0") (pconstant "sometoken") 1
            pmintOtherToken = PValue.pconstantPositiveSingleton (pconstant "c0") (pconstant "othertoken") 1
            pmintOtherSymbol = PValue.pconstantPositiveSingleton (pconstant "c7") (pconstant "sometoken") 1
            pada = PValue.pconstantPositiveSingleton PValue.padaSymbol PValue.padaToken 10_000_000
            growingSymbols, symbols :: [EnclosedTerm (PValue 'Sorted 'Positive)]
            growingSymbols =
              scanl
                (\s v -> EnclosedTerm $ getEnclosedTerm s <> getEnclosedTerm v)
                (EnclosedTerm pmint)
                symbols
            symbols = (\n -> EnclosedTerm (toSymbolicValue n)) <$> [0 .. 15]
            toSymbolicValue :: Integer -> ClosedTerm (PValue 'Sorted 'Positive)
            toSymbolicValue n =
              PValue.pconstantPositiveSingleton (pconstant $ fromString $ "c" <> showHex n "") (pconstant "token") 1
        "singleton" @| pmint @-> \p ->
          plift (PValue.pforgetSorted $ PValue.pforgetPositive p) @?= mint
        "singletonData"
          @| PValue.psingletonData # pdata (pconstant "c0") # pdata (pconstant "sometoken") # pdata 1
          @-> \p -> plift (PValue.pforgetSorted p) @?= mint
        "valueOf" @\ do
          "itself" @| PValue.pvalueOf @-> \v -> plift (v # pmint # pconstant "c0" # pconstant "sometoken") @?= 1
          "applied" @| PValue.pvalueOf # pmint # pconstant "c0" # pconstant "sometoken" @-> \p ->
            plift p @?= 1
          "growing"
            @\ forM_
              (zip [1 :: Int .. length growingSymbols] growingSymbols)
              ( \(size, v) ->
                  fromString (show size)
                    @| PValue.pvalueOf # getEnclosedTerm v # pconstant "c7" # pconstant "token"
                    @-> \p -> plift p @?= if size < 9 then 0 else 1
              )
        "unionWith" @\ do
          "const" @| PValue.punionWith # plam const # pmint # pmint @-> \p ->
            plift (PValue.pforgetSorted $ PValue.pnormalize # p) @?= mint
          "(+)" @\ do
            "itself" @| PValue.punionWith # plam (+) @-> \plus ->
              plift (PValue.pforgetSorted $ PValue.pnormalize #$ plus # pmint # pmint) @?= mint <> mint
            "applied" @| PValue.punionWith # plam (+) # pmint # pmint @-> \p ->
              plift (PValue.pforgetSorted $ PValue.pnormalize # p) @?= mint <> mint
          "tokens" @| PValue.punionWith # plam (+) # pmint # pmintOtherToken @-> \p ->
            plift (PValue.pforgetSorted $ PValue.pnormalize # p) @?= mint <> mintOtherToken
          "symbols" @| PValue.punionWith # plam (+) # pmint # pmintOtherSymbol @-> \p ->
            plift (PValue.pforgetSorted $ PValue.pnormalize # p) @?= mint <> mintOtherSymbol
          "growing"
            @\ forM_
              (zip [1 :: Int .. length growingSymbols] growingSymbols)
              ( \(size, v) ->
                  fromString (show size) @| PValue.punionWith # plam const # getEnclosedTerm v # pmintOtherSymbol
                    @-> \v' -> passert (v' #== PValue.punionWith # plam const # pmintOtherSymbol # getEnclosedTerm v)
              )
        "unionWithData const" @\ do
          "itself" @| PValue.punionWithData @-> \u ->
            plift (PValue.pforgetSorted $ PValue.pnormalize #$ u # plam const # pmint # pmint) @?= mint
          "applied" @| PValue.punionWithData # plam const # pmint # pmint @-> \p ->
            plift (PValue.pforgetSorted $ PValue.pnormalize # p) @?= mint
        "inv"
          @| inv (PValue.pforgetPositive pmint :: Term _ (PValue 'Sorted 'NonZero))
          @-> \p -> plift (PValue.pforgetSorted p) @?= inv mint
        "equality" @\ do
          "itself" @| plam ((#==) @(PValue 'Sorted 'Positive)) @-> \eq -> passert (eq # pmint # pmint)
          "triviallyTrue" @| pmint #== pmint @-> passert
          "triviallyFalse" @| pmint #== pmintOtherToken @-> passertNot
          "swappedTokensTrue"
            @| pto (PValue.punionWith # plam (+) # pmint # pmintOtherToken)
              #== pto (PValue.punionWith # plam (+) # pmintOtherToken # pmint)
            @-> passert
          "swappedSymbolsTrue"
            @| pto (PValue.punionWith # plam (+) # pmint # pmintOtherSymbol)
              #== pto (PValue.punionWith # plam (+) # pmintOtherSymbol # pmint)
            @-> passert
          "growing"
            @\ forM_
              (zip [1 :: Int .. length growingSymbols] growingSymbols)
              ( \(size, v) ->
                  fromString (show size)
                    @| getEnclosedTerm v #== getEnclosedTerm v @-> passert
              )
        "normalize" @\ do
          "identity"
            @| PValue.passertPositive # (PValue.pnormalize # (pmint <> pmintOtherSymbol))
            @-> \v -> passert (v #== pmint <> pmintOtherSymbol)
          "empty"
            @| PValue.pnormalize # (PValue.punionWith # plam (-) # pmint # pmint)
            @-> \v -> passert (v #== mempty)
        "assertSorted" @\ do
          "succeeds" @| PValue.passertSorted # (pmint <> pmintOtherSymbol) @-> psucceeds
          "fails on malsorted symbols"
            @| PValue.passertSorted
              # ( pcon $
                    PValue.PValue $
                      pcon $
                        AssocMap.PMap $
                          pconcat # pto (pto pmintOtherSymbol) # pto (pto pmint)
                )
            @-> pfails
          "fails on zero quantities"
            @| PValue.passertSorted # (PValue.punionWith # plam (-) # pmint # pmint)
            @-> pfails
          "fails on empty token map"
            @| PValue.passertSorted
              # (pcon $ PValue.PValue $ AssocMap.psingleton # pconstant "c0" # AssocMap.pempty)
            @-> pfails
        "Ada" @\ do
          "adaSymbol" @| PValue.padaSymbol @-> psucceeds
          "adaToken" @| PValue.padaToken @-> psucceeds
          "lovelaceValueOf" @| PValue.plovelaceValueOf @-> \p -> passert (p # pada #== 10_000_000)
          "isAdaOnlyValue" @\ do
            "itself" @| PValue.pisAdaOnlyValue @-> \p -> passert (p # pada)
            "true on empty" @| PValue.pisAdaOnlyValue # (mempty :: Term _ (PValue 'Sorted 'Positive)) @-> passert
            "trivially false" @| PValue.pisAdaOnlyValue # pmint @-> passertNot
            "less trivially false" @| PValue.pisAdaOnlyValue # (pmint <> pada) @-> passertNot
          "adaOnlyValue" @\ do
            "itself" @| PValue.padaOnlyValue @-> \p -> passert (p # (pada <> pmint) #== pada)
            "on empty"
              @| PValue.padaOnlyValue # (mempty :: Term _ (PValue 'Sorted 'Positive))
              @-> \p -> passert (p #== mempty)
            "on non-Ada" @| PValue.padaOnlyValue # pmint @-> \p -> passert (p #== mempty)
            "on Ada" @| PValue.padaOnlyValue # pada @-> \p -> passert (p #== pada)
          "noAdaValue" @\ do
            "itself" @| PValue.pnoAdaValue @-> \p -> passert (p # (pada <> pmint) #== pmint)
            "on empty"
              @| PValue.pnoAdaValue # (mempty :: Term _ (PValue 'Sorted 'Positive))
              @-> \p -> passert (p #== mempty)
            "on non-Ada" @| PValue.pnoAdaValue # pmint @-> \p -> passert (p #== pmint)
            "on Ada" @| PValue.pnoAdaValue # pada @-> \p -> passert (p #== mempty)
    describe "map" $ do
      pgoldenSpec $ do
        let pmap, pdmap, emptyMap, doubleMap, otherMap :: Term _ (AssocMap.PMap 'Sorted PByteString PInteger)
            pmap = AssocMap.psingleton # pconstant "key" # 42
            pdmap = AssocMap.psingletonData # pdata (pconstant "key") # pdata 42
            emptyMap = AssocMap.pempty
            doubleMap = AssocMap.psingleton # pconstant "key" # 84
            otherMap = AssocMap.psingleton # pconstant "newkey" # 6
        "lookup" @\ do
          "itself" @| AssocMap.plookup
            @-> \lookup -> passert $ lookup # pconstant "key" # pmap #== pcon (PJust 42)
          "hit" @| AssocMap.plookup # pconstant "key" # pmap
            @-> \result -> passert $ result #== pcon (PJust 42)
          "miss" @| AssocMap.plookup # pconstant "nokey" # pmap
            @-> \result -> passert $ result #== pcon PNothing
        "lookupData" @\ do
          "hit" @| AssocMap.plookupData # pdata (pconstant "key") # pmap
            @-> \result -> passert $ result #== pcon (PJust $ pdata 42)
          "miss" @| AssocMap.plookupData # pdata (pconstant "nokey") # pmap
            @-> \result -> passert $ result #== pcon PNothing
        "findWithDefault" @\ do
          "itself" @| AssocMap.pfindWithDefault
            @-> \find -> (find # 12 # pconstant "key" # pmap) #@?= (42 :: Term _ PInteger)
          "hit" @| AssocMap.pfindWithDefault # 12 # pconstant "key" # pmap
            @-> \result -> passert $ result #== 42
          "hit2"
            @| AssocMap.pfindWithDefault # 12 # pconstant "newkey" # (AssocMap.punionWith # plam const # pmap # otherMap)
            @-> \result -> passert $ result #== 6
          "miss" @| AssocMap.pfindWithDefault # 12 # pconstant "nokey" # pmap
            @-> \result -> passert $ result #== 12
        "singleton" @| pmap @-> pshouldReallyBe pdmap
        "singletonData" @| pdmap @-> pshouldReallyBe pmap
        "insert" @\ do
          "empty" @| AssocMap.pinsert # pconstant "key" # 42 # emptyMap @-> pshouldReallyBe pmap
          "replace" @| AssocMap.pinsert # pconstant "key" # 84 # pmap @-> pshouldReallyBe doubleMap
        "delete" @\ do
          "empty" @| AssocMap.pdelete # pconstant "key" # emptyMap @-> pshouldReallyBe emptyMap
          "only" @| AssocMap.pdelete # pconstant "key" # pmap @-> pshouldReallyBe emptyMap
          "miss" @| AssocMap.pdelete # pconstant "nokey" # pmap @-> pshouldReallyBe pmap
          "new"
            @| AssocMap.pdelete # pconstant "newkey" # (AssocMap.pinsert # pconstant "newkey" # 6 # pmap)
            @-> pshouldReallyBe pmap
          "old"
            @| AssocMap.pdelete # pconstant "key" # (AssocMap.pinsert # pconstant "newkey" # 6 # pmap)
            @-> pshouldReallyBe otherMap
        "difference" @\ do
          "emptyLeft" @| AssocMap.pdifference # emptyMap # pmap @-> pshouldReallyBe emptyMap
          "emptyRight" @| AssocMap.pdifference # pmap # emptyMap @-> pshouldReallyBe pmap
          "emptyResult" @| AssocMap.pdifference # pmap # doubleMap @-> pshouldReallyBe emptyMap
        "unionWith" @\ do
          "const" @| AssocMap.punionWith # plam const # pmap # pmap @-> pshouldReallyBe pmap
          "double" @| AssocMap.punionWith # plam (+) # pmap # pmap @-> pshouldReallyBe doubleMap
          "(+)"
            @| AssocMap.punionWith # plam (+) # pmap # otherMap
            @-> \p -> passert (p #== AssocMap.punionWith # plam (+) # otherMap # pmap)
          "flip (+)"
            @| AssocMap.punionWith # plam (+) # otherMap # pmap
            @-> \p -> passert (p #== AssocMap.punionWith # plam (+) # pmap # otherMap)
        "unionWithData" @\ do
          "const" @| AssocMap.punionWithData # plam const # pmap # pmap @-> pshouldReallyBe pmap
          "emptyLeft" @| AssocMap.punionWithData # plam const # emptyMap # pmap @-> pshouldReallyBe pmap
          "emptyRight" @| AssocMap.punionWithData # plam const # pmap # emptyMap @-> pshouldReallyBe pmap
    describe "example" $ do
      -- The checkSignatory family of functions implicitly use tracing due to
      -- monadic syntax, and as such we need two sets of tests here.
      -- See Plutarch.MonadicSpec for GHC9 only syntax.
      describe "signatory" . pgoldenSpec $ do
        let aSig :: PubKeyHash = "ab01fe235c"
        "cont" @\ do
          "succeeds" @| checkSignatoryCont # pconstant aSig # ctx @-> psucceeds
          "fails" @| checkSignatoryCont # pconstant "41" # ctx @-> pfails
        "termcont" @\ do
          "succeeds" @| checkSignatoryTermCont # pconstant aSig # ctx @-> psucceeds
          "fails" @| checkSignatoryTermCont # pconstant "41" # ctx @-> pfails
      describe "getFields" . pgoldenSpec $ do
        "0" @| getFields
    describe "data recovery" $ do
      describe "succeding property tests" $ do
        it "recovering PAddress succeeds" $
          property (propPlutarchtypeCanBeRecovered @Address)
        it "recovering PTokenName succeeds" $
          property (propPlutarchtypeCanBeRecovered @TokenName)
        it "recovering PCredential succeeds" $
          property (propPlutarchtypeCanBeRecovered @Credential)
        it "recovering PStakingCredential succeeds" $
          property (propPlutarchtypeCanBeRecovered @StakingCredential)
        it "recovering PPubKeyHash succeeds" $
          property (propPlutarchtypeCanBeRecovered @PubKeyHash)
        it "recovering PValidatorHash succeeds" $
          property (propPlutarchtypeCanBeRecovered @ValidatorHash)
        it "recovering PValue succeeds" $
          property (propPlutarchtypeCanBeRecovered @Value)
        it "recovering PCurrencySymbol succeeds" $
          property (propPlutarchtypeCanBeRecovered @CurrencySymbol)
        it "recovering PMaybeData succeeds" $
          property prop_pmaybedata_can_be_recovered

--------------------------------------------------------------------------------

{- |
  An example 'PScriptContext' Term,
  lifted with 'pconstant'
-}
ctxPPlutus' s => Term s PScriptContext
ctx =
  pconstant
    (ScriptContext info purpose)

-- | Simple script context, with minting and a single input
info :: TxInfo
info =
  TxInfo
    { txInfoInputs = [inp]
    , txInfoOutputs = []
    , txInfoFee = mempty
    , txInfoMint = mint
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = signatories
    , txInfoData = []
    , txInfoId = "b0"
    }

-- | A script input
inp :: TxInInfo
inp =
  TxInInfo
    { txInInfoOutRef = ref
    , txInInfoResolved =
        TxOut
          { txOutAddress =
              Address (ScriptCredential validator) Nothing
          , txOutValue = mempty
          , txOutDatumHash = Just datum
          }
    }

-- | Minting a single token
mint :: Value
mint = Value.singleton sym "sometoken" 1

mintOtherToken :: Value
mintOtherToken = Value.singleton sym "othertoken" 1

mintOtherSymbol :: Value
mintOtherSymbol = Value.singleton "c7" "sometoken" 1

ref :: TxOutRef
ref = TxOutRef "a0" 0

purpose :: ScriptPurpose
purpose = Spending ref

validator :: ValidatorHash
validator = "a1"

datum :: DatumHash
datum = "d0"

sym :: CurrencySymbol
sym = "c0"

signatories :: [PubKeyHash]
signatories = ["ab01fe235c", "123014", "abcdef"]

--------------------------------------------------------------------------------

getTxInfoPPlutus' s => Term s (PScriptContext #-> PAsData PTxInfo)
getTxInfo =
  plam $ \ctx ->
    pfield @"txInfo" # ctx

getMintPPlutus' s => Term s (PAsData PTxInfo #-> PAsData (PValue 'Sorted 'NoGuarantees))
getMint =
  plam $ \info ->
    pfield @"mint" # info

-- | Get validator from first input in ScriptContext's TxInfo
getCredentialsPPlutus' s => Term s PScriptContext -> Term s (PBuiltinList PData)
getCredentials ctx =
  let inp = pfield @"inputs" #$ pfield @"txInfo" # ctx
   in pmap # inputCredentialHash # inp

{- |
  Get the hash of the Credential in an input, treating
  PubKey & ValidatorHash identically.
-}
inputCredentialHashPPlutus' s => Term s (PTxInInfo #-> PData)
inputCredentialHash =
  phoistAcyclic $
    plam $ \inp ->
      let credential :: Term _ (PAsData PCredential)
          credential =
            (pfield @"credential")
              #$ (pfield @"address")
              #$ (pfield @"resolved" # inp)
       in phead #$ psndBuiltin #$ pasConstr # pforgetData credential

-- | Get first CurrencySymbol from Value
getSymPPlutus' s => Term s (PValue 'Sorted 'NonZero #-> PAsData PCurrencySymbol)
getSym =
  plam $ \v -> pfstBuiltin #$ phead # pto (pto v)

-- | `checkSignatory` implemented using `runCont`
checkSignatoryCont :: forall s. Term s (PPubKeyHash #-> PScriptContext #-> PUnit)
checkSignatoryCont = plam $ \ph ctx' ->
  pletFields @["txInfo", "purpose"] ctx' $ \ctx -> (`runCont` id) $ do
    purpose <- cont (pmatch $ getField @"purpose" ctx)
    pure $ case purpose of
      PSpending _ ->
        let signatoriesPPlutus' s => Term s (PBuiltinList (PAsData PPubKeyHash))
            signatories = pfield @"signatories" # getField @"txInfo" ctx
         in pif
              (pelem # pdata ph # signatories)
              -- Success!
              (pconstant ())
              -- Signature not present.
              perror
      _ ->
        ptraceError "checkSignatoryCont: not a spending tx"

-- | `checkSignatory` implemented using `runTermCont`
checkSignatoryTermContPPlutus' s => Term s (PPubKeyHash #-> PScriptContext #-> PUnit)
checkSignatoryTermCont = plam $ \ph ctx' -> unTermCont $ do
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
  tcont (pmatch $ getField @"purpose" ctx) >>= \case
    PSpending _ -> do
      let signatories = pfield @"signatories" # getField @"txInfo" ctx
      pure $
        pif
          (pelem # pdata ph # pfromData signatories)
          -- Success!
          (pconstant ())
          -- Signature not present.
          perror
    _ ->
      pure $ ptraceError "checkSignatoryCont: not a spending tx"

getFieldsPPlutus' s => Term s (PData #-> PBuiltinList PData)
getFields = phoistAcyclic $ plam $ \addr -> psndBuiltin #$ pasConstr # addr

dummyCurrency :: CurrencySymbol
dummyCurrency = Value.currencySymbol "\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11"

------------------- Mocking a ScriptContext ----------------------------------------

validContext0PPlutus' s => Term s PScriptContext
validContext0 = mkCtx validOutputs0 validDatums1

invalidContext1PPlutus' s => Term s PScriptContext
invalidContext1 = mkCtx invalidOutputs1 validDatums1

mkCtx :: [TxOut] -> [(DatumHash, Datum)] -> Term s PScriptContext
mkCtx outs l = pconstant (ScriptContext (info' outs l) purpose)
  where
    info' :: [TxOut] -> [(DatumHash, Datum)] -> TxInfo
    info' outs dat =
      info
        { txInfoData = dat
        , txInfoOutputs = outs
        }

validOutputs0 :: [TxOut]
validOutputs0 =
  [ TxOut
      { txOutAddress =
          Address (ScriptCredential validator) Nothing
      , txOutValue = mempty
      , txOutDatumHash = Just datum
      }
  ]

invalidOutputs1 :: [TxOut]
invalidOutputs1 =
  [ TxOut
      { txOutAddress =
          Address (ScriptCredential validator) Nothing
      , txOutValue = mempty
      , txOutDatumHash = Just datum
      }
  , TxOut
      { txOutAddress =
          Address (ScriptCredential validator) Nothing
      , txOutValue = mempty
      , txOutDatumHash = Nothing
      }
  ]

validDatums1 :: [(DatumHash, Datum)]
validDatums1 =
  [("d0", d0Dat)]

-- | Mock datum that is a list of integers.
d0Dat :: Datum
d0Dat = Datum $ toBuiltinData d0DatValue

d0DatValue :: [Integer]
d0DatValue = [1 .. 10]

------------------- Property tests -------------------------------------------------

propPlutarchtypeCanBeRecovered ::
  forall a.
  ( Eq a
  , Show a
  , PConstant a
  , PIsData (PConstanted a)
  , PTryFrom PData (PAsData (PConstanted a))
  , PLifted (PConstanted a) ~ a
  , ToData a
  ) =>
  a ->
  Property
propPlutarchtypeCanBeRecovered addr =
  addr
    === plift
      ( unTermCont $
          pfromData . fst <$> tcont (ptryFrom @(PAsData (PConstanted a)) $ pforgetData $ pconstantData addr)
      )

prop_pmaybedata_can_be_recovered :: Maybe StakingCredential -> Property
prop_pmaybedata_can_be_recovered addr =
  addr
    === plift
      ( unTermCont $
          pfromData . fst
            <$> tcont
              (ptryFrom @(PAsData (PMaybeData PStakingCredential)) $ pforgetData $ pconstantData addr)
      )

pshouldReallyBe :: ClosedTerm a -> ClosedTerm a -> Expectation
pshouldReallyBe a b = pshouldBe b a
