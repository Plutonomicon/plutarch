{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}

module Plutarch.MonadicSpec (spec) where

import Control.Monad.Trans.Cont (cont, runCont)
import Plutarch.Api.V1 (
  PAddress (PAddress),
  PCredential,
  PMaybeData,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose (PSpending),
  PStakingCredential,
 )
import qualified Plutarch.ApiSpec as ApiSpec
import Plutarch.List (pconvertLists)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import Plutarch.Test
import PlutusLedgerApi.V1
import Test.Hspec

spec :: Spec
spec = do
  describe "monadic" $ do
    describe "pmatch-twice" . pgoldenSpec $ do
      -- We expect all these benchmarks to produce equivalent numbers
      let integerList :: [Integer] -> Term s (PList PInteger)
          integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs
          xs = integerList [1 .. 10]
      "normal"
        @| pmatch xs
        $ \case
          PSCons _x xs' -> do
            pmatch xs' $ \case
              PSCons _ xs'' ->
                xs''
              PSNil -> perror
          PSNil -> perror
      "do"
        @| P.do
          PSCons _ xs' <- pmatch xs
          PSCons _ xs'' <- pmatch xs'
          xs''
      "cont"
        @| flip runCont id
        $ do
          ls <- cont $ pmatch xs
          case ls of
            PSCons _ xs' -> do
              ls' <- cont $ pmatch xs'
              case ls' of
                PSCons _ xs'' -> pure xs''
                PSNil -> pure perror
            PSNil -> pure perror
      "termcont"
        @| unTermCont
        $ do
          PSCons _ xs' <- TermCont $ pmatch xs
          PSCons _ xs'' <- TermCont $ pmatch xs'
          pure xs''
    describe "api.example" $ do
      -- The checkSignatory family of functions implicitly use tracing due to
      -- monadic syntax, and as such we need two sets of tests here.
      describe "signatory" . pgoldenSpec $ do
        let aSig :: PubKeyHash = "ab01fe235c"
        "do" @\ do
          "succeeds" @| checkSignatory # pconstant aSig # ApiSpec.ctx @-> psucceeds
          "fails" @| checkSignatory # pconstant "41" # ApiSpec.ctx @-> pfails
      describe "getFields" . pgoldenSpec $ do
        "0" @| getFields

checkSignatoryPPlutus' s => Term s (PPubKeyHash #-> PScriptContext #-> PUnit)
checkSignatory = plam $ \ph ctx' ->
  pletFields @["txInfo", "purpose"] ctx' $ \ctx -> P.do
    PSpending _ <- pmatch $ ctx.purpose
    let signatories = pfield @"signatories" # ctx.txInfo
    pif
      (pelem # pdata ph # pfromData signatories)
      -- Success!
      (pconstant ())
      -- Signature not present.
      perror

getFieldsPPlutus' s => Term s (PAddress #-> PDataRecord '["credential" ':= PCredential, "stakingCredential" ':= PMaybeData PStakingCredential])
getFields = phoistAcyclic $
  plam $ \addr -> P.do
    PAddress addrFields <- pmatch addr
    addrFields
