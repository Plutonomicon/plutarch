module Plutarch.Extra.ApiSpec (spec) where

import Plutarch.Extra.Api
import Plutarch.Prelude

import Plutarch.Api.V1 (PScriptPurpose (PSpending))
import Plutarch.ApiSpec (d0DatValue, inp, validContext0, validOutputs0)
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Maybe (pfromJust)
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "extra.api" $ do
    let ctx = validContext0
    pgoldenSpec $ do
      "pfindOwnInput"
        @| ( unTermCont $ do
              ctxF <- tcont $ pletFields @["txInfo", "purpose"] ctx
              pmatchC (getField @"purpose" ctxF) >>= \case
                PSpending outRef' -> do
                  let outRef = pfield @"_0" # outRef'
                      inputs = pfield @"inputs" # (getField @"txInfo" ctxF)
                  pure $ pfindOwnInput # inputs # outRef
                _ ->
                  pure perror
           )
          @-> \res ->
            passert (pfromJust # res #== pconstant inp)
      "pgetContinuingOutputs"
        @| ( unTermCont $ do
              ctxF <- tcont $ pletFields @["txInfo", "purpose"] ctx
              pmatchC (getField @"purpose" ctxF) >>= \case
                PSpending outRef' -> do
                  let outRef = pfield @"_0" # outRef'
                      inputs = pfield @"inputs" #$ getField @"txInfo" ctxF
                      outputs = pfield @"outputs" #$ getField @"txInfo" ctxF
                  pure $ pgetContinuingOutputs # inputs # outputs # outRef
                _ ->
                  pure perror
           )
          @-> \txOuts ->
            passert $ txOuts #== pconstant validOutputs0
      "pparseDatum"
        @| ( pparseDatum @(PBuiltinList (PAsData PInteger))
              # pconstant "d0"
                #$ pfield @"datums"
                #$ pfield @"txInfo"
              # ctx
           )
          @-> \res ->
            passert $ res #== pcon (PJust $ pdata $ d0DatTerm)

-- | The Plutarch term we expect when decoding `d0Dat`.
d0DatTermPPlutus' s => Term s (PBuiltinList (PAsData PInteger))
d0DatTerm = liftList $ flip fmap d0DatValue $ \i -> pdata $ pconstant i

liftList :: PLift a => [Term s a] -> Term s (PBuiltinList a)
liftList = \case
  [] -> pnil
  (x : xs) -> pcons # x # liftList xs
