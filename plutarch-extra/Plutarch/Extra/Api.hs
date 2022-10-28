module Plutarch.Extra.Api (
  pgetContinuingOutputs,
  pfindOwnInput,
  pparseDatum,
) where

import Plutarch.Api.V1 (
  PAddress,
  PDatum,
  PDatumHash,
  PTuple,
  PTxInInfo,
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Prelude

{- | Find the output txns corresponding to the input being validated.

  Takes as arguments the inputs, outputs and the spending transaction referenced
  from `PScriptPurpose`.

  __Example:__

  @
  ctx <- tcont $ pletFields @["txInfo", "purpose"] sc
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # (getField @"txInfo" ctx)
          outputs = pfield @"outputs" # (getField @"txInfo" ctx)
      pure $ pgetContinuingOutputs # inputs # outputs # outRef
    _ ->
      pure $ ptraceError "not a spending tx"
  @
-}
pgetContinuingOutputsPPlutus' s => Term s (PBuiltinList PTxInInfo #-> PBuiltinList PTxOut #-> PTxOutRef #-> PBuiltinList PTxOut)
pgetContinuingOutputs = phoistAcyclic $
  plam $ \inputs outputs outRef ->
    pmatch (pfindOwnInput # inputs # outRef) $ \case
      PJust tx -> do
        let resolved = pfield @"resolved" # tx
            outAddr = pfield @"address" # resolved
        pfilter # (matches # outAddr) # outputs
      PNothing ->
        ptraceError "can't get any continuing outputs"
  where
    matchesPPlutus' s => Term s (PAddress #-> PTxOut #-> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut ->
        adr #== pfield @"address" # txOut

{- | Find the input being spent in the current transaction.

  Takes as arguments the inputs, as well as the spending transaction referenced from `PScriptPurpose`.

  __Example:__

  @
  ctx <- tcont $ pletFields @["txInfo", "purpose"] sc
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # (getField @"txInfo" ctx)
      pure $ pfindOwnInput # inputs # outRef
    _ ->
      pure $ ptraceError "not a spending tx"
  @
-}
pfindOwnInputPPlutus' s => Term s (PBuiltinList PTxInInfo #-> PTxOutRef #-> PMaybe PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \inputs outRef ->
    pfind # (matches # outRef) # inputs
  where
    matchesPPlutus' s => Term s (PTxOutRef #-> PTxInInfo #-> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        outref #== pfield @"outRef" # txininfo

{- | Lookup up the datum given the datum hash.

  Takes as argument the datum assoc list from a `PTxInfo`. Validates the datum
  using `PTryFrom`.

  __Example:__

  @
  pparseDatum @MyType # datumHash #$ pfield @"datums" # txinfo
  @
-}
pparseDatum :: forall a s. PTryFrom PData (PAsData a) => Term s (PDatumHash #-> PBuiltinList (PAsData (PTuple PDatumHash PDatum)) #-> PMaybe (PAsData a))
pparseDatum = phoistAcyclic $
  plam $ \dh datums ->
    pmatch (pfind # (matches # dh) # datums) $ \case
      PNothing ->
        pcon PNothing
      PJust datumTuple ->
        let datum :: Term _ PData
            datum = pto $ pfromData $ pfield @"_1" # pfromData datumTuple
         in pcon $ PJust $ ptryFromData datum
  where
    matches :: forall k v s. (PEq k, PIsData k) => Term s (k #-> PAsData (PTuple k v) #-> PBool)
    matches = phoistAcyclic $
      plam $ \a ab ->
        a #== pfield @"_0" # ab

ptryFromData :: forall a s. PTryFrom PData (PAsData a) => Term s PData -> Term s (PAsData a)
ptryFromData x = unTermCont $ fst <$> tcont (ptryFrom @(PAsData a) x)
