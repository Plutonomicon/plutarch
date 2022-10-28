module Plutarch.ScriptsSpec (
  authorizedValidator,
  authorizedPolicy,
  authorizedStakeValidator,
  authValidatorCompiled,
  validatorEncoded,
  validatorHashEncoded,
  authValidatorHash,
  authStakeValidatorCompiled,
  stakeValidatorEncoded,
  authStakeValidatorHash,
  stakeValidatorHashEncoded,
  authPolicyCompiled,
  policyEncoded,
  policySymEncoded,
  authPolicySymbol,
  spec,
) where

import qualified Codec.CBOR.Write as Write
import Codec.Serialise (Serialise, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.Coerce (coerce)
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import qualified PlutusLedgerApi.V1 as Plutus

import Plutarch.Api.V1 (
  PScriptContext,
  mintingPolicySymbol,
  mkMintingPolicy,
  mkStakeValidator,
  mkValidator,
  stakeValidatorHash,
  validatorHash,
  type PMintingPolicy,
  type PStakeValidator,
  type PValidator,
 )
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Builtin (pasByteStr)
import Plutarch.Crypto (pverifyEd25519Signature)
import Plutarch.Prelude
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "scripts" . pgoldenSpec $ do
    "auth_validator" @\ do
      "0" @| authValidatorTerm
      "hash" @| pconstant validatorHashEncoded
    "auth_policy" @\ do
      "0" @| authPolicyTerm
      "hash" @| pconstant policySymEncoded
    "auth_stake_validator" @\ do
      "0" @| authStakeValidatorTerm
      "hash" @| pconstant stakeValidatorHashEncoded

encodeSerialise :: Serialise a => a -> Text
encodeSerialise = TE.decodeUtf8 . Base16.encode . Write.toStrictByteString . encode

type PSignature = PByteString
type PPubKey = PByteString
type PubKey = ByteString

{- |
  A parameterized Validator which may be unlocked
    by signing the Datum Message with the parameter PubKey.
-}
authorizedValidator ::
  ClosedTerm PPubKey ->
  Term s PByteString ->
  Term s PSignature ->
  Term s PScriptContext ->
  Term s POpaque
authorizedValidator authKey datumMessage redeemerSig _ctx =
  pif
    (pverifyEd25519Signature # authKey # datumMessage # redeemerSig)
    (popaque $ pcon PUnit)
    perror

{- |
  A parameterized MintingPolicy which allows minting if
   the parameter PubKeyHash signs the transaction.
-}
authorizedPolicy ::
  forall s.
  ClosedTerm (PAsData PPubKeyHash) ->
  Term s PData ->
  Term s PScriptContext ->
  Term s POpaque
authorizedPolicy authHash _redeemer ctx =
  let sigsPPlutus' s => Term s (PBuiltinList (PAsData PPubKeyHash))
      sigs = pfromData (pfield @"signatories" #$ pfield @"txInfo" # ctx)
   in pif
        (pelem # authHash # sigs)
        (popaque $ pcon PUnit)
        perror

{- |
  A parameterized StakeValidator which allows any StakeValidator action
  if the parameter PubKeyHash signs the transaction.
-}
authorizedStakeValidator ::
  forall s.
  ClosedTerm (PAsData PPubKeyHash) ->
  Term s PData ->
  Term s PScriptContext ->
  Term s POpaque
authorizedStakeValidator authHash _redeemer ctx =
  let sigsPPlutus' s => Term s (PBuiltinList (PAsData PPubKeyHash))
      sigs = pfromData (pfield @"signatories" #$ pfield @"txInfo" # ctx)
   in pif
        (pelem # authHash # sigs)
        (popaque $ pcon PUnit)
        perror

adminPubKey :: PubKey
adminPubKey = "11661a8aca9b09bb93eefda295b5da2be3f944d1f4253ab29da17db580f50d02d26218e33fbba5e0cc1b0c0cadfb67a5f9a90157dcc19eecd7c9373b0415c888"

adminPubKeyHash :: Plutus.PubKeyHash
adminPubKeyHash = "cc1360b04bdd0825e0c6552abb2af9b4df75b71f0c7cca20256b1f4f"

{- |
  We can compile a `Validator` using `mkValidator` &
  `pwrapValidatorFromData`
-}
authValidatorCompiled :: Plutus.Validator
authValidatorCompiled = mkValidator def authValidatorTerm

authValidatorTerm :: ClosedTerm PValidator
authValidatorTerm =
  plam $ \datum redeemer ctx ->
    authorizedValidator
      (pconstant adminPubKey)
      (pasByteStr # datum)
      (pasByteStr # redeemer)
      ctx

-- | `validatorHash` gets the Plutus `ValidatorHash`
authValidatorHash :: Plutus.ValidatorHash
authValidatorHash = validatorHash authValidatorCompiled

-- | Similarly, for a MintingPolicy
authPolicyCompiled :: Plutus.MintingPolicy
authPolicyCompiled = mkMintingPolicy def authPolicyTerm

authPolicyTerm :: ClosedTerm PMintingPolicy
authPolicyTerm =
  plam $ \redeemer ctx ->
    authorizedPolicy
      (pconstantData adminPubKeyHash)
      redeemer
      ctx

-- | `mintingPolicySymbol` gets the Plutus `CurrencySymbol`
authPolicySymbol :: Plutus.CurrencySymbol
authPolicySymbol =
  mintingPolicySymbol authPolicyCompiled

-- | ...And for a StakeValidator
authStakeValidatorCompiled :: Plutus.StakeValidator
authStakeValidatorCompiled = mkStakeValidator def authStakeValidatorTerm

authStakeValidatorTerm :: ClosedTerm PStakeValidator
authStakeValidatorTerm =
  plam $ \redeemer ctx ->
    authorizedStakeValidator
      (pconstantData adminPubKeyHash)
      redeemer
      ctx

-- | `stakeValidatorHash` gets the Plutus `StakeValidatorHash`
authStakeValidatorHash :: Plutus.StakeValidatorHash
authStakeValidatorHash = stakeValidatorHash authStakeValidatorCompiled

-- | `encodeSerialise` will get the hex-encoded serialisation of a script
validatorEncoded :: Text
validatorEncoded = encodeSerialise authValidatorCompiled

-- | Similarly, with a `MintingPolicy`
policyEncoded :: Text
policyEncoded = encodeSerialise authPolicyCompiled

-- | And with a `StakeValidator`
stakeValidatorEncoded :: Text
stakeValidatorEncoded = encodeSerialise authStakeValidatorCompiled

{- |
  We can also encode `ValidatorHash` the same way.

  NB:
  The serialisation from Codec.Serialise will prepend a 4-hexit prefix,
  tagging the type, so this will differ slightly from the encoding
  of the `Show` & `IsString` instances.
  Also note that this is not the addr1/CIP-0019 Address encoding of the script.
-}
validatorHashEncoded :: Text
validatorHashEncoded = encodeSerialise (coerce authValidatorHash :: Plutus.BuiltinByteString)

-- | The same goes for `CurrencySymbol`
policySymEncoded :: Text
policySymEncoded = encodeSerialise (coerce authPolicySymbol :: Plutus.BuiltinByteString)

-- | ... And `StakeValidatorHash`
stakeValidatorHashEncoded :: Text
stakeValidatorHashEncoded = encodeSerialise (coerce authStakeValidatorHash :: Plutus.BuiltinByteString)
