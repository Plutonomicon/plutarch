module Plutarch.Crypto (
  -- ** from V1
  psha2_256,
  psha3_256,
  pblake2b_256,
  pverifySignature,
  pverifyEd25519Signature,

  -- ** from V2
  pverifyEcdsaSecp256k1Signature,
  pverifySchnorrSecp256k1Signature,
) where

import Plutarch (
  Term,
  type (#->),
 )
import Plutarch.Unsafe (punsafeBuiltin)

import Plutarch.Bool (PBool)
import Plutarch.ByteString (PByteString)
import qualified PlutusCore as PLC

-- | Hash a 'PByteString' using SHA-256.
psha2_256PPlutus' s => Term s (PByteString #-> PByteString)
psha2_256 = punsafeBuiltin PLC.Sha2_256

-- | Hash a 'PByteString' using SHA3-256.
psha3_256PPlutus' s => Term s (PByteString #-> PByteString)
psha3_256 = punsafeBuiltin PLC.Sha3_256

-- | Hash a 'PByteString' using Blake2B-256.
pblake2b_256PPlutus' s => Term s (PByteString #-> PByteString)
pblake2b_256 = punsafeBuiltin PLC.Blake2b_256

-- | Verify the signature against the public key and message.
pverifySignaturePPlutus' s => Term s (PByteString #-> PByteString #-> PByteString #-> PBool)
pverifySignature = pverifyEd25519Signature
{-# DEPRECATED pverifySignature "use one of the Ed25519, Schnorr- or ECDSA Secp256k1 signature verification functions" #-}

{- | Verify an ED25519 signature
   arguments are in this order: pubkey, message, signature
-}
pverifyEd25519SignaturePPlutus' s => Term s (PByteString #-> PByteString #-> PByteString #-> PBool)
pverifyEd25519Signature = punsafeBuiltin PLC.VerifyEd25519Signature

{- | Verify an ECDSA SECP256k1 signature
   arguments are in this order: pubkey, message, signature
-}
pverifyEcdsaSecp256k1SignaturePPlutus' s => Term s (PByteString #-> PByteString #-> PByteString #-> PBool)
pverifyEcdsaSecp256k1Signature = punsafeBuiltin PLC.VerifyEcdsaSecp256k1Signature

{- | Verify a Schnorr SECP256k1 signature
   arguments are in this order: pubkey, message, signature
-}
pverifySchnorrSecp256k1SignaturePPlutus' s => Term s (PByteString #-> PByteString #-> PByteString #-> PBool)
pverifySchnorrSecp256k1Signature = punsafeBuiltin PLC.VerifySchnorrSecp256k1Signature
