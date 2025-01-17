module Plutarch.Builtin.BLS (
  PBuiltinBLS12_381_G1_Element (PBuiltinBLS12_381_G1_Element),
  PBuiltinBLS12_381_G2_Element (PBuiltinBLS12_381_G2_Element),
  PBuiltinBLS12_381_MlResult (PBuiltinBLS12_381_MlResult),
  pbls12_381_G1_add,
  pbls12_381_G1_scalarMul,
  pbls12_381_G1_neg,
  pbls12_381_G1_compress,
  pbls12_381_G1_uncompress,
  pbls12_381_G1_hashToGroup,
  pbls12_381_G1_compressed_zero,
  pbls12_381_G1_compressed_generator,
  pbls12_381_G2_add,
  pbls12_381_G2_scalarMul,
  pbls12_381_G2_neg,
  pbls12_381_G2_compress,
  pbls12_381_G2_uncompress,
  pbls12_381_G2_hashToGroup,
  pbls12_381_G2_compressed_zero,
  pbls12_381_G2_compressed_generator,
  pbls12_381_millerLoop,
  pbls12_381_mulMlResult,
  pbls12_381_finalVerify,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Term (Term, punsafeBuiltin, punsafeConstantInternal, (:-->))
import PlutusCore qualified as PLC
import PlutusCore.Crypto.BLS12_381.G1 qualified as BLS12_381.G1
import PlutusCore.Crypto.BLS12_381.G2 qualified as BLS12_381.G2

{- | A point on the BLS12-381 G1 curve.

@since 1.9.0
-}
newtype PBuiltinBLS12_381_G1_Element s = PBuiltinBLS12_381_G1_Element (Term s POpaque)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)

{- | Add two points on the BLS12-381 G1 curve.

@since 1.9.0
-}
pbls12_381_G1_add :: Term s (PBuiltinBLS12_381_G1_Element :--> PBuiltinBLS12_381_G1_Element :--> PBuiltinBLS12_381_G1_Element)
pbls12_381_G1_add = punsafeBuiltin PLC.Bls12_381_G1_add

{- | Multiply a point on the BLS12-381 G1 curve by a scalar.

@since 1.9.0
-}
pbls12_381_G1_scalarMul :: Term s (PInteger :--> PBuiltinBLS12_381_G1_Element :--> PBuiltinBLS12_381_G1_Element)
pbls12_381_G1_scalarMul = punsafeBuiltin PLC.Bls12_381_G1_scalarMul

{- | Negate a point on the BLS12-381 G1 curve.

@since 1.9.0
-}
pbls12_381_G1_neg :: Term s (PBuiltinBLS12_381_G1_Element :--> PBuiltinBLS12_381_G1_Element)
pbls12_381_G1_neg = punsafeBuiltin PLC.Bls12_381_G1_neg

{- | Compress a point on the BLS12-381 G1 curve to a byte string.

@since 1.9.0
-}
pbls12_381_G1_compress :: Term s (PBuiltinBLS12_381_G1_Element :--> PByteString)
pbls12_381_G1_compress = punsafeBuiltin PLC.Bls12_381_G1_compress

{- | Uncompress a byte string to a point on the BLS12-381 G1 curve.

@since 1.9.0
-}
pbls12_381_G1_uncompress :: Term s (PByteString :--> PBuiltinBLS12_381_G1_Element)
pbls12_381_G1_uncompress = punsafeBuiltin PLC.Bls12_381_G1_uncompress

{- | Hash a message to a point on the BLS12-381 G1 curve.

@since 1.9.0
-}
pbls12_381_G1_hashToGroup :: Term s (PByteString :--> PByteString :--> PBuiltinBLS12_381_G1_Element)
pbls12_381_G1_hashToGroup = punsafeBuiltin PLC.Bls12_381_G1_hashToGroup

{- | The compressed representation of the zero point on the BLS12-381 G1 curve.

@since 1.9.0
-}
pbls12_381_G1_compressed_zero :: Term s PByteString
pbls12_381_G1_compressed_zero = punsafeConstantInternal $ PLC.someValue BLS12_381.G1.compressed_zero

{- | The compressed representation of the generator point on the BLS12-381 G1 curve.

@since 1.9.0
-}
pbls12_381_G1_compressed_generator :: Term s PByteString
pbls12_381_G1_compressed_generator = punsafeConstantInternal $ PLC.someValue BLS12_381.G1.compressed_generator

-- | @since 1.9.0
newtype PBuiltinBLS12_381_G2_Element s = PBuiltinBLS12_381_G2_Element (Term s POpaque)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)

{- | Add two points on the BLS12-381 G2 curve.

@since 1.9.0
-}
pbls12_381_G2_add :: Term s (PBuiltinBLS12_381_G2_Element :--> PBuiltinBLS12_381_G2_Element :--> PBuiltinBLS12_381_G2_Element)
pbls12_381_G2_add = punsafeBuiltin PLC.Bls12_381_G2_add

{- | Multiply a point on the BLS12-381 G2 curve by a scalar.

@since 1.9.0
-}
pbls12_381_G2_scalarMul :: Term s (PInteger :--> PBuiltinBLS12_381_G2_Element :--> PBuiltinBLS12_381_G2_Element)
pbls12_381_G2_scalarMul = punsafeBuiltin PLC.Bls12_381_G2_scalarMul

{- | Negate a point on the BLS12-381 G2 curve.

@since 1.9.0
-}
pbls12_381_G2_neg :: Term s (PBuiltinBLS12_381_G2_Element :--> PBuiltinBLS12_381_G2_Element)
pbls12_381_G2_neg = punsafeBuiltin PLC.Bls12_381_G2_neg

{- | Compress a point on the BLS12-381 G2 curve to a byte string.

@since 1.9.0
-}
pbls12_381_G2_compress :: Term s (PBuiltinBLS12_381_G2_Element :--> PByteString)
pbls12_381_G2_compress = punsafeBuiltin PLC.Bls12_381_G2_compress

{- | Uncompress a byte string to a point on the BLS12-381 G2 curve.

@since 1.9.0
-}
pbls12_381_G2_uncompress :: Term s (PByteString :--> PBuiltinBLS12_381_G2_Element)
pbls12_381_G2_uncompress = punsafeBuiltin PLC.Bls12_381_G2_uncompress

{- | Hash a message to a point on the BLS12-381 G2 curve.

@since 1.9.0
-}
pbls12_381_G2_hashToGroup :: Term s (PByteString :--> PByteString :--> PBuiltinBLS12_381_G2_Element)
pbls12_381_G2_hashToGroup = punsafeBuiltin PLC.Bls12_381_G2_hashToGroup

{- | The compressed representation of the zero point on the BLS12-381 G2 curve.

@since 1.9.0
-}
pbls12_381_G2_compressed_zero :: Term s PByteString
pbls12_381_G2_compressed_zero = punsafeConstantInternal $ PLC.someValue BLS12_381.G2.compressed_zero

{- | The compressed representation of the generator point on the BLS12-381 G2 curve.

@since 1.9.0
-}
pbls12_381_G2_compressed_generator :: Term s PByteString
pbls12_381_G2_compressed_generator = punsafeConstantInternal $ PLC.someValue BLS12_381.G2.compressed_generator

{- | Represents the result of a Miller loop operation in BLS12-381 pairing.

@since 1.9.0
-}
newtype PBuiltinBLS12_381_MlResult s = PBuiltinBLS12_381_MlResult (Term s POpaque)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)

{- | Perform a Miller loop operation on a G1 and G2 element.

@since 1.9.0
-}
pbls12_381_millerLoop :: Term s (PBuiltinBLS12_381_G1_Element :--> PBuiltinBLS12_381_G2_Element :--> PBuiltinBLS12_381_MlResult)
pbls12_381_millerLoop = punsafeBuiltin PLC.Bls12_381_millerLoop

{- | Multiply two Miller loop results.

@since 1.9.0
-}
pbls12_381_mulMlResult :: Term s (PBuiltinBLS12_381_MlResult :--> PBuiltinBLS12_381_MlResult :--> PBuiltinBLS12_381_MlResult)
pbls12_381_mulMlResult = punsafeBuiltin PLC.Bls12_381_mulMlResult

{- | Perform the final verification step in BLS12-381 pairing.

@since 1.9.0
-}
pbls12_381_finalVerify :: Term s (PBuiltinBLS12_381_MlResult :--> PBuiltinBLS12_381_MlResult :--> PBool)
pbls12_381_finalVerify = punsafeBuiltin PLC.Bls12_381_finalVerify
