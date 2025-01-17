{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Plutarch.Builtin.ByteString (
  PByteString (..),
  PByte (..),
  PLogicOpSemantics (..),
  PEndianness (..),
  ppadding,
  ptruncation,
  pmostSignificantFirst,
  pmostSignificantLast,
  pandBS,
  porBS,
  pxorBS,
  pcomplementBS,
  preplicateBS,
  pconsBS,
  pbyteToInteger,
  pintegerToByte,
  psliceBS,
  plengthBS,
  pindexBS,
  phexByteStr,
  pbyteStringToInteger,
  pintegerToByteString,
) where

import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Bool (PBool, pfalse, ptrue)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.Opaque (POpaque)
import {-# SOURCE #-} Plutarch.Internal.PLam (plam)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  punsafeBuiltin,
  punsafeCoerce,
  punsafeConstantInternal,
  (#),
  (:-->),
 )
import PlutusCore qualified as PLC

-- | Plutus 'BuiltinByteString'
newtype PByteString s = PByteString (Term s POpaque)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)

{- | A Plutarch-level representation of bytes.

= Note =

This type is intentionally quite restrictive, as it's not really meant to be
computed with. Instead, it ensures certain operations' type safety while also
allowing more sensible signatures. If you want to do anything with 'PByte's,
we recommend converting them to 'PInteger's first.

@since WIP
-}
newtype PByte (s :: S) = PByte (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass (SOP.Generic)

instance Semigroup (Term s PByteString) where
  x <> y = punsafeBuiltin PLC.AppendByteString # x # y

instance Monoid (Term s PByteString) where
  mempty = punsafeConstantInternal $ PLC.someValue BS.empty

{- | Type designating whether a conversion should be most-significant-first or
most-significant-last. See
[CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121#representation)
for more details on this.

@since WIP
-}
newtype PEndianness (s :: S) = PEndianness (Term s PBool)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass (SOP.Generic)

{- | Indicates the conversion should be most-significant-first.

@since WIP
-}
pmostSignificantFirst :: forall (s :: S). Term s PEndianness
pmostSignificantFirst = punsafeCoerce ptrue

{- | Indicates the conversion should be most-significant-last.

@since WIP
-}
pmostSignificantLast :: forall (s :: S). Term s PEndianness
pmostSignificantLast = punsafeCoerce pfalse

{- | Indicates that padding semantics should be used.

@since WIP
-}
ppadding :: forall (s :: S). Term s PLogicOpSemantics
ppadding = punsafeCoerce ptrue

{- | Indicates that truncation semantics should be used.

@since WIP
-}
ptruncation :: forall (s :: S). Term s PLogicOpSemantics
ptruncation = punsafeCoerce pfalse

{- | Type designating whether logical operations should use padding or
truncation semantics. See
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#padding-versus-truncation-semantics)
for more details on this.

@since WIP
-}
newtype PLogicOpSemantics (s :: S) = PLogicOpSemantics (Term s PBool)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)

{- | Perform the logical AND of two 'PByteString's, as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#bitwiselogicaland).
The 'PLogicOpSemantics' argument specifies what should be done if the lengths
of the two 'PByteString' arguments do not match.

@since WIP
-}
pandBS ::
  forall (s :: S).
  Term s (PLogicOpSemantics :--> PByteString :--> PByteString :--> PByteString)
pandBS = punsafeBuiltin PLC.AndByteString

{- | Perform the logical OR of two 'PByteString's, as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#bitwiselogicalor).
The 'PLogicOpSemantics' argument specifies what should be done if the lengths
of the two 'PByteString' arguments do not match.

@since WIP
-}
porBS ::
  forall (s :: S).
  Term s (PLogicOpSemantics :--> PByteString :--> PByteString :--> PByteString)
porBS = punsafeBuiltin PLC.OrByteString

{- | Perform the logical XOR of two 'PByteString's, as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#bitwiselogicalxor).
The 'PLogicOpSemantics' argument specifies what should be done if the lengths
of the two 'PByteString' arguments do not match.

@since WIP
-}
pxorBS ::
  forall (s :: S).
  Term s (PLogicOpSemantics :--> PByteString :--> PByteString :--> PByteString)
pxorBS = punsafeBuiltin PLC.XorByteString

{- | Perform the logical complement of a 'PByteString', as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#bitwiselogicalcomplement).

@since WIP
-}
pcomplementBS ::
  forall (s :: S).
  Term s (PByteString :--> PByteString)
pcomplementBS = punsafeBuiltin PLC.ComplementByteString

{- | Given a desired length and a 'PByte', construct a 'PByteString' of the
specified length (0 if negative) consisting entirely of that 'PByte'.

@since WIP
-}
preplicateBS :: forall (s :: S). Term s (PInteger :--> PByte :--> PByteString)
preplicateBS = punsafeBuiltin PLC.ReplicateByte

{- | Prepend a 'PByte' to a 'PByteString.

@since WIP
-}
pconsBS :: Term s (PByte :--> PByteString :--> PByteString)
pconsBS = punsafeBuiltin PLC.ConsByteString

{- | Convert a 'PByte' into its corresponding 'PInteger'.

@since WIP
-}
pbyteToInteger :: Term s (PByte :--> PInteger)
pbyteToInteger = phoistAcyclic $ plam punsafeCoerce

{- | Try to convert a 'PInteger' into its corresponding 'PByte'. This operation
unchecked: use with care.

@since WIP
-}
pintegerToByte :: Term s (PInteger :--> PByte)
pintegerToByte = phoistAcyclic $ plam punsafeCoerce

{- | Slice a 'PByteString' with given start index and slice length.

>>> (pslice # 2 # 3 phexByteStr "4102afde5b2a") #== phexByteStr "afde5b"
-}
psliceBS :: Term s (PInteger :--> PInteger :--> PByteString :--> PByteString)
psliceBS = punsafeBuiltin PLC.SliceByteString

-- | Find the length of a 'PByteString'.
plengthBS :: Term s (PByteString :--> PInteger)
plengthBS = punsafeBuiltin PLC.LengthOfByteString

{- | Given a valid index into a 'PByteString', returns the 'PByte' at that
index. Will crash if given an out-of-bounds index.

@since WIP
-}
pindexBS :: Term s (PByteString :--> PInteger :--> PByte)
pindexBS = punsafeBuiltin PLC.IndexByteString

-- | Interpret a hex string as a PByteString.
phexByteStr :: HasCallStack => String -> Term s PByteString
phexByteStr = punsafeConstantInternal . PLC.someValue . BS.pack . f
  where
    f "" = []
    f [_] = error "UnevenLength"
    f (x : y : rest) = (hexDigitToWord8 x * 16 + hexDigitToWord8 y) : f rest

{- | Convert a 'PByteString' into a 'PInteger', as per
[CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121#builtinbytestringtointeger).

@since WIP
-}
pbyteStringToInteger ::
  forall (s :: S).
  Term s (PEndianness :--> PByteString :--> PInteger)
pbyteStringToInteger = punsafeBuiltin PLC.ByteStringToInteger

{- | Converts a 'PInteger' into a 'PByteString', given a desired endianness and
target length. For more details, see [CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121#builtinintegertobytestring).

= Note

This conversion is unsafe. It will fail if any of the following occur:

1. The size is negative.
2. The size is too large (currently if over 8196 bytes).
3. The size won't fit the integer to convert.
-}
pintegerToByteString ::
  forall (s :: S).
  Term s (PEndianness :--> PInteger :--> PInteger :--> PByteString)
pintegerToByteString = punsafeBuiltin PLC.IntegerToByteString

-- Helpers

hexDigitToWord8 :: HasCallStack => Char -> Word8
hexDigitToWord8 = f . toLower
  where
    f :: Char -> Word8
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f '9' = 9
    f 'a' = 10
    f 'b' = 11
    f 'c' = 12
    f 'd' = 13
    f 'e' = 14
    f 'f' = 15
    f c = error ("InvalidHexDigit " <> [c])
