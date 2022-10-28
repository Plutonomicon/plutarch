{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.ByteString (
  PByteString,
  phexByteStr,
  pbyteStr,
  pconsBS,
  psliceBS,
  plengthBS,
  pindexBS,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Plutarch.Bool (PEq, POrd, PPartialOrd, (#<), (#<=), (#==))
import Plutarch.Integer (PInteger)
import Plutarch.Internal (Term, (#), (#->))
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Other (POpaque)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PlutusType)
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Unsafe (punsafeBuiltin)
import qualified PlutusCore as PLC

-- | Plutus 'BuiltinByteString'
data PByteString s = PByteString (Term s POpaque)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PByteString where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PByteString where type PLifted PByteString = ByteString
deriving via (DerivePConstantDirect ByteString PByteString) instance PConstantDecl ByteString

instance PEq PByteString where
  x #== y = punsafeBuiltin PLC.EqualsByteString # x # y

instance PPartialOrd PByteString where
  x #<= y = punsafeBuiltin PLC.LessThanEqualsByteString # x # y
  x #< y = punsafeBuiltin PLC.LessThanByteString # x # y

instance POrd PByteString

instance Semigroup (Term s PByteString) where
  x <> y = punsafeBuiltin PLC.AppendByteString # x # y

instance Monoid (Term s PByteString) where
  mempty = pconstant BS.empty

-- | Interpret a hex string as a PByteString.
phexByteStr :: HasCallStack => String -> Term s PByteString
phexByteStr = pconstant . BS.pack . f
  where
    f "" = []
    f [_] = error "UnevenLength"
    f (x : y : rest) = (hexDigitToWord8 x * 16 + hexDigitToWord8 y) : f rest

{-# DEPRECATED pbyteStr "Use `pconstant` instead." #-}

-- | Construct a PByteString term from a Haskell bytestring.
pbyteStr :: ByteString -> Term s PByteString
pbyteStr = pconstant

-----------------------------------------------------------
-- The following functions should be import qualified. --
-----------------------------------------------------------

-- | Prepend a byte, represented by a non negative 'PInteger', to a 'PBytestring'.
pconsBSPPlutus' s => Term s (PInteger #-> PByteString #-> PByteString)
pconsBS = punsafeBuiltin PLC.ConsByteString

{- | Slice a 'PByteString' with given start index and slice length.

>>> (pslice # 2 # 3 phexByteStr "4102afde5b2a") #== phexByteStr "afde5b"
-}
psliceBSPPlutus' s => Term s (PInteger #-> PInteger #-> PByteString #-> PByteString)
psliceBS = punsafeBuiltin PLC.SliceByteString

-- | Find the length of a 'PByteString'.
plengthBSPPlutus' s => Term s (PByteString #-> PInteger)
plengthBS = punsafeBuiltin PLC.LengthOfByteString

-- | 'PByteString' indexing function.
pindexBSPPlutus' s => Term s (PByteString #-> PInteger #-> PInteger)
pindexBS = punsafeBuiltin PLC.IndexByteString

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
    f c = error $ "InvalidHexDigit " ++ [c]
