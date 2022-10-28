{-# Options_GHC -w #-}

module Plutarch.Pair2 where

import GHC.Generics (Generic)
-- import Plutarch.Bool (PEq)
import Plutarch.Core
import Plutarch.PType
-- import Plutarch.Internal (PType, S, Term)
-- import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PlutusType)
import Plutarch.Internal.PlutusType2
-- import Plutarch.Internal.ScottEncoding (PlutusTypeScott)
-- import Plutarch.Show (PShow)

-- {- |
--   Plutus encoding of Pairs.

--   Note: This is represented differently than 'BuiltinPair'. It is scott-encoded.
-- -}
type PPair :: PType -> PType -> PType
data PPair (a :: PType) (b :: PType) pl 
  = PPair (pl /$ a) (pl /$ b)
  deriving 
  stock Generic

  deriving 
  anyclass (PlutusType) -- , PEq, PShow)

-- instance DerivePlutusType (PPair a b) where type DPTStrat _ = PlutusTypeScott
