module Plutarch.Pair (PPair (..)) where

import GHC.Generics (Generic)
import Plutarch.Bool (PEq ((#==)), POrd, PPartialOrd ((#<), (#<=)), pif)
import Plutarch.Internal (PType, S, Term)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PlutusType, pmatch)
import Plutarch.Internal.ScottEncoding (PlutusTypeScott)
import Plutarch.Show (PShow)
import Plutarch.TermCont (tcont, unTermCont)

{- |
  Plutus encoding of Pairs.
  Note: This is represented differently than 'BuiltinPair'. It is scott-encoded.
-}
data PPair (a :: PType) (b :: PType) (s :: S)
  = PPair (Term s a) (Term s b)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType (PPair a b) where type DPTStrat _ = PlutusTypeScott

instance (PPartialOrd a, PPartialOrd b) => PPartialOrd (PPair a b) where
  a #<= b = unTermCont $ do
    PPair a1 a2 <- tcont $ pmatch a
    PPair b1 b2 <- tcont $ pmatch b
    pure $
      pif
        (a1 #== b1)
        (a2 #<= b2)
        (a1 #<= b1)

  a #< b = unTermCont $ do
    PPair a1 a2 <- tcont $ pmatch a
    PPair b1 b2 <- tcont $ pmatch b
    pure $
      pif
        (a1 #== b1)
        (a2 #< b2)
        (a1 #< b1)

instance (POrd a, POrd b) => POrd (PPair a b)
