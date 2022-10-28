{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Positive (PPositive, ppositive, ptryPositive) where

import Data.Functor.Const (Const)
import Data.Text (pack)
import GHC.Generics (Generic)

import Plutarch.Bool (PEq, POrd, PPartialOrd, pif, (#<=))
import Plutarch.Builtin (PAsData, PData, PIsData, pdata)
import Plutarch.Integer (PInteger, PIntegral)

import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Show (pshow)

import Plutarch (
  DerivePlutusType (DPTStrat),
  PlutusType,
  PlutusTypeNewtype,
  Term,
  TermCont (runTermCont),
  pcon,
  phoistAcyclic,
  plam,
  plet,
  pthrow,
  pto,
  (#),
  (#$),
  type (#->),
 )
import Plutarch.Num (PNum (pfromInteger, (#-)))
import Plutarch.Show (PShow)
import Plutarch.TermCont (tcont)
import Plutarch.Trace (ptraceError)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'), ptryFrom)

newtype PPositive s = PPositive (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PIntegral, PShow)
instance DerivePlutusType PPositive where type DPTStrat _ = PlutusTypeNewtype

instance PNum PPositive where
  x #- y = ptryPositive #$ pto x #- pto y

  pfromInteger x
    | x <= 0 =
        pthrow $
          "PPositive.pfromInteger: encountered non positive: " <> pack (show x)
    | otherwise = pcon $ PPositive $ pfromInteger x

instance PTryFrom PInteger PPositive where
  type PTryFromExcess PInteger PPositive = Const ()
  ptryFrom' opq = runTermCont $ pure (ptryPositive # opq, ())

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

instance PTryFrom PData (PAsData PPositive) where
  type PTryFromExcess PData (PAsData PPositive) = Flip Term PPositive
  ptryFrom' opq = runTermCont $ do
    (_, i) <- tcont $ ptryFrom @(PAsData PInteger) opq
    res <- tcont . plet $ ptryPositive # i
    resData <- tcont . plet $ pdata res
    pure (resData, res)

-- | Build a 'PPositive' from a 'PInteger'. Yields 'PNothing' if argument is zero.
ppositivePPlutus' s => Term s (PInteger #-> PMaybe PPositive)
ppositive = phoistAcyclic $
  plam $ \i ->
    pif
      (i #<= 0)
      (pcon PNothing)
      $ pcon . PJust . pcon $ PPositive i

-- | Partial version of 'PPositive'. Errors if argument is zero.
ptryPositivePPlutus' s => Term s (PInteger #-> PPositive)
ptryPositive = phoistAcyclic $
  plam $ \i ->
    pif
      (i #<= 0)
      (ptraceError $ "ptryPositive: building with non positive: " <> pshow i)
      $ pcon $ PPositive i
