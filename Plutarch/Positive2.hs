{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Positive2 where

-- import Data.Functor.Const (Const)
import GHC.Generics (Generic)
import Data.Kind (Type, Constraint)
import Plutarch.Core
import Plutarch.PType
import Plutarch.Lam
import Plutarch.Plutus
import Plutarch.Maybe2
import Plutarch.Num2
import Plutarch.Bool2
import Plutarch.Integer2

-- import Plutarch.Bool (PEq, POrd, PPartialOrd, pif, (#<=))
-- import Plutarch.Builtin (PAsData, PData, PIsData, pdata)
-- import Plutarch.Integer (PInteger, PIntegral)

-- import Plutarch.Maybe (PMaybe (PJust, PNothing))

-- import Plutarch (
--   DerivePlutusType (DPTStrat),
--   PlutusType,
--   PlutusTypeNewtype,
--   Term,
--   TermCont (runTermCont),
--   pcon,
--   phoistAcyclic,
--   plam,
--   plet,
--   pthrow,
--   pto,
--   (#),
--   (#$),
--   type (#->),
--  )
-- import Plutarch.Num (PNum (pfromInteger, (#-)))
-- import Plutarch.Show (PShow)
-- import Plutarch.TermCont (tcont)
-- import Plutarch.Trace (ptraceError)
-- import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'), ptryFrom)

type    PPositive :: PType
newtype PPositive ef = PPositive (ef /$ PInteger)
  deriving 
  stock Generic

  deriving PHasRepr
  via HasPrimitiveRepr PPositive

  deriving PlutusType
  via PInteger `HasInner` PInteger

--   deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PIntegral, PShow)
-- instance DerivePlutusType PPositive where type DPTStrat _ = PlutusTypeNewtype

-- instance PNum PPositive where
--   x #- y = ptryPositive #$ pto x #- pto y

--   pfromInteger x
--     | x <= 0 = pthrow "PPositive.pfromInteger: encountered non positive"
--     | otherwise = pcon $ PPositive $ pfromInteger x

-- instance PTryFrom PInteger PPositive where
--   type PTryFromExcess PInteger PPositive = Const ()
--   ptryFrom' opq = runTermCont $ pure (ptryPositive # opq, ())

-- newtype Flip f a b = Flip (f b a) deriving stock (Generic)

-- instance PTryFrom PData (PAsData PPositive) where
--   type PTryFromExcess PData (PAsData PPositive) = Flip Term PPositive
--   ptryFrom' opq = runTermCont $ do
--     (_, i) <- tcont $ ptryFrom @(PAsData PInteger) opq
--     res <- tcont . plet $ ptryPositive # i
--     resData <- tcont . plet $ pdata res
--     pure (resData, res)

-- -- | Build a 'PPositive' from a 'PInteger'. Yields 'PNothing' if argument is zero.
ppositive 
  :: forall edsl. ()
  => IsPType edsl (PMaybe PPositive)
  => IsPType edsl PInteger
  => PConstructable edsl (PMaybe PPositive)
  => PConstructable edsl PBool
  => PConstructable edsl PPositive
  => PHoist edsl
  => PLC edsl
  => PNum edsl PInteger
  => PPartialOrd edsl PInteger
  => Term edsl (PInteger #-> PMaybe PPositive)
ppositive = phoistAcyclic (plam body) where


  body :: Term edsl PInteger -> Term edsl (PMaybe PPositive)
  body i = pif (i #<= 0) 
    do pcon PNothing
    do pcon do PJust do pcon (PPositive i)

-- | Partial version of 'PPositive'. Errors if argument is zero.
ptryPositive 
  :: forall edsl. ()
  => IsPType edsl PInteger
  => IsPType edsl PPositive
  => PHoist edsl
  => PLC edsl
  => Term edsl (PInteger #-> PPositive)
ptryPositive = phoistAcyclic (plam body) where

  body :: Term edsl PInteger -> Term edsl PPositive
  body i = pif (i #<= 0)
    do ptraceError "ptryPositive: building with non positive"
    do pcon (PPositive i)
