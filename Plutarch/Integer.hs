{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Integer (PInteger, PIntegral (..)) where

import GHC.Generics (Generic)
import Plutarch.Bool (PEq, POrd, PPartialOrd, pif, (#<), (#<=), (#==))
import Plutarch.Internal (
  Term,
  phoistAcyclic,
  (#),
  (#->),
 )
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Other (POpaque, pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PInner, PlutusType)
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Num (PNum, pabs, pfromInteger, pnegate, psignum, (#*), (#+), (#-))
import Plutarch.Unsafe (punsafeBuiltin, punsafeDowncast)
import qualified PlutusCore as PLC

-- | Plutus BuiltinInteger
data PInteger s = PInteger (Term s POpaque)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PInteger where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PInteger where type PLifted PInteger = Integer
deriving via (DerivePConstantDirect Integer PInteger) instance PConstantDecl Integer

class PIntegral a where
  pdivPPlutus' s => Term s (a #-> a #-> a)
  default pdiv :: PIntegral (PInner a) => Term s (a #-> a #-> a)
  pdiv = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ pdiv # pto x # pto y
  pmodPPlutus' s => Term s (a #-> a #-> a)
  default pmod :: PIntegral (PInner a) => Term s (a #-> a #-> a)
  pmod = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ pmod # pto x # pto y
  pquotPPlutus' s => Term s (a #-> a #-> a)
  default pquot :: PIntegral (PInner a) => Term s (a #-> a #-> a)
  pquot = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ pquot # pto x # pto y
  premPPlutus' s => Term s (a #-> a #-> a)
  default prem :: PIntegral (PInner a) => Term s (a #-> a #-> a)
  prem = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ prem # pto x # pto y

instance PIntegral PInteger where
  pdiv = punsafeBuiltin PLC.DivideInteger
  pmod = punsafeBuiltin PLC.ModInteger
  pquot = punsafeBuiltin PLC.QuotientInteger
  prem = punsafeBuiltin PLC.RemainderInteger

instance PEq PInteger where
  x #== y = punsafeBuiltin PLC.EqualsInteger # x # y

instance PPartialOrd PInteger where
  x #<= y = punsafeBuiltin PLC.LessThanEqualsInteger # x # y
  x #< y = punsafeBuiltin PLC.LessThanInteger # x # y

instance POrd PInteger

instance PNum PInteger where
  x #+ y = punsafeBuiltin PLC.AddInteger # x # y
  x #- y = punsafeBuiltin PLC.SubtractInteger # x # y
  x #* y = punsafeBuiltin PLC.MultiplyInteger # x # y
  pabs = phoistAcyclic $ plam \x -> pif (x #<= -1) (negate x) x
  pnegate = phoistAcyclic $ plam \x -> 0 #- x
  psignum = plam \x ->
    pif
      (x #== 0)
      0
      $ pif
        (x #<= 0)
        (-1)
        1
  pfromInteger = pconstant
