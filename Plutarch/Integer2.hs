{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Integer2 (PInteger, PIntegral (..)) where

import Data.Kind
import GHC.Generics (Generic)

import Plutarch.Lam
import Plutarch.Core
import Plutarch.Plutus
import Plutarch.PType
import Plutarch.Num2
import Plutarch.Unsafe2 (punsafeDowncast)

-- | Plutus BuiltinInteger
type    PInteger :: PType
newtype PInteger ef = PInteger (ef /$ PAny)
  deriving 
  stock Generic

  deriving PHasRepr
  via HasPrimitiveRepr PInteger

  deriving PlutusType
  via PInteger `HasInner` PAny

type  PIntegral :: PDSLKind -> PType -> Constraint
class PIntegral edsl a where
  pdiv :: Term edsl (a #-> a #-> a)
  default pdiv 
    :: PIntegral edsl (PInner a) 
    => PLC edsl
    => PUntyped edsl
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => PHoist edsl
    => Term edsl (a #-> a #-> a)
  pdiv = phoistAcyclic do plam \x y -> punsafeDowncast do pdiv # pto x # pto y

  pmod :: Term edsl (a #-> a #-> a)
  default pmod 
    :: PIntegral edsl (PInner a) 
    => PLC edsl
    => PUntyped edsl
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => PHoist edsl
    => Term edsl (a #-> a #-> a)
  pmod = phoistAcyclic do plam \x y -> punsafeDowncast do pmod # pto x # pto y

  pquot :: Term edsl (a #-> a #-> a)
  default pquot 
    :: PIntegral edsl (PInner a) 
    => PLC edsl
    => PUntyped edsl
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => PHoist edsl
    => Term edsl (a #-> a #-> a)
  pquot = phoistAcyclic do plam \x y -> punsafeDowncast do pquot # pto x # pto y

  prem :: Term edsl (a #-> a #-> a)
  default prem 
    :: PIntegral edsl (PInner a) 
    => PLC edsl
    => PUntyped edsl
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => PHoist edsl
    => Term edsl (a #-> a #-> a)
  prem = phoistAcyclic do plam \x y -> punsafeDowncast do prem # pto x # pto y

-- instance PIntegral PInteger where
--   pdiv = punsafeBuiltin PLC.DivideInteger
--   pmod = punsafeBuiltin PLC.ModInteger
--   pquot = punsafeBuiltin PLC.QuotientInteger
--   prem = punsafeBuiltin PLC.RemainderInteger

-- instance PEq PInteger where
--   x #== y = punsafeBuiltin PLC.EqualsInteger # x # y

-- instance PPartialOrd PInteger where
--   x #<= y = punsafeBuiltin PLC.LessThanEqualsInteger # x # y
--   x #< y = punsafeBuiltin PLC.LessThanInteger # x # y

-- instance POrd PInteger

-- instance PNum PInteger where
--   x #+ y = punsafeBuiltin PLC.AddInteger # x # y
--   x #- y = punsafeBuiltin PLC.SubtractInteger # x # y
--   x #* y = punsafeBuiltin PLC.MultiplyInteger # x # y
--   pabs = phoistAcyclic $ plam \x -> pif (x #<= -1) (negate x) x
--   pnegate = phoistAcyclic $ plam \x -> 0 #- x
--   psignum = plam \x ->
--     pif
--       (x #== 0)
--       0
--       $ pif
--         (x #<= 0)
--         (-1)
--         1
--   pfromInteger = pconstant
