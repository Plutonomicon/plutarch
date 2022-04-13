{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Integer (
  PInteger,
  PIntegral (..),
) where

import Plutarch.Bool (PEq, POrd, pif, (#<), (#<=), (#==))
import Plutarch.Internal.Other (
  DerivePNewtype,
  Term,
  phoistAcyclic,
  plam,
  plet,
  pto,
  (#),
  type (:-->),
 )
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Unsafe (punsafeBuiltin, punsafeFrom)
import qualified PlutusCore as PLC

-- | Plutus BuiltinInteger
data PInteger s

instance PUnsafeLiftDecl PInteger where type PLifted PInteger = Integer
deriving via (DerivePConstantDirect Integer PInteger) instance PConstantDecl Integer

class PIntegral a where
  pdiv :: Term s (a :--> a :--> a)
  pmod :: Term s (a :--> a :--> a)
  pquot :: Term s (a :--> a :--> a)
  prem :: Term s (a :--> a :--> a)

instance PIntegral PInteger where
  pdiv = punsafeBuiltin PLC.DivideInteger
  pmod = punsafeBuiltin PLC.ModInteger
  pquot = punsafeBuiltin PLC.QuotientInteger
  prem = punsafeBuiltin PLC.RemainderInteger

instance PIntegral b => PIntegral (DerivePNewtype a b) where
  pdiv = phoistAcyclic $ plam $ \x y -> punsafeFrom $ pdiv # pto x # pto y
  pmod = phoistAcyclic $ plam $ \x y -> punsafeFrom $ pmod # pto x # pto y
  pquot = phoistAcyclic $ plam $ \x y -> punsafeFrom $ pquot # pto x # pto y
  prem = phoistAcyclic $ plam $ \x y -> punsafeFrom $ prem # pto x # pto y

instance PEq PInteger where
  x #== y = punsafeBuiltin PLC.EqualsInteger # x # y

instance POrd PInteger where
  x #<= y = punsafeBuiltin PLC.LessThanEqualsInteger # x # y
  x #< y = punsafeBuiltin PLC.LessThanInteger # x # y

instance Num (Term s PInteger) where
  x + y = punsafeBuiltin PLC.AddInteger # x # y
  x - y = punsafeBuiltin PLC.SubtractInteger # x # y
  x * y = punsafeBuiltin PLC.MultiplyInteger # x # y
  abs x' = plet x' $ \x -> pif (x #<= -1) (negate x) x
  negate x = 0 - x
  signum x' = plet x' $ \x ->
    pif
      (x #== 0)
      0
      $ pif
        (x #<= 0)
        (-1)
        1
  fromInteger = pconstant
