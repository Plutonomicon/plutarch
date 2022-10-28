{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Num2 (PNum (..)) where

import Data.Kind
import Plutarch.Core
import Plutarch.Unsafe2 (punsafeDowncast)

type  PNum :: PDSLKind -> PType -> Constraint
class PNum edsl a where
  (#+) :: Term edsl a -> Term edsl a -> Term edsl a
  (#-) :: Term edsl a -> Term edsl a -> Term edsl a
  (#*) :: Term edsl a -> Term edsl a -> Term edsl a
  pnegate :: Term edsl (a #-> a)
  pabs    :: Term edsl (a #-> a)
  psignum :: Term edsl (a #-> a)
  pfromInteger :: Integer -> Term edsl a

  default (#+) 
    :: PUntyped edsl
    => PNum edsl (PInner a)
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => Term edsl a -> Term edsl a -> Term edsl a
  x #+ y = punsafeDowncast (pto x #+ pto y)

  default (#-) 
    :: PUntyped edsl
    => PNum edsl (PInner a)
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => Term edsl a -> Term edsl a -> Term edsl a
  x #- y = punsafeDowncast $ pto x #- pto y

  default (#*) 
    :: PUntyped edsl
    => PNum edsl (PInner a)
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => Term edsl a -> Term edsl a -> Term edsl a
  x #* y = punsafeDowncast $ pto x #* pto y

  default pnegate
    :: PUntyped edsl
    => PNum edsl (PInner a)
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => PLC edsl 
    => Term edsl (a #-> a)
  pnegate = punsafeCoerce (pnegate @edsl @(PInner a))

  default pabs
    :: PUntyped edsl
    => PNum edsl (PInner a)
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => PLC edsl 
    => Term edsl (a #-> a)
  pabs = punsafeCoerce (pabs @edsl @(PInner a))

  default psignum
    :: PUntyped edsl
    => PNum edsl (PInner a)
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => PLC edsl 
    => Term edsl (a #-> a)
  psignum = punsafeCoerce (pabs @edsl @(PInner a))

  default pfromInteger
    :: PUntyped edsl
    => PNum edsl (PInner a)
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => Integer -> Term edsl a
  pfromInteger = punsafeCoerce . pfromInteger @edsl @(PInner a)

-- orphan instance, but only visibly orphan when importing internal modules
instance (PNum edsl a, PLC edsl, IsPType edsl a) => Num (Term edsl a) where
  (+), (-), (*) :: Term edsl a -> Term edsl a -> Term edsl a
  (+) = (#+)
  (-) = (#-)
  (*) = (#*)

  abs, negate, signum :: Term edsl a -> Term edsl a
  abs x = pabs # x
  negate x = pnegate # x
  signum x = psignum # x

  fromInteger :: Integer -> Term edsl a
  fromInteger = pfromInteger
