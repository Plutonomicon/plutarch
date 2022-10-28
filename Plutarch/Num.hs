{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Num (PNum (..)) where

import Plutarch.Internal (
  PType,
  Term,
  punsafeCoerce,
  (#),
  (#->),
 )
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Unsafe (punsafeDowncast)

class PNum (a :: PType) where
  (#+)PPlutus' s => Term s a -> Term s a -> Term s a
  default (#+) :: PNum (PInner a) => Term s a -> Term s a -> Term s a
  x #+ y = punsafeDowncast $ pto x #+ pto y

  (#-)PPlutus' s => Term s a -> Term s a -> Term s a
  default (#-) :: PNum (PInner a) => Term s a -> Term s a -> Term s a
  x #- y = punsafeDowncast $ pto x #- pto y

  (#*)PPlutus' s => Term s a -> Term s a -> Term s a
  default (#*) :: PNum (PInner a) => Term s a -> Term s a -> Term s a
  x #* y = punsafeDowncast $ pto x #* pto y

  pnegatePPlutus' s => Term s (a #-> a)
  default pnegate :: PNum (PInner a) => Term s (a #-> a)
  pnegate = punsafeCoerce (pnegatePPlutus' s => Term s (PInner a #-> PInner a))

  pabsPPlutus' s => Term s (a #-> a)
  default pabs :: PNum (PInner a) => Term s (a #-> a)
  pabs = punsafeCoerce (pabsPPlutus' s => Term s (PInner a #-> PInner a))

  psignumPPlutus' s => Term s (a #-> a)
  default psignum :: PNum (PInner a) => Term s (a #-> a)
  psignum = punsafeCoerce (psignumPPlutus' s => Term s (PInner a #-> PInner a))

  pfromInteger :: Integer -> Term s a
  default pfromInteger :: PNum (PInner a) => Integer -> Term s a
  pfromInteger x = punsafeDowncast $ pfromInteger x

-- orphan instance, but only visibly orphan when importing internal modules
instance PNum a => Num (Term s a) where
  (+) = (#+)
  (-) = (#-)
  (*) = (#*)
  abs x = pabs # x
  negate x = pnegate # x
  signum x = psignum # x
  fromInteger = pfromInteger
