{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Plutarch.Maybe (PMaybe (..)) where

import Plutarch (
  PDelayed,
  PInner,
  PType,
  PlutusType,
  S,
  Term,
  pcon',
  pdelay,
  pforce,
  plam,
  pmatch',
  (#),
  type (:-->),
 )

-- | Plutus Maybe type, with Scott-encoded repr
data PMaybe (a :: PType) (s :: S) = PJust (Term s a) | PNothing

instance PlutusType (PMaybe a) where
  type PInner (PMaybe a) b = (a :--> b) :--> PDelayed b :--> b
  pcon' :: forall s. PMaybe a s -> forall b. Term s (PInner (PMaybe a) b)
  pcon' (PJust x) = plam $ \f _ -> f # x
  pcon' PNothing = plam $ \_ g -> pforce g
  pmatch' x f = x # plam (f . PJust) # pdelay (f PNothing)
