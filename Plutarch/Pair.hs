{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Pair (PPair (..)) where

import Plutarch (
  PInner,
  PType,
  PlutusType,
  S,
  Term,
  pcon',
  plam,
  pmatch',
  (#),
  (#$),
  type (:-->),
 )

{- |
  Plutus encoding of Pairs.

  Note: This is represented differently than 'BuiltinPair'
-}
data PPair (a :: PType) (b :: PType) (s :: S) = PPair (Term s a) (Term s b)

instance PlutusType (PPair a b) where
  type PInner (PPair a b) c = (a :--> b :--> c) :--> c
  pcon' (PPair x y) = plam $ \f -> f # x # y
  pmatch' p f = p #$ plam $ \x y -> f (PPair x y)
