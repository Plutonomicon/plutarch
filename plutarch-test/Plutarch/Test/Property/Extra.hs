{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
  Extra properties based on `prop_haskEquiv`
-}
module Plutarch.Test.Property.Extra (
  prop_leftInverse,
  prop_dataRoundTrip,
) where

import Hedgehog (Gen, Property)

import Plutarch.Prelude

import Plutarch.Test.Property.HaskEquiv (
  Equality (OnPEq),
  HaskEquiv,
  LamArgs,
  NP (Nil, (:*)),
  Totality (TotalFun),
  prop_haskEquiv,
 )
import Plutarch.Test.Property.Marshal (Marshal)

{- |
  `l` is a left inverse of `r`

  See https://en.wikipedia.org/wiki/Inverse_function#Left_inverses

  Like `prop_haskEquiv`, you want to call this with @TypeApplications@
  specifying the value of `e`. For example,

  >>> prop_leftInverse
    @'OnPEq
    mapJoin
    mapSplit
    $ mapOf (pairOf integer integer) rational
-}
prop_leftInverse ::
  forall e p p' h.
  ( LamArgs h ~ '[]
  , HaskEquiv e 'TotalFun (h -> h) (p #-> p) '[h]
  ) =>
  ClosedTerm (p' #-> p) ->
  ClosedTerm (p #-> p') ->
  Gen h ->
  Property
prop_leftInverse l r arg =
  prop_haskEquiv @e @( 'TotalFun) (id @h) (plam $ \x -> l #$ r # x) (arg :* Nil)

{- |
  A Plutarch term that is a `PIsData` can be encoded to and decoded back to the
  same value.
-}
prop_dataRoundTrip ::
  forall h p.
  ( LamArgs h ~ '[]
  , Show h
  , Marshal h p
  , PIsData p
  , PEq p
  ) =>
  Gen h ->
  Property
prop_dataRoundTrip =
  prop_leftInverse
    @( 'OnPEq)
    @p
    @(PAsData p)
    @h
    (plam pfromData)
    (plam pdata)
