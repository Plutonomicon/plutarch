-- This prevents errors on 'instance IntegralDomain PInteger PNatural'
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Extra.Natural (
  PNatural (..),
  pscaleNat,
  ppowNat,
  pnatToInt,
  pnatFromInt,
) where

import Plutarch (PlutusType (pcon', pmatch'))
import Plutarch.Extra.Numeric (
  PAdditiveGroup ((#-)),
  PAdditiveHemigroup ((#^-)),
  PAdditiveMonoid (pzero),
  PAdditiveSemigroup ((#+)),
  PEuclideanClosed (pdivMod, peuclideanDiv),
  PIntegralDomain (pabs, paddExtend, pprojectAbs, prestrictMay),
  PMultiplicativeMonoid (pone),
  PMultiplicativeSemigroup ((#*)),
  peven,
  pnegate,
 )
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeFrom)

newtype PNatural (s :: S) = PNatural (Term s PInteger)
  deriving (PIsData, PEq, POrd, PIntegral) via (DerivePNewtype PNatural PInteger)

instance PlutusType PNatural where
  type PInner PNatural _ = PInteger
  pcon' (PNatural n) = f # n
    where
      f = phoistAcyclic $
        plam $ \n ->
          pif
            (n #< 0)
            (ptraceError "pcon(PNatural): negative integer")
            n
  pmatch' p f = f $ PNatural p

instance PAdditiveSemigroup PNatural where
  (#+) x y =
    phoistAcyclic
      ( plam $ \x' y' ->
          pcon . PNatural $ pnatToInt x' #+ pnatToInt y'
      )
      # x
      # y

instance PAdditiveMonoid PNatural where
  pzero = pcon $ PNatural pzero

instance PMultiplicativeSemigroup PNatural where
  (#*) x y =
    phoistAcyclic
      ( plam $ \x' y' ->
          pcon . PNatural $ pnatToInt x' #* pnatToInt y'
      )
      # x
      # y

instance PMultiplicativeMonoid PNatural where
  pone = pcon $ PNatural pone

instance PAdditiveHemigroup PNatural where
  x #^- y =
    phoistAcyclic
      ( plam $ \n1 n2 ->
          plet (pnatToInt n1) $ \n1' ->
            plet (pnatToInt n2) $ \n2' ->
              pcon . PNatural $
                pif (n1' #<= n2') pzero (n1' #- n2')
      )
      # x
      # y

instance PEuclideanClosed PNatural where
  pdivMod =
    phoistAcyclic $
      ( plam $ \x y ->
          pmatch (pdivMod # (pnatToInt x) # (pnatToInt y)) $ \(PPair d r) ->
            pcon $
              PPair
                (pcon $ PNatural d)
                (pcon $ PNatural r)
      )

-- | Scale by a 'Natural' multiplier.
pscaleNat ::
  forall s a.
  PAdditiveMonoid a =>
  Term s PNatural ->
  Term s a ->
  Term s a
pscaleNat n a =
  phoistAcyclic
    ( pfix
        #$ plam
        $ \self nat x ->
          pif
            (nat #== pzero)
            pzero
            (x #+ (self # (nat #^- pone) # x))
    )
    # n
    # a

instance PIntegralDomain PInteger PNatural where
  pprojectAbs = pcon . PNatural . pabs
  paddExtend = pnatToInt
  prestrictMay = pnatFromInt
  pabs x' =
    phoistAcyclic
      ( plam $ \x ->
          pif (x #< pzero) (pnegate x) x
      )
      # x'

-- | Raise by a 'PNatural' power.
ppowNat :: PMultiplicativeMonoid a => Term s a -> Term s PNatural -> Term s a
ppowNat a nat =
  phoistAcyclic
    ( plam $ \x n ->
        pif
          (n #== pzero)
          pone
          (pmatch n $ \(PNatural i) -> pexpBySquaring # x # i)
    )
    # a
    # nat

pnatToInt :: Term s PNatural -> Term s PInteger
pnatToInt nat = phoistAcyclic (plam $ \x -> pmatch x $ \(PNatural n) -> n) # nat

pnatFromInt :: Term s PInteger -> Term s (PMaybe PNatural)
pnatFromInt x =
  phoistAcyclic
    ( plam $ \i ->
        pif
          (i #< 0)
          (pcon PNothing)
          (pcon . PJust . (\x -> punsafeFrom x) $ i)
    )
    # x

-- We secretly know that i is always positive.
pexpBySquaring ::
  forall s a.
  PMultiplicativeMonoid a =>
  Term s (a :--> PInteger :--> a)
pexpBySquaring = phoistAcyclic $
  pfix #$ plam $ \self acc i ->
    pif (i #== pone) acc $
      plet (self # (acc #* acc) # (peuclideanDiv # i # 2)) $ \x ->
        pif (peven # i) x (acc #* x)
