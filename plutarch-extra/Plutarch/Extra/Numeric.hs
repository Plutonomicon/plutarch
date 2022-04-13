{-# LANGUAGE FunctionalDependencies #-}

module Plutarch.Extra.Numeric (
  -- * Type classes
  PAdditiveSemigroup (..),
  PAdditiveMonoid (..),
  PAdditiveGroup (..),
  PMultiplicativeSemigroup (..),
  PMultiplicativeMonoid (..),
  PAdditiveHemigroup (..),
  PEuclideanClosed (..),
  PMultiplicativeGroup (..),
  PIntegralDomain (..),

  -- * Helper types
  PField,
  PHemifield,
  PHemiring,

  -- * Helper functions
  pnegate,
  peven,
  pmonus,
  (#^),
) where

import Plutarch.Prelude

{- | A Plutrch level 'Semigroup' that it is sensible to describe
 using addition.
-}
class PAdditiveSemigroup a where
  (#+) :: Term s a -> Term s a -> Term s a

infixl 6 #+

{- | A Plutarch level 'Monoid' that it is sensible to describe
 using addition and zero.
-}
class PAdditiveSemigroup a => PAdditiveMonoid a where
  pzero :: Term s a

{- | A Plutarch level 'Group' that it is sensible to describe
 using addition, pzero, and subtraction.
-}
class PAdditiveMonoid a => PAdditiveGroup a where
  (#-) :: Term s a -> Term s a -> Term s a

infixl 6 #-

pnegate :: PAdditiveGroup a => Term s a -> Term s a
pnegate x = pzero #- x

{- | A Plutarch level 'Semigroup' that it is sensible to describe
 using multiplication.
-}
class PMultiplicativeSemigroup a where
  (#*) :: Term s a -> Term s a -> Term s a

infixl 7 #*

{- | A Plutarch level 'Semigroup' that it is sensible to describe
 using multiplication and one.
-}
class PMultiplicativeSemigroup a => PMultiplicativeMonoid a where
  pone :: Term s a

-- | A Plutarch level semiring.
type PSemiring a = (PAdditiveMonoid a, PMultiplicativeMonoid a)

-- | A Plutarch level ring.
type PRing a = (PAdditiveGroup a, PMultiplicativeMonoid a)

-- | An 'PAdditiveMonoid' with a notion of Plutarch level monus.
class PAdditiveMonoid a => PAdditiveHemigroup a where
  (#^-) :: Term s a -> Term s a -> Term s a

infixl 6 #^-

type PHemiring a = (PAdditiveHemigroup a, PMultiplicativeMonoid a)

{- | A 'PSemiring' with a notion of (kind of) Euclidean division. This differs from
 the mathematical notion of this, as we do not exclude zero.
-}
class (POrd a, PSemiring a) => PEuclideanClosed a where
  pdivMod :: Term s (a :--> a :--> PPair a a)

  -- | Gets only the division part of a 'pdivMod'.
  peuclideanDiv :: Term s (a :--> a :--> a)
  peuclideanDiv =
    phoistAcyclic $
      plam $ \x y -> pmatch (pdivMod # x # y) $ \(PPair d _) -> d

  -- | Gets only the remainder part of a 'divMod'.
  peuclideanRem :: Term s (a :--> a :--> a)
  peuclideanRem =
    phoistAcyclic $
      plam $ \x y -> pmatch (pdivMod # x # y) $ \(PPair _ r) -> r

{- | A 'PMultiplicativeMonoid' with a notion of multiplicative inverse (for
 non-zero values).

 We have to exclude division by zero, as it leads to paradoxical situations.
 This does mean that '#/' and 'preciprocal' aren't total, but there's not much
 we can do about that.
-}
class PMultiplicativeMonoid a => PMultiplicativeGroup a where
  {-# MINIMAL (#/) | preciprocal #-}
  (#/) :: Term s a -> Term s a -> Term s a
  x #/ y = x #* preciprocal y
  preciprocal :: Term s a -> Term s a
  preciprocal x = pone #/ x

type PField a = (PAdditiveGroup a, PMultiplicativeGroup a)

type PHemifield a = (PAdditiveHemigroup a, PMultiplicativeGroup a)

{- | A 'PRing' with a notion of \'sign\' or \'direction\' separate from magnitude.

 We extend the notion of integral domain with the idea of an \'additive
 restriction\', which is a type representing \'strictly positive\' values. For
 example, the \'additive restriction\' of 'PInteger' is 'PNatural', and the
 \'additive restricton\' of 'PRational' is 'PNatRatio'. This gives us the
 ability to move between these two types while preserving magnitude.
-}
class (PEq a, POrd a, PRing a) => PIntegralDomain a r | a -> r, r -> a where
  pabs :: Term s a -> Term s a
  pprojectAbs :: Term s a -> Term s r
  paddExtend :: Term s r -> Term s a
  prestrictMay :: Term s a -> Term s (PMaybe r)
  prestrictMay x =
    phoistAcyclic
      ( plam $ \x ->
          pif
            (pabs x #== x)
            (pcon . PJust $ pprojectAbs x)
            (pcon PNothing)
      )
      # x
  psignum :: Term s a -> Term s a
  psignum x' =
    phoistAcyclic
      ( plam $ \x ->
          pif (x #== pzero) pzero $
            pif (x #< pzero) (pnegate pone) (pone)
      )
      # x'

pmonus :: PAdditiveHemigroup a => Term s a -> Term s a -> Term s a
pmonus = (#^-)

peven :: (PEq a, PEuclideanClosed a) => Term s (a :--> PBool)
peven =
  phoistAcyclic $
    ( plam $ \x ->
        peuclideanRem # x # (pone #+ pone) #== pzero
    )

-- | Raise by an 'Integer' power.
(#^) :: PMultiplicativeGroup a => Term s a -> Term s PInteger -> Term s a
a #^ x = f # a # x
  where
    f =
      phoistAcyclic
        ( plam $ \x i ->
            plet i $ \i' ->
              plet x $ \x' ->
                pif (i' #== pzero) pone $
                  pif (i' #== pone) x' $
                    plet (pexpBySquaring # x') $ \sqX ->
                      pif
                        (i' #< pzero)
                        (preciprocal $ sqX #$ pnegate i')
                        (sqX # i')
        )

infixr 8 #^

-- Helpers

-- We secretly know that i is always positive.
pexpBySquaring ::
  forall s a.
  PMultiplicativeMonoid a =>
  Term s (a :--> PInteger :--> a)
pexpBySquaring = pfix #$ plam f
  where
    f :: Term s (a :--> PInteger :--> a) -> Term s a -> Term s PInteger -> Term s a
    f self acc i =
      pif (i #== pone) acc $
        plet (self # (acc #* acc) # (peuclideanDiv # i # 2)) $ \x ->
          pif (peven # i) x (acc #* x)

-- Instances for existing core types.

instance PAdditiveSemigroup PInteger where
  (#+) = (+)

instance PAdditiveMonoid PInteger where
  pzero = 0

instance PAdditiveGroup PInteger where
  (#-) = (-)

instance PMultiplicativeSemigroup PInteger where
  (#*) = (*)

instance PMultiplicativeMonoid PInteger where
  pone = 1

instance PEuclideanClosed PInteger where
  pdivMod =
    phoistAcyclic
      ( plam $ \x y ->
          pif
            (y #== pzero)
            (pcon $ PPair pzero x)
            ( pcon $
                PPair
                  (pquot # x # y)
                  (prem # x # y)
            )
      )
  peuclideanDiv = pquot
  peuclideanRem = prem

instance PAdditiveSemigroup PRational where
  (#+) = (+)

instance PAdditiveMonoid PRational where
  pzero = phoistAcyclic (pcon $ PRational pzero pone)

instance PAdditiveGroup PRational where
  (#-) = (-)

instance PMultiplicativeSemigroup PRational where
  (#*) = (*)

instance PMultiplicativeMonoid PRational where
  pone = phoistAcyclic (pcon $ PRational pone pone)

instance PMultiplicativeGroup PRational where
  preciprocal = recip
