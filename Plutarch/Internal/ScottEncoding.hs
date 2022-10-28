{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Internal.ScottEncoding (PlutusTypeScott, PScottEncoded (PScottEncoded)) where

import Data.Proxy (Proxy (Proxy))
import Generics.SOP (
  All,
  NP (Nil, (:*)),
  NS (S, Z),
  SListI,
  SListI2,
  SOP (SOP),
  case_SList,
  cpara_SList,
  para_SList,
 )
import Plutarch.Internal (PDelayed, PType, Term, pdelay, pforce, plam', plet, (#), (#->))
import Plutarch.Internal.Generic (PCode, PGeneric, gpfrom, gpto)
import Plutarch.Internal.PlutusType (
  DerivedPInner,
  PInner,
  PlutusType,
  PlutusTypeStrat,
  PlutusTypeStratConstraint,
  derivedPCon,
  derivedPMatch,
  pcon,
  pcon',
  pmatch,
  pmatch',
 )
import Plutarch.Internal.Quantification (PForall (PForall))

data PlutusTypeScott

type ScottFn' :: [PType] -> PType -> PType
type family ScottFn' xs r where
  ScottFn' '[] r = r
  ScottFn' (x ': xs) r = x #-> ScottFn' xs r

type ScottFn :: [PType] -> PType -> PType
type family ScottFn xs r where
  ScottFn '[] r = PDelayed r
  ScottFn xs r = ScottFn' xs r

-- scottList l r = map (flip scottFn r) l
type ScottList :: [[PType]] -> PType -> [PType]
type family ScottList code r where
  ScottList '[] _ = '[]
  ScottList (xs ': xss) r = ScottFn xs r ': ScottList xss r

newtype PScottEncoded a r s = PScottEncoded (Term s (ScottFn (ScottList a r) r))

instance PlutusType (PScottEncoded a r) where
  type PInner (PScottEncoded a r) = ScottFn (ScottList a r) r
  pcon' (PScottEncoded x) = x
  pmatch' x f = f (PScottEncoded x)

newtype PLamL' s b as = PLamL' {unPLamL' :: (NP (Term s) as -> Term s b) -> Term s (ScottFn' as b)}

-- Explicitly variadic `plam`.
plamL' :: SListI as => (NP (Term s) as -> Term s b) -> Term s (ScottFn' as b)
plamL' = unPLamL' $ para_SList (PLamL' \f -> f Nil) (\(PLamL' prev) -> PLamL' \f -> plam' \a -> prev \as -> f (a :* as))

newtype PLamL s b as = PLamL {unPLamL :: (NP (Term s) as -> Term s b) -> Term s (ScottFn as b)}

-- `pdelay`s the 0-arity case.
plamL :: SListI as => (NP (Term s) as -> Term s b) -> Term s (ScottFn as b)
plamL = unPLamL $ case_SList (PLamL \f -> pdelay $ f Nil) (PLamL plamL')

newtype PAppL' s r as = PAppL' {unPAppL'PPlutus' s => Term s (ScottFn' as r) -> NP (Term s) as -> Term s r}

pappL' :: SListI as => Term s (ScottFn' as c) -> NP (Term s) as -> Term s c
pappL' = unPAppL' $ para_SList (PAppL' \f Nil -> f) (\(PAppL' prev) -> PAppL' \f (x :* xs) -> prev (f # x) xs)

newtype PAppL s r as = PAppL {unPAppLPPlutus' s => Term s (ScottFn as r) -> NP (Term s) as -> Term s r}

pappL :: SListI as => Term s (ScottFn as r) -> NP (Term s) as -> Term s r
pappL = unPAppL $ case_SList (PAppL \f Nil -> pforce f) (PAppL pappL')

newtype PLetL s r as = PLetL {unPLetL :: NP (Term s) as -> (NP (Term s) as -> Term s r) -> Term s r}

pletL' :: SListI as => NP (Term s) as -> (NP (Term s) as -> Term s r) -> Term s r
pletL' = unPLetL $ para_SList
  (PLetL \Nil f -> f Nil)
  \(PLetL prev) -> PLetL \(x :* xs) f -> plet x \x' ->
    prev xs (\xs' -> f (x' :* xs'))

pletL :: All SListI as => SOP (Term s) as -> (SOP (Term s) as -> Term s r) -> Term s r
pletL (SOP (Z x)) f = pletL' x \x' -> f (SOP $ Z x')
pletL (SOP (S xs)) f = pletL (SOP xs) \(SOP xs') -> f (SOP $ S xs')

newtype GPCon' s r as = GPCon' {unGPCon' :: NP (Term s) (ScottList as r) -> NS (NP (Term s)) as -> Term s r}

{- |
  `gpcon'`, given a *partial* scott encoding (as a `PLamL`) and a sum choice, applies
  that encoding to the sum choice.

  The partial encoding is any tail of the full scott encoded function, such that
  one of its elements corresponds to the sum choice.
-}
gpcon' :: SListI2 as => NP (Term s) (ScottList as r) -> NS (NP (Term s)) as -> Term s r
gpcon' = unGPCon' $ cpara_SList (Proxy @SListI) (GPCon' \Nil -> \case {}) \(GPCon' prev) -> GPCon' \(arg :* args) -> \case
  Z x -> pappL arg x
  S xs -> prev args xs

-- | Generic version of `pcon'`
gpcon ::
  forall as r s.
  (SListI (ScottList as r), SListI2 as) =>
  SOP (Term s) as ->
  Term s (PScottEncoded as r)
gpcon fields' =
  pletL fields' \(SOP fields) ->
    pcon $ PScottEncoded $ plamL \args -> (gpcon' args fieldsPPlutus' s => Term s r)

newtype GPMatch' s r as = GPMatch' {unGPMatch' :: (SOP (Term s) as -> Term s r) -> NP (Term s) (ScottList as r)}

gpmatch' ::
  forall as r s.
  SListI2 as =>
  (SOP (Term s) as -> Term s r) ->
  NP (Term s) (ScottList as r)
gpmatch' = unGPMatch' $ cpara_SList (Proxy @SListI) (GPMatch' (const Nil)) \(GPMatch' prev) -> GPMatch' \f ->
  plamL (\args -> f (SOP $ Z args)) :* prev (\(SOP x) -> f (SOP (S x)))

gpmatch ::
  forall as r s.
  (SListI (ScottList as r), SListI2 as) =>
  Term s (PScottEncoded as r) ->
  (SOP (Term s) as -> Term s r) ->
  Term s r
gpmatch x' f = pmatch x' \(PScottEncoded x) -> pappL x (gpmatch' f)

class SListI (ScottList (PCode a) r) => SListIScottList a r
instance SListI (ScottList (PCode a) r) => SListIScottList a r

class
  ( forall r. SListIScottList a r
  , SListI2 (PCode a)
  , PGeneric a
  ) =>
  PlutusTypeScottConstraint a
instance
  ( forall r. SListIScottList a r
  , SListI2 (PCode a)
  , PGeneric a
  ) =>
  PlutusTypeScottConstraint a

instance PlutusTypeStrat PlutusTypeScott where
  type PlutusTypeStratConstraint PlutusTypeScott = PlutusTypeScottConstraint
  type DerivedPInner PlutusTypeScott a = PForall (PScottEncoded (PCode a))
  derivedPCon x = pcon $ PForall $ gpcon $ gpfrom x
  derivedPMatch x' f = pmatch x' \(PForall x) -> gpmatch x (f . gpto)
