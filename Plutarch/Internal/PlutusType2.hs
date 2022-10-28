{-# Options_GHC -w #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Internal.PlutusType2 (PlutusType(..)) where
  -- PlutusType,
--   -- PlutusTypeStratConstraint,
--   -- PCon,
--   -- PMatch,
--   -- pcon',
--   -- pmatch',
--   -- pmatch,
--   -- pcon,
--   -- PInner,
--   -- PlutusTypeStrat,
--   -- DerivePlutusType,
--   -- DPTStrat,
--   -- DerivedPInner,
--   -- derivedPCon,
--   -- derivedPMatch,
--   -- PVariant,
--   -- PCovariant,
--   -- PContravariant,
--   -- PVariant',
--   -- PCovariant',
--   -- PContravariant',

import Data.Kind (Constraint, Type)
-- -- import Data.Proxy (Proxy (Proxy))
-- import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
-- import Generics.SOP (All2)
import Plutarch.PType
import Plutarch.Core
import Plutarch.Internal2
-- -- import Plutarch.Internal (PType, Term, plam', plet, punsafeCoerce, (#->) (PLam))
-- import Plutarch.Internal.Generic (PCode)
-- -- import Plutarch.Internal.PLam ((#))
-- -- import Plutarch.Internal.Quantification (PFix (PFix), PForall (PForall), PSome (PSome)
-- -- import Plutarch.Internal.Witness (witness)

type  PlutusTypeStrat :: Type -> Constraint
class PlutusTypeStrat strategy where
--   type PlutusTypeStratConstraint strategy :: PType -> Constraint
--   type DerivedPInner strategy (a :: PType) :: PType
-- --   derivedPCon :: forall a s. (DerivePlutusType a, DPTStrat a ~ strategy) => a s -> Term s (DerivedPInner strategy a)
-- --   derivedPMatch :: forall a s b. (DerivePlutusType a, DPTStrat a ~ strategy) => Term s (DerivedPInner strategy a) -> (a s -> Term s b) -> Term s b

-- class
--   ( PInner a ~ DerivedPInner (DPTStrat a) a
--   , PlutusTypeStrat (DPTStrat a)
--   , PlutusTypeStratConstraint (DPTStrat a) a
--   , PlutusType a
--   ) =>
--   DerivePlutusType (a :: PType)
--   where
--   type DPTStrat a :: Type
--   type DPTStrat a = TypeError ( 'Text "Please specify a strategy for deriving PlutusType for type " ':<>: 'ShowType a)


--   type PCovariant' a :: Constraint
--   type PCovariant' a = All2 PCovariant'' (PCode a)
--   type PContravariant' a :: Constraint
--   type PContravariant' a = All2 PContravariant'' (PCode a)
--   type PVariant' a :: Constraint
--   type PVariant' a = All2 PVariant'' (PCode a)
  -- pcon' :: pl ef -> ef /$ PInner pl

-- --   default pcon' :: DerivePlutusType a => forall s. a s -> Term s (PInner a)
-- --   pcon' = let _ = witness (Proxy @(PlutusType a)) in derivedPCon

-- --   pmatch' :: forall s b. Term s (PInner a) -> (a s -> Term s b) -> Term s b
-- --   -- FIXME buggy GHC, needs AllowAmbiguousTypes
-- --   default pmatch' :: DerivePlutusType a => forall s b. Term s (PInner a) -> (a s -> Term s b) -> Term s b
-- --   pmatch' = derivedPMatch

-- -- {-# DEPRECATED PCon "Use PlutusType" #-}
-- -- type PCon = PlutusType
-- -- {-# DEPRECATED PMatch "Use PlutusType" #-}
-- -- type PMatch = PlutusType

-- -- -- | Construct a Plutarch Term via a Haskell datatype
-- -- pcon :: PlutusType a => a s -> Term s a
-- -- pcon x = punsafeCoerce (pcon' x)

-- -- -- | Pattern match over Plutarch Terms via a Haskell datatype
-- -- pmatch :: PlutusType a => Term s a -> (a s -> Term s b) -> Term s b
-- -- pmatch x = pmatch' (punsafeCoerce x)

-- class PCovariant' a => PCovariant'' a
-- instance PCovariant' a => PCovariant'' a

-- class PContravariant' a => PContravariant'' a
-- instance PContravariant' a => PContravariant'' a

-- class PVariant' a => PVariant'' a
-- instance PVariant' a => PVariant'' a

-- class (forall t. PCovariant'' t => PCovariant'' (a t)) => PCovariant a
-- instance (forall t. PCovariant'' t => PCovariant'' (a t)) => PCovariant a

-- class (forall t. PCovariant'' t => PContravariant'' (a t)) => PContravariant a
-- instance (forall t. PCovariant'' t => PContravariant'' (a t)) => PContravariant a

-- class (forall t. PVariant'' t => PVariant'' (a t)) => PVariant a
-- instance (forall t. PVariant'' t => PVariant'' (a t)) => PVariant a

-- instance PlutusType (a #-> b) where
--   type PInner (a #-> b) = a #-> b
-- --   type PCovariant' (a #-> b) = (PContravariant' a, PCovariant' b)
-- --   type PContravariant' (a #-> b) = (PCovariant' a, PContravariant' b)
-- --   type PVariant' (a #-> b) = (PVariant' a, PVariant' b)
  -- pcon' :: forall ef. (a #-> b) ef -> ef /$ (a #-> b)
  -- pcon' (PLam f) = undefined where

  --   u = f :: ef /$ a -> ef /$ b

-- plam' :: (Term s a -> Term s b) -> Term s (a #-> b)

-- --   pmatch' f g = plet f \f' -> g (PLam (f' #))

-- instance PlutusType (PFix f) where
--   type PInner (PFix f) = f (PFix f)

  -- pcon' :: PFix f ef -> ef /$ f (PFix f)
  -- pcon' (PFix x) = x
-- --   pmatch' x f = f (PFix x)

-- pconFun :: (a #-> b) s -> Term s (a #-> b)
-- pconFun = undefined

-- pconForall :: (PForall f) s -> Term s (PForall f)
-- pconForall = undefined

-- pconPSome :: (PSome f) s -> Term s (PSome f)
-- pconPSome = undefined

-- pconPFix :: PFix f ef -> ef /$ f (PFix f)
-- pconPFix (PFix as) = as

-- pconnn :: (Pf ef a1 ~ Term edsl a2, Pf ef b1 ~ Term edsl b2, PConstructable' edsl (a2 #-> b2)) => (#->) a1 b1 ef -> Term edsl (a2 #-> b2)
-- pconnn (PLam f) = plam f

-- pconn :: PFix f ef -> ef /$ f (PFix f)
-- pconn (PFix x) = x
