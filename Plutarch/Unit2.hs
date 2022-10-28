{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -w -Wno-orphans #-}

module Plutarch.Unit2 (PUnit(..)) where

import Plutarch.Core
-- import Plutarch (Term, pcon, plet)
-- import Plutarch.Bool (PBool (PFalse, PTrue), PEq, POrd, PPartialOrd, (#<), (#<=), (#==))
import Plutarch.Internal.PlutusType2
import Plutarch.Lift2
-- import Plutarch.Lift (
--   DerivePConstantDirect (DerivePConstantDirect),
--   PConstantDecl,
--   PLifted,
--   PUnsafeLiftDecl,
--   pconstant,
--  )
-- import Plutarch.Show (PShow (pshow'))

-- data PUnit s = PUnit

-- instance PUnsafeLiftDecl PUnit where type PLifted PUnit = ()
-- deriving via (DerivePConstantDirect () PUnit) instance PConstantDecl ()

-- instance PlutusType PUnit where
--   type PInner PUnit = PUnit
--   pcon' PUnit = pconstant ()
--   pmatch' x f = plet x \_ -> f PUnit

-- instance PEq PUnit where
--   x #== y = plet x \_ -> plet y \_ -> pcon PTrue

-- instance PPartialOrd PUnit where
--   x #<= y = plet x \_ -> plet y \_ -> pcon PTrue
--   x #< y = plet x \_ -> plet y \_ -> pcon PFalse

-- instance POrd PUnit

-- instance Semigroup (Term s PUnit) where
--   x <> y = plet x \_ -> plet y \_ -> pcon PUnit

-- instance Monoid (Term s PUnit) where
--   mempty = pcon PUnit

-- instance PShow PUnit where
--   pshow' _ x = plet x \_ -> "()"
