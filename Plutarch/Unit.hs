{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Unit (PUnit (..)) where

import Plutarch (Term, pcon, plet)
import Plutarch.Bool (PBool (PFalse, PTrue), PEq, POrd, PPartialOrd, (#<), (#<=), (#==))
import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon', pmatch')
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Show (PShow (pshow'))

instance PUnsafeLiftDecl PUnit where type PLifted PUnit = ()
deriving via (DerivePConstantDirect () PUnit) instance PConstantDecl ()

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

-- instance Semigroup (Term edsl PUnit) where
--   x <> y = plet x \_ -> plet y \_ -> pcon PUnit

-- instance Monoid (Term s PUnit) where
--   mempty = pcon PUnit

-- instance PShow PUnit where
--   pshow' _ x = plet x \_ -> "()"
