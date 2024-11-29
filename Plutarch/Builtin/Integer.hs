{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Builtin.Integer (
  -- * Type
  PInteger,

  -- * Functions
  pexpModInteger,
) where

import GHC.Generics (Generic)

-- import Plutarch.Internal.Lift (DeriveBuiltinPLiftable, PLiftable, PLifted (PLifted))
-- import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Term (S, Term, punsafeBuiltin, (#), (:-->))
import PlutusCore qualified as PLC

{- | A builtin Plutus integer.

@since WIP
-}
newtype PInteger s = PInteger (Term s POpaque)
  deriving stock (Generic)

{- | Performs modulo exponentiation. More precisely, @pexpModInteger b e m@
performs @b@ to the power of @e@, modulo @m@. The result is always
non-negative.

= Note

This will error if the modulus is zero. When given a negative exponent, this
will try to find a modular multiplicative inverse, and will error if none
exists.

@since WIP
-}
pexpModInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
pexpModInteger = punsafeBuiltin PLC.ExpModInteger