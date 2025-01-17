{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Builtin.Integer (
  -- * Type
  PInteger,

  -- * Functions
  pexpModInteger,
  peqInteger,
  pleInteger,
  pltInteger,
  paddInteger,
  psubtractInteger,
  pmultiplyInteger,
  pconstantInteger,
  pquotientInteger,
  premainderInteger,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Term (S, Term, punsafeBuiltin, punsafeConstantInternal, (:-->))
import PlutusCore qualified as PLC

{- | A builtin Plutus integer.

@since WIP
-}
newtype PInteger s = PInteger (Term s POpaque)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)

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

peqInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
peqInteger = punsafeBuiltin PLC.EqualsInteger

pleInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
pleInteger = punsafeBuiltin PLC.LessThanEqualsInteger

pltInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
pltInteger = punsafeBuiltin PLC.LessThanInteger

paddInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
paddInteger = punsafeBuiltin PLC.AddInteger

psubtractInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
psubtractInteger = punsafeBuiltin PLC.SubtractInteger

pmultiplyInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pmultiplyInteger = punsafeBuiltin PLC.MultiplyInteger

pconstantInteger :: forall (s :: S). Integer -> Term s PInteger
pconstantInteger = punsafeConstantInternal . PLC.someValue

pquotientInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pquotientInteger = punsafeBuiltin PLC.QuotientInteger

premainderInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
premainderInteger = punsafeBuiltin PLC.RemainderInteger
