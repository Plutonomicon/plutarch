module Plutarch.Unsafe (
  PI.punsafeBuiltin,
  PI.punsafeCoerce,
  PI.punsafeConstant,
  punsafeDowncast,
) where

import Plutarch.Internal (Term)
import qualified Plutarch.Internal as PI
import Plutarch.Internal.PlutusType (PInner)

{- |
  Unsafely coerce from the 'PInner' representation of a Term,
  assuming that the value is a safe construction of the Term.
-}
punsafeDowncastPPlutus' s => Term s (PInner a) -> Term s a
punsafeDowncast x = PI.punsafeCoerce x
