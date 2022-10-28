module Plutarch.Unsafe2 where

import Plutarch.Core

{- |
  Unsafely coerce from the 'PInner' representation of a Term,
  assuming that the value is a safe construction of the Term.
-}
punsafeDowncast 
  :: PUntyped edsl
  => IsPType edsl a
  => IsPType edsl (PInner a)
  => Term edsl (PInner a) -> Term edsl a
punsafeDowncast = punsafeCoerce
