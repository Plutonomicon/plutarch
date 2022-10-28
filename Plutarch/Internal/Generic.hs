{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- Common generics-sop utilities for use in Plutarch.
-}
module Plutarch.Internal.Generic (
  -- * Plutarch adapters for generics-sop API
  PGeneric,
  PGeneric',
  PCode,
  gpfrom,
  gpto,
) where

-- lol
import Data.Constraint (Dict (Dict))
import Data.Kind (Constraint)
import GHC.Exts (Any)
import GHC.Generics (Generic)
import Generics.SOP (All2, I, SOP, Top)
import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, GTo, gfrom, gto)
import Plutarch.Internal (PType, S, Term)
import Plutarch.Internal.TypeFamily (ToPType2)
import Unsafe.Coerce (unsafeCoerce)

class GFrom a => GFrom' a
instance GFrom a => GFrom' a

class GTo a => GTo' a
instance GTo a => GTo' a

type PGeneric' :: PType -> S -> Constraint
class
  ( Generic (a s)
  , GFrom (a s)
  , GTo (a s)
  , All2 Top (PCode a) -- DO NOT REMOVE! Will cause unsound behavior otherwise. See `unsafeCoerce` below.
  , All2 Top (GCode (a s))
  , GDatatypeInfo (a s)
  ) =>
  PGeneric' a s
instance
  ( Generic (a s)
  , GFrom (a s)
  , GTo (a s)
  , All2 Top (PCode a) -- DO NOT REMOVE! Will cause unsound behavior otherwise. See `unsafeCoerce` below.
  , All2 Top (GCode (a s))
  , GDatatypeInfo (a s)
  ) =>
  PGeneric' a s

-- | `Generic` constraint extended to work with Plutarch types.
type PGeneric :: PType -> Constraint
class (forall s. PGeneric' a s) => PGeneric a

instance (forall s. PGeneric' a s) => PGeneric a

type PCode :: PType -> [[PType]]

-- | Like `Code` but for Plutarch types
type PCode a = ToPType2 (GCode (a Any))

gpfrom :: forall a s. PGeneric a => a s -> SOP (Term s) (PCode a)
-- This could be done safely, but it's a PITA.
-- Depends on `All` constraint above.
gpfrom x = case (Dict :: Dict (PGeneric' a s)) of
  Dict -> unsafeCoerce (gfrom x :: SOP I (GCode (a s)))

gpto :: forall a s. PGeneric a => SOP (Term s) (PCode a) -> a s
-- This could be done safely, but it's a PITA.
-- Depends on `All` constraint above.
gpto x = case (Dict :: Dict (PGeneric' a s)) of
  Dict -> gto (unsafeCoerce x :: SOP I (GCode (a s)))

