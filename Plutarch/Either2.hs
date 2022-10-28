{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

{-# Options_GHC -w #-}

module Plutarch.Either2 where

import Data.Kind 
import GHC.Generics (Generic)

import Plutarch.Core hiding (PEither(..))
import Plutarch.Core qualified as Core
import Plutarch.Lam
import Plutarch.PType

type  PEitherC :: PDSLKind -> (PType -> PType -> PType) -> Constraint
class PEitherC edsl pEither where
  pLeft 
    :: Term edsl a 
    -> Term edsl (pEither a b)
  pRight 
    :: Term edsl b 
    -> Term edsl (pEither a b)

  pEither 
    :: IsPType edsl a
    => IsPType edsl b
    => IsPType edsl res
    => Term edsl (a #-> res) 
    -> Term edsl (b #-> res)
    -> Term edsl (pEither a b #-> res)

instance (PLC edsl, forall a b. PConstructable' edsl (Core.PEither a b)) => PEitherC edsl Core.PEither where
  pLeft :: Term edsl a -> Term edsl (Core.PEither a b)
  pLeft = pcon . Core.PLeft

  pRight :: Term edsl b -> Term edsl (Core.PEither a b)
  pRight = pcon . Core.PRight

  pEither 
    :: forall a b (res :: PType). ()
    => IsPType edsl a
    => IsPType edsl b
    => IsPType edsl res
    => Term edsl (a #-> res) 
    -> Term edsl (b #-> res)
    -> Term edsl (Core.PEither a b #-> res)
  pEither pLeft pRight = plam body where

    body :: Term edsl (Core.PEither a b) -> Term edsl res
    body either = pmatch either \case
      Core.PLeft  a -> pLeft  # a
      Core.PRight b -> pRight # b

-- type PEither :: PType -> PType -> PType 
-- data PEither a b ef
--   = PLeft (Term ef a)
--   | PRight (Term s b)
--   deriving 
--   stock Generic

--   deriving PHasRepr
--   via WithPrimitiveRepr (PEither a b)
  -- deriving anyclass (PlutusType, PEq, PShow)
-- instance DerivePlutusType (PEither a b) where type DPTStrat _ = PlutusTypeScott
