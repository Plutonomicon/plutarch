{-# Language UndecidableInstances #-}

module Plutarch.Maybe2 
  ( PMaybe (PJust, PNothing)
  , pfromJust
  )
  where

import Data.Kind (Type, Constraint)
import Plutarch.PType
import Plutarch.Generics
import Plutarch.Core
import Plutarch.Plutus
import Plutarch.Lam
import Plutarch.Bool2
import GHC.Generics (Generic)

-- | Plutus Maybe type, with Scott-encoded repr
type PMaybe :: PType -> PType
data PMaybe a ef
  = PNothing
  | PJust (ef /$ a)
  deriving 
  stock Generic

  deriving PHasRepr
  via HasSOPRepr (PMaybe a)

  deriving PlutusType
  via HasSameInner (PMaybe a)

{- |
 fallible unwrapping from @PMaybe@
-}
pfromJust 
  :: forall edsl a. ()
  => IsPType edsl a
  => IsPType edsl (PMaybe a)
  => PConstructable edsl (PMaybe a)
  => PHoist edsl 
  => PLC edsl 
  => PPartial edsl
  => Term edsl (PMaybe a #-> a)
pfromJust = phoistAcyclic (plam body) where

  body :: Term edsl (PMaybe a) -> Term edsl a
  body maybe = pmatch maybe \case
    PNothing -> perror 
    PJust a  -> a
