module Plutarch.Maybe (
  PMaybe (PJust, PNothing),
  pfromJust,
) where

import GHC.Generics (Generic)
import Plutarch (
  DPTStrat,
  DerivePlutusType,
  PType,
  PlutusType,
  PlutusTypeScott,
  S,
  Term,
  perror,
  phoistAcyclic,
  plam,
  pmatch,
  type (#->),
 )
import Plutarch.Bool (PEq)
import Plutarch.Show (PShow)

-- | Plutus Maybe type, with Scott-encoded repr
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType (PMaybe a) where type DPTStrat _ = PlutusTypeScott

{- |
 fallible unwrapping from @PMaybe@
-}
pfromJustPPlutus' s => Term s (PMaybe a #-> a)
pfromJust = phoistAcyclic $
  plam $ \maybe -> pmatch maybe $ \case
    PNothing -> perror
    PJust a -> a
