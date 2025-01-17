{- Justification: PlutusType is superclass requirement of PLiftable
-}

module Plutarch.Internal.Lift (PlutusRepr, getPLifted, unsafeHaskToUni) where

import Data.Kind (Type)
import Plutarch.Internal.Term (S, Term)
import PlutusCore qualified as PLC
import Universe (Includes)

type role PLifted nominal nominal
data PLifted (s :: S) (a :: S -> Type)

type family PlutusRepr (a :: S -> Type)

getPLifted :: forall (s :: S) (a :: S -> Type). PLifted s a -> Term s a
unsafeHaskToUni ::
  forall (h :: Type) (a :: S -> Type) (s :: S).
  PLC.DefaultUni `Includes` h =>
  h ->
  PLifted s a
