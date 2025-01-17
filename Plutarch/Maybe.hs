{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Maybe (
  -- * Type
  PMaybe (..),

  -- * Functions

  -- ** Introduction
  pjust,
  pnothing,

  -- ** Predicates
  pisJust,

  -- ** Eliminators
  pfromJust,
  ptraceIfNothing,
  pfromMaybe,
  pmaybe,
  passertPJust,
  pmapMaybe,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.String (PString, ptraceInfo)
import Plutarch.Internal.Eq (PEq)
import Plutarch.Internal.Lift (
  PLiftable (
    AsHaskell,
    PlutusRepr,
    haskToRepr,
    plutToRepr,
    reprToHask,
    reprToPlut
  ),
  PLiftedClosed,
  getPLiftedClosed,
  mkPLifted,
  mkPLiftedClosed,
  pconstant,
  pliftedFromClosed,
  pliftedToClosed,
 )
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PlutusType,
  pcon,
  pmatch,
 )
import Plutarch.Internal.Term (
  S,
  Term,
  perror,
  phoistAcyclic,
  (#),
  (:-->),
 )
import Plutarch.Repr.SOP (DeriveAsSOPStruct (DeriveAsSOPStruct))

-- | @since WIP
data PMaybe (a :: S -> Type) (s :: S)
  = PJust (Term s a)
  | PNothing
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      SOP.Generic
    , -- | @since WIP
      PEq
    )

-- | @since WIP
deriving via DeriveAsSOPStruct (PMaybe a) instance PlutusType (PMaybe a)

-- | @since WIP
instance PLiftable a => PLiftable (PMaybe a) where
  type AsHaskell (PMaybe a) = Maybe (AsHaskell a)
  type PlutusRepr (PMaybe a) = PLiftedClosed (PMaybe a)
  {-# INLINEABLE haskToRepr #-}
  haskToRepr = \case
    Nothing -> mkPLiftedClosed $ pcon PNothing
    Just x -> mkPLiftedClosed $ pcon $ PJust (pconstant @a x)
  {-# INLINEABLE reprToHask #-}
  reprToHask x = do
    isJust :: Bool <- plutToRepr $ mkPLifted (pisJust # getPLiftedClosed x)
    if isJust
      then do
        vr :: PlutusRepr a <- plutToRepr $ mkPLifted (pfromJust # getPLiftedClosed x)
        vh :: AsHaskell a <- reprToHask @a vr
        pure $ Just vh
      else pure Nothing
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = pliftedFromClosed
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = Right . pliftedToClosed

-- | Extracts the element out of a 'PJust' and throws an error if its argument is 'PNothing'.
pfromJust ::
  forall (a :: S -> Type) (s :: S).
  Term s (PMaybe a :--> a)
pfromJust = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PNothing -> ptraceInfo "pfromJust: found PNothing" perror
    PJust x -> x

{- | Extracts the element out of a 'PJust' and throws a custom error if it's given a 'PNothing'.

@since WIP
-}
ptraceIfNothing ::
  forall (a :: S -> Type) (s :: S).
  -- | The custom error message.
  Term s PString ->
  Term s (PMaybe a) ->
  Term s a
ptraceIfNothing err t = pmatch t $ \case
  PNothing -> ptraceInfo err perror
  PJust x -> x

{- | Yields true if the given 'PMaybe' value is of form @'PJust' _@.

@since WIP
-}
pisJust ::
  forall (a :: S -> Type) (s :: S).
  Term s (PMaybe a :--> PBool)
pisJust = phoistAcyclic $
  plam $ \v' ->
    pmatch v' $ \case
      PJust _ -> pconstant True
      _ -> pconstant False

{- | Extract a 'PMaybe' by providing a default value in case of 'PJust'.

@since WIP
-}
pfromMaybe ::
  forall (a :: S -> Type) (s :: S).
  Term s (a :--> PMaybe a :--> a)
pfromMaybe = phoistAcyclic $
  plam $ \e a -> pmatch a $ \case
    PJust a' -> a'
    PNothing -> e

{- | Construct a 'PJust' value.

@since WIP
-}
pjust ::
  forall (a :: S -> Type) (s :: S).
  Term s (a :--> PMaybe a)
pjust = phoistAcyclic $ plam $ pcon . PJust

{- | Construct a 'PNothing' value.

@since WIP
-}
pnothing ::
  forall (a :: S -> Type) (s :: S).
  Term s (PMaybe a)
pnothing = phoistAcyclic $ pcon PNothing

{- | Given a default value, a function and a 'PMaybe' value, yields the default
value if the 'PMaybe' value is 'PNothing' and applies the function to the
value stored in the 'PJust' otherwise.

@since WIP
-}
pmaybe ::
  forall (b :: S -> Type) (a :: S -> Type) (s :: S).
  Term s (b :--> (a :--> b) :--> PMaybe a :--> b)
pmaybe = phoistAcyclic $
  plam $ \d f -> flip pmatch $ \case
    PJust v -> f # v
    _ -> d

{- | Extract the value stored in a 'PMaybe' container. If there's no value,
throw an error with the given message.

@since WIP
-}
passertPJust ::
  forall (a :: S -> Type) (s :: S).
  Term s (PString :--> PMaybe a :--> a)
passertPJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    PJust v -> v
    _ -> ptraceInfo emsg perror

{- | Map underlying value if `PMaybe` is `PJust`, do nothing if it is `PNothing`

@since WIP
-}
pmapMaybe :: Term s ((a :--> b) :--> PMaybe a :--> PMaybe b)
pmapMaybe = phoistAcyclic $
  plam $ \f mv -> pmatch mv $ \case
    PJust v -> pjust # (f # v)
    PNothing -> pnothing
