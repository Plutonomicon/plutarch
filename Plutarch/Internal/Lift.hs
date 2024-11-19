{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- Because of the weird way the PlutusType derivation mechanisms work, we lose
-- the PlutusType constraint. Kind of annoying, but we can't convince GHC
-- otherwise.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Internal.Lift (
  -- * Type class
  PLiftable (..),

  -- * Functions
  pconstant,
  plift,

  -- * Derivation

  -- ** Via-helpers
  DeriveBuiltinPLiftable (..),
  DeriveDataPLiftable (..),
  DeriveNewtypePLiftable (..),

  -- ** Manual instance helpers
  LiftError (..),
) where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import {-# SOURCE #-} Plutarch.Builtin (PData)
import Plutarch.Internal (
  Config (Tracing),
  LogLevel (LogInfo),
  S,
  Term,
  TracingMode (DoTracing),
  compile,
  punsafeConstantInternal,
 )
import Plutarch.Internal.Evaluate (EvalError, evalScriptHuge)
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PlutusType)
import Plutarch.Script (Script (Script))
import Plutarch.TryFrom (PSubtype)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore qualified as PLC
import PlutusCore.Builtin (BuiltinError, readKnownConstant)
import PlutusTx qualified as PTx
import Universe (Includes)
import UntypedPlutusCore qualified as UPLC

{- | Used with 'fromPlutarch' methods to give additional information about why
evaluating a Plutarch term into a Haskell value went wrong.

@since WIP
-}
data LiftError
  = -- | Evaluation failed for some reason.
    CouldNotEvaluate EvalError
  | -- | We tried to use a builtin not part of the Plutus universe.
    TypeError BuiltinError
  | -- | Compiling the term into a script failed.
    CouldNotCompile Text
  | -- | @Data@ encoding was invalid for our type
    CouldNotDecodeData
  deriving stock
    ( -- | @since WIP
      Eq
    , -- | @since WIP
      Show
    )

{- | Indicates that the given Plutarch type has an equivalent in Haskell (and
Plutus by extension), and we have the ability to move between them.

= Important note

Calling 'toPlutarch' and 'fromPlutarch' directly should rarely, if ever, be a
thing you do. Prefer using 'pconstant' and 'plift', as these handle some of
the oddities required without you having to think about them.

You should rarely, if ever, need to define 'PLiftable' instances by hand.
Whenever possible, prefer using 'DeriveBuiltinPLiftable' and
'DeriveDataPLiftable', as they have fewer complexities and caveats. See their
documentation for when to use them.

If you do want to define the methods yourself, there's a few key factors to
keep in mind:

1. 'PLifted' is a trivial wrapper to assist with @via@ derivations: it is
   isomorphic to 'Identity' and should be treated as such. Use 'unPLifted' to
   \'pull\' terms out.
2. 'fromPlutarch' involves compilation and evaluation. When doing so, ensure
   that you use the largest possible budgets to avoid weird problems.
3. When choosing a type for 'AsHaskell', /any/ value of that type /must/ be
   representable in Plutarch. If you have internal invariants to maintain on
   the Haskell side, make sure you do so with great care.

= Laws

1. @'fromPlutarch' '.' 'toPlutarch'@ @=@ @'Right'@
2. @'fmap' 'toPlutarch' '.' 'fromPlutarch'@ @=@ @'Right'@

Any derivations via 'DeriveBuiltinPLiftable' and 'DeriveDataPLiftable'
automatically follow these laws.

@since WIP
-}
class (PlutusType a, PLC.DefaultUni `Includes` PlutusRepr a) => PLiftable (a :: S -> Type) where
  type AsHaskell a :: Type
  type PlutusRepr a :: Type
  toPlutarch :: AsHaskell a -> PlutusRepr a
  fromPlutarch :: PlutusRepr a -> Either LiftError (AsHaskell a)

{- | Given a Haskell-level representation of a Plutarch term, transform it into
its equivalent term.

@since WIP
-}
pconstant ::
  forall (a :: S -> Type) (s :: S).
  PLiftable a =>
  AsHaskell a ->
  Term s a
pconstant = punsafeCoerce . punsafeConstantInternal . PLC.someValue . toPlutarch @a

{- | Given a closed Plutarch term, compile and evaluate it, then produce the
corresponding Haskell value. If compilation or evaluation fails somehow, this
will call 'error': if you need to \'trap\' these outcomes and handle them
differently somehow, use 'fromPlutarch'.

@since WIP
-}
plift ::
  forall (a :: S -> Type).
  PLiftable a =>
  (forall (s :: S). Term s a) ->
  AsHaskell a
plift t = case plift' t of
  Left err ->
    error $
      "plift failed: "
        <> ( case err of
              CouldNotEvaluate evalErr -> "term errored: " <> show evalErr
              TypeError builtinError -> "incorrect type: " <> show builtinError
              CouldNotCompile compErr -> "could not compile: " <> Text.unpack compErr
              CouldNotDecodeData -> "Data value is not a valid encoding for this type"
           )
  Right res -> res

plift' ::
  forall (a :: S -> Type).
  PLiftable a =>
  (forall (s :: S). Term s a) ->
  Either LiftError (AsHaskell a)
plift' t =
  case compile (Tracing LogInfo DoTracing) t of
    Left err -> Left . CouldNotCompile $ err
    Right compiled -> case evalScriptHuge compiled of
      (evaluated, _, _) -> case evaluated of
        Left err -> Left . CouldNotEvaluate $ err
        Right (Script (UPLC.Program _ _ term)) -> case readKnownConstant term of
          Left err -> Left . TypeError $ err
          Right res -> fromPlutarch @a res

{- | @via@-deriving helper, indicating that @a@ has a Haskell-level equivalent
@h@ that is directly part of the Plutus default universe (instead of by way
of a @Data@ encoding).

@since WIP
-}
newtype DeriveBuiltinPLiftable (a :: S -> Type) (h :: Type) (s :: S)
  = DeriveBuiltinPLiftable (a s)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

-- | @since WIP
instance DerivePlutusType (DeriveBuiltinPLiftable a h) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance
  ( PlutusType a
  , PLC.DefaultUni `Includes` h
  ) =>
  PLiftable (DeriveBuiltinPLiftable a h)
  where
  type AsHaskell (DeriveBuiltinPLiftable a h) = h
  type PlutusRepr (DeriveBuiltinPLiftable a h) = h

  {-# INLINEABLE toPlutarch #-}
  toPlutarch = id

  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch = Right

{- | @via@-deriving helper, indicating that @a@ has a Haskell-level equivalent
@h@ by way of its @Data@ encoding, rather than by @h@ being directly part of
the Plutus default universe.

@since WIP
-}
newtype DeriveDataPLiftable (a :: S -> Type) (h :: Type) (s :: S)
  = DeriveDataPLiftable (a s)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

-- | @since WIP
instance DerivePlutusType (DeriveDataPLiftable a h) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance
  ( PlutusType a
  , PSubtype PData a
  , PTx.ToData h
  , PTx.FromData h
  ) =>
  PLiftable (DeriveDataPLiftable a h)
  where
  type AsHaskell (DeriveDataPLiftable a h) = h
  type PlutusRepr (DeriveDataPLiftable a h) = PTx.Data
  {-# INLINEABLE toPlutarch #-}
  toPlutarch = PTx.toData
  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch t = do
    case PTx.fromData t of
      Nothing -> Left CouldNotDecodeData
      Just res' -> pure res'

-- | @since WIP
newtype DeriveNewtypePLiftable (wrapper :: S -> Type) (inner :: S -> Type) (h :: Type) (s :: S)
  = DeriveNewtypePLiftable (wrapper s)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

-- | @since WIP
instance DerivePlutusType (DeriveNewtypePLiftable w i h) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance (PLiftable inner, Coercible (AsHaskell inner) h) => PLiftable (DeriveNewtypePLiftable wrapper inner h) where
  type AsHaskell (DeriveNewtypePLiftable wrapper inner h) = h
  type PlutusRepr (DeriveNewtypePLiftable wrapper inner h) = PlutusRepr inner

  {-# INLINEABLE toPlutarch #-}
  toPlutarch = toPlutarch @inner . coerce @h @(AsHaskell inner)

  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch = coerce . fromPlutarch @inner
