{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- Because of the weird way the PlutusType derivation mechanisms work, we lose
-- the PlutusType constraint. Kind of annoying, but we can't convince GHC
-- otherwise.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Internal.Lift (
  -- * Type class
  PLiftable (..),

  -- * Error type
  LiftError (..),

  -- * Functions
  pconstant,
  plift,

  -- * Derivation

  -- ** Via-helpers
  DeriveBuiltinPLiftable (..),
  DeriveDataPLiftable (..),
  DeriveNewtypePLiftable (..),

  -- ** Manual instance helpers
  unsafeHaskToUni,
  reprToPlutUni,
  plutToReprUni,
  PLifted (PLifted),
  mkPLifted,
  getPLifted,
  PLiftedClosed (..),
  getPLiftedClosed,
  mkPLiftedClosed,
  pliftedToClosed,
  pliftedFromClosed,
  punsafeCoercePLifted,
) where

import Data.Bits (toIntegralSized)
import Data.ByteString (ByteString)
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)
import GHC.Generics (Generic)
import Plutarch.Builtin.BLS (
  PBuiltinBLS12_381_G1_Element,
  PBuiltinBLS12_381_G2_Element,
  PBuiltinBLS12_381_MlResult,
 )
import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.ByteString (PByte, PByteString)
import Plutarch.Builtin.Data (
  PAsData,
  PBuiltinList,
  PBuiltinPair,
  PData,
 )
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.Opaque (POpaque, popaque)
import Plutarch.Builtin.String (PString)
import Plutarch.Builtin.Unit (PUnit)
import Plutarch.Internal.Evaluate (EvalError, evalScriptHuge)
import {-# SOURCE #-} Plutarch.Internal.IsData (PIsData)
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PlutusType (PInner))
import Plutarch.Internal.Subtype (PSubtype)
import Plutarch.Internal.Term (
  Config (Tracing),
  LogLevel (LogInfo),
  S,
  Term,
  TracingMode (DoTracing),
  compile,
  punsafeConstantInternal,
 )
import Plutarch.Script (Script (Script))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore qualified as PLC
import PlutusCore.Builtin (BuiltinError, readKnownConstant)
import PlutusCore.Crypto.BLS12_381.G1 qualified as BLS12_381.G1
import PlutusCore.Crypto.BLS12_381.G2 qualified as BLS12_381.G2
import PlutusCore.Crypto.BLS12_381.Pairing qualified as BLS12_381.Pairing
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
  | -- | @Data@ encoding was invalid for our type.
    CouldNotDecodeData
  | -- | Something else went wrong.
    OtherLiftError Text
  deriving stock
    ( -- | @since WIP
      Eq
    , -- | @since WIP
      Show
    )

{- | Indicates that the given Plutarch type has an equivalent in Haskell (and
Plutus by extension), and we have the ability to move between them.

= Important note

Calling methods of 'PLiftable' directly should rarely, if ever, be a
thing you do, unless defining your own instances without @via@-deriving
helpers (below). Prefer using 'pconstant' and 'plift', as these handle
some of the oddities required without you having to think about them.

You should rarely, if ever, need to define 'PLiftable' instances by hand.
Whenever possible, prefer using 'DeriveBuiltinPLiftable',
'DeriveDataPLiftable', and `DeriveNewtypePLiftable` as they have fewer
complexities and caveats. See their documentation for when to use them.

If you do want to define the methods yourself, there's a few key factors to
keep in mind:

1. You still shouldn't write every method by hand, there are helpers
   @plutToReprUni@ and @reprToPlutUni@ to cover common cases.
2. If defining 'plutToRepr' and 'reprToPlut' for Scott encoded types you need to
   set @'PlutusRepr' PMyType = 'PLiftedClosed' PMyType@
3. When choosing a type for 'AsHaskell', /any/ value of that type /must/ be
   representable in Plutarch. If you have internal invariants to maintain on
   the Haskell side, make sure you do so with great care.

= Laws

1. @'reprToHask' '.' 'haskToRepr'@ @=@ @'Right'@
2. @'plutToRepr' '.' 'reprToPlut'@ @=@ @'Right'@

Any derivations via 'DeriveBuiltinPLiftable', 'DeriveDataPLiftable', and
'DeriveNewtypePLiftable' automatically follow these laws.

Together, these imply @plift . pconstant = id@.

@since WIP
-}
class PlutusType a => PLiftable (a :: S -> Type) where
  type AsHaskell a :: Type

  -- Implementation note: we need this second repr type because builtin
  -- containers like 'PBuiltinList' and 'PBuiltinPair' are not actually
  -- polymorphic. They can only hold types that are in 'DefaultUni'.
  -- Thus to convert e.g. a list to Plutarch, we first need to convert
  -- list elements to something that is in the Plutus universe before it
  -- can be processed further.
  type PlutusRepr a :: Type

  -- | Transform @a@'s Haskell equivalent to its Plutus universe
  -- representation.
  haskToRepr :: AsHaskell a -> PlutusRepr a

  -- | Given @a@'s Plutus universe representation, turn it back into its (true)
  -- Haskell equivalent if possible.
  reprToHask :: PlutusRepr a -> Either LiftError (AsHaskell a)

  -- | Given @a@'s Plutus universe representation, lift it into Plutarch.
  reprToPlut :: forall (s :: S). PlutusRepr a -> PLifted s a

  -- | Given a closed Plutarch term, evaluate it back into its Plutus universe
  -- representation, or fail.
  plutToRepr :: (forall (s :: S). PLifted s a) -> Either LiftError (PlutusRepr a)

{- | Valid definition of 'reprToPlut' if @PlutusRepr a@ is in the Plutus universe.

@since WIP
-}
reprToPlutUni ::
  forall (a :: S -> Type) (s :: S).
  (PLiftable a, PLC.DefaultUni `Includes` PlutusRepr a) =>
  PlutusRepr a ->
  PLifted s a
reprToPlutUni = unsafeHaskToUni

{- | Helper that bypasses 'PlutusRepr' and lifts the Haskell equivalent
directly. This is unsafe: we cannot verify (in general) that @h@ can be
represented sensibly as an @a@, so use this with care.

@since WIP
-}
unsafeHaskToUni ::
  forall (h :: Type) (a :: S -> Type) (s :: S).
  PLC.DefaultUni `Includes` h =>
  h ->
  PLifted s a
unsafeHaskToUni x =
  PLifted $ popaque $ punsafeCoerce $ punsafeConstantInternal $ PLC.someValue x

{- | Valid definition of 'plutToRepr' if @PlutusRepr a@ is in the Plutus
universe.

@since WIP
-}
plutToReprUni ::
  forall (a :: S -> Type).
  (PLiftable a, PLC.DefaultUni `Includes` PlutusRepr a) =>
  (forall (s :: S). PLifted s a) ->
  Either LiftError (PlutusRepr a)
plutToReprUni t = case compile (Tracing LogInfo DoTracing) $ unPLifted t of
  Left err -> Left . CouldNotCompile $ err
  Right compiled -> case evalScriptHuge compiled of
    (evaluated, _, _) -> case evaluated of
      Left err -> Left . CouldNotEvaluate $ err
      Right (Script (UPLC.Program _ _ term)) ->
        case readKnownConstant term of
          Left err -> Left . TypeError $ err
          Right res -> Right res

{- | Given a Haskell-level representation of a Plutarch term, transform it into
its equivalent term.

@since WIP
-}
pconstant ::
  forall (a :: S -> Type) (s :: S).
  PLiftable a =>
  AsHaskell a ->
  Term s a
pconstant = getPLifted . reprToPlut . haskToRepr @a

{- | Given a closed Plutarch term, compile and evaluate it, then produce the
corresponding Haskell value. If compilation or evaluation fails somehow, this
will call 'error': if you need to \'trap\' these outcomes and handle them
differently somehow, use 'reprToPlut' and 'reprToHask' manually.

@since WIP
-}
plift ::
  forall (a :: S -> Type).
  PLiftable a =>
  (forall (s :: S). Term s a) ->
  AsHaskell a
plift t = case plutToRepr @a $ mkPLifted t of
  Left err ->
    error $
      "plift failed: "
        <> ( case err of
              CouldNotEvaluate evalErr -> "term errored: " <> show evalErr
              TypeError builtinError -> "incorrect type: " <> show builtinError
              CouldNotCompile compErr -> "could not compile: " <> Text.unpack compErr
              CouldNotDecodeData -> "Data value is not a valid encoding for this type"
              OtherLiftError err -> "other error: " <> Text.unpack err
           )
  Right res -> case reprToHask @a res of
    Left _ ->
      -- FIXME
      error $
        "plift failed: "
          <> "Plutus representation does not correspond to a Haskell value"
    Right res' -> res'

{- | @via@-deriving helper, indicating that @a@ has a Haskell-level equivalent
@h@ that is directly part of the Plutus default universe (instead of by way
of an encoding).

@since WIP
-}
newtype DeriveBuiltinPLiftable (a :: S -> Type) (h :: Type) (s :: S)
  = DeriveBuiltinPLiftable (a s)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    )

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
  {-# INLINEABLE haskToRepr #-}
  haskToRepr = id
  {-# INLINEABLE reprToHask #-}
  reprToHask = Right
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

{- | @via@-deriving helper, indicating that @a@ has a Haskell-level equivalent
@h@ by way of its @Data@ encoding, rather than by @h@ being directly part of
the Plutus default universe.

@since WIP
-}
newtype DeriveDataPLiftable (a :: S -> Type) (h :: Type) (s :: S)
  = DeriveDataPLiftable (a s)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    )

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
  {-# INLINEABLE haskToRepr #-}
  haskToRepr = PTx.toData
  {-# INLINEABLE reprToHask #-}
  reprToHask = maybe (Left CouldNotDecodeData) Right . PTx.fromData
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

{- | @via@-deriving helper, indicating that @wrapper@ has a Haskell-level equivalent
@h@ by way @PInner wrapper@, up to coercibility.

@since WIP
-}
newtype DeriveNewtypePLiftable (wrapper :: S -> Type) (h :: Type) (s :: S)
  = DeriveNewtypePLiftable (wrapper s)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    )

-- | @since WIP
instance DerivePlutusType (DeriveNewtypePLiftable w h) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance
  ( PLiftable (PInner wrapper)
  , Coercible h (AsHaskell (PInner wrapper))
  , PLC.DefaultUni `Includes` PlutusRepr (PInner wrapper)
  ) =>
  PLiftable (DeriveNewtypePLiftable wrapper h)
  where
  type AsHaskell (DeriveNewtypePLiftable wrapper h) = h
  type PlutusRepr (DeriveNewtypePLiftable wrapper h) = PlutusRepr (PInner wrapper)
  {-# INLINEABLE haskToRepr #-}
  haskToRepr = haskToRepr @(PInner wrapper) . coerce
  {-# INLINEABLE reprToHask #-}
  reprToHask = coerce . reprToHask @(PInner wrapper)
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = punsafeCoercePLifted . reprToPlutUni @(PInner wrapper)
  {-# INLINEABLE plutToRepr #-}
  plutToRepr t = plutToReprUni @(PInner wrapper) $ punsafeCoercePLifted t

{- | Similar to 'Identity', but at the level of Plutarch. Only needed when
writing manual instances of 'PLiftable', or if you want to use 'reprToPlut'
and 'plutToRepr' directly.

This is used for coercing Plutarch terms at Haskell level with
`coerce :: PLifted s a -> PLifted s b` for @via@-deriving helpers.

@since WIP
-}
newtype PLifted (s :: S) (a :: S -> Type) = PLifted {unPLifted :: Term s POpaque}

type role PLifted nominal nominal

-- | @since WIP
punsafeCoercePLifted :: PLifted s a -> PLifted s b
punsafeCoercePLifted (PLifted t) = PLifted t

-- | @since WIP
getPLifted :: PLifted s a -> Term s a
getPLifted (PLifted t) = punsafeCoerce t

-- | @since WIP
mkPLifted :: Term s a -> PLifted s a
mkPLifted t = PLifted (popaque t)

{- |  Use this as 'PlutusRepr' when defining 'PLiftable' instances for Scott encoded types.

@since WIP
-}
newtype PLiftedClosed (a :: S -> Type) = PLiftedClosed
  { unPLiftedClosed :: forall (s :: S). Term s POpaque
  }

-- | @since WIP
getPLiftedClosed ::
  forall (a :: S -> Type).
  PLiftedClosed a ->
  (forall (s :: S). Term s a)
getPLiftedClosed (PLiftedClosed x) = punsafeCoerce x

-- | @since WIP
mkPLiftedClosed ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  PLiftedClosed a
mkPLiftedClosed t = PLiftedClosed $ popaque t

-- | @since WIP
pliftedToClosed ::
  forall (a :: S -> Type).
  (forall (s :: S). PLifted s a) ->
  PLiftedClosed a
pliftedToClosed x = PLiftedClosed $ popaque $ getPLifted x

-- | @since WIP
pliftedFromClosed ::
  forall (a :: S -> Type) (s :: S).
  PLiftedClosed a ->
  PLifted s a
pliftedFromClosed (PLiftedClosed x) = PLifted x

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PInteger Integer)
  instance
    PLiftable PInteger

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PBool Bool)
  instance
    PLiftable PBool

-- | @since WIP
instance
  (PTx.ToData (AsHaskell a), PTx.FromData (AsHaskell a), PIsData a) =>
  PLiftable (PAsData a)
  where
  type AsHaskell (PAsData a) = AsHaskell a
  type PlutusRepr (PAsData a) = PTx.Data
  {-# INLINEABLE haskToRepr #-}
  haskToRepr = PTx.toData
  {-# INLINEABLE reprToHask #-}
  reprToHask = maybe (Left CouldNotDecodeData) Right . PTx.fromData
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since WIP
instance
  ( PLiftable a
  , PLC.DefaultUni `Includes` PlutusRepr a
  , PLiftable b
  , PLC.DefaultUni `Includes` PlutusRepr b
  ) =>
  PLiftable (PBuiltinPair a b)
  where
  type AsHaskell (PBuiltinPair a b) = (AsHaskell a, AsHaskell b)
  type PlutusRepr (PBuiltinPair a b) = (PlutusRepr a, PlutusRepr b)
  {-# INLINEABLE haskToRepr #-}
  haskToRepr (a, b) = (haskToRepr @a a, haskToRepr @b b)
  {-# INLINEABLE reprToHask #-}
  reprToHask (a, b) = (,) <$> reprToHask @a a <*> reprToHask @b b
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since WIP
instance
  (PLiftable a, PLC.DefaultUni `Includes` PlutusRepr a) =>
  PLiftable (PBuiltinList a)
  where
  type AsHaskell (PBuiltinList a) = [AsHaskell a]
  type PlutusRepr (PBuiltinList a) = [PlutusRepr a]
  {-# INLINEABLE haskToRepr #-}
  haskToRepr = fmap (haskToRepr @a)
  {-# INLINEABLE reprToHask #-}
  reprToHask = traverse (reprToHask @a)
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since WIP
instance PLiftable PByteString where
  type AsHaskell PByteString = ByteString
  type PlutusRepr PByteString = ByteString
  {-# INLINEABLE haskToRepr #-}
  haskToRepr = id
  {-# INLINEABLE reprToHask #-}
  reprToHask = Right
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since WIP
instance PLiftable PData where
  type AsHaskell PData = PTx.Data
  type PlutusRepr PData = PTx.Data
  {-# INLINEABLE haskToRepr #-}
  haskToRepr = id
  {-# INLINEABLE reprToHask #-}
  reprToHask = Right
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since WIP
instance PLiftable PByte where
  type AsHaskell PByte = Word8
  type PlutusRepr PByte = Integer
  {-# INLINEABLE haskToRepr #-}
  haskToRepr = fromIntegral
  {-# INLINEABLE reprToHask #-}
  reprToHask = maybe (Left (OtherLiftError "Integral size out of range")) Right . toIntegralSized
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PUnit ())
  instance
    PLiftable PUnit

deriving via
  (DeriveBuiltinPLiftable PString Text)
  instance
    PLiftable PString

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PBuiltinBLS12_381_G1_Element BLS12_381.G1.Element)
  instance
    PLiftable PBuiltinBLS12_381_G1_Element

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PBuiltinBLS12_381_G2_Element BLS12_381.G2.Element)
  instance
    PLiftable PBuiltinBLS12_381_G2_Element

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PBuiltinBLS12_381_MlResult BLS12_381.Pairing.MlResult)
  instance
    PLiftable PBuiltinBLS12_381_MlResult
