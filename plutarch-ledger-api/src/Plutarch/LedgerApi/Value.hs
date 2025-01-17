{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Value-related functionality. In order to keep the interface efficient and
 safe at the same time, there is a type-level distinction between 'PValue's
 that are guaranteed to be properly normalized and those that provide no
 such guarantee.

 Also for efficiency reasons, the Ada-specific functions assume that there
 can be only one token name for the Ada currency symbol, and they don't check
 whether it matches 'Plutus.adaToken'.
-}
module Plutarch.LedgerApi.Value (
  -- * Types
  PValue (..),
  PCurrencySymbol (..),
  PTokenName (..),
  AmountGuarantees (..),
  PLovelace (..),
  PAssetClass (..),

  -- * Functions

  -- ** Creation

  -- *** PCurrencySymbol
  padaSymbol,
  padaSymbolData,

  -- *** PTokenName
  padaToken,

  -- *** PValue
  psingleton,
  psingletonData,
  pconstantPositiveSingleton,

  -- ** Transformation
  passertPositive,
  passertNonZero,
  passertSorted,
  pforgetPositive,
  pforgetSorted,
  pnormalize,
  padaOnlyValue,
  pnoAdaValue,

  -- ** Partial ordering
  pltPositive,
  pltNonZero,
  pleqPositive,
  pleqNonZero,
  pcheckBinRel,

  -- ** Combination
  pleftBiasedCurrencyUnion,
  pleftBiasedTokenUnion,
  punionResolvingCollisionsWith,
  punionResolvingCollisionsWithData,

  -- ** Queries
  pvalueOf,
  plovelaceValueOf,
  pisAdaOnlyValue,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.Prelude hiding (psingleton)
import Plutarch.Prelude qualified as PPrelude
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import PlutusLedgerApi.V1.Value qualified as PlutusValue
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx.Prelude qualified as PlutusTx

-- | @since 2.2.0
newtype PLovelace (s :: S) = PLovelace (Term s (PDataNewtype PInteger))
  deriving stock
    ( -- | @since 2.2.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.2.0
      PlutusType
    , -- | @since 2.2.0
      PIsData
    , -- | @since 2.2.0
      PEq
    , -- | @since WIP
      POrd
    , -- | @since 2.2.0
      PShow
    , -- | @since 3.1.0
      PTryFrom PData
    )

-- | @since 2.2.0
instance DerivePlutusType PLovelace where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PLovelace Plutus.Lovelace
  instance
    PLiftable PLovelace

-- | @since 3.1.0
instance PTryFrom PData (PAsData PLovelace)

-- | @since 2.0.0
newtype PTokenName (s :: S) = PTokenName (Term s (PDataNewtype PByteString))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTokenName where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PTokenName Plutus.TokenName
  instance
    PLiftable PTokenName

-- | @since 3.1.0
instance PTryFrom PData PTokenName where
  type PTryFromExcess PData PTokenName = Mret PTokenName
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif (plengthBS # unwrapped #<= 32) (f ()) (ptraceInfoError "ptryFrom(TokenName): must be at most 32 bytes long")
    pure (punsafeCoerce opq, pcon . PTokenName . pcon . PDataNewtype . pdata $ unwrapped)

-- | @since 2.0.0
instance PTryFrom PData (PAsData PTokenName) where
  type PTryFromExcess PData (PAsData PTokenName) = Mret PTokenName
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif (plengthBS # unwrapped #<= 32) (f ()) (ptraceInfoError "ptryFrom(TokenName): must be at most 32 bytes long")
    pure (punsafeCoerce opq, pcon . PTokenName . pcon . PDataNewtype . pdata $ unwrapped)

-- | @since 2.0.0
newtype PCurrencySymbol (s :: S) = PCurrencySymbol (Term s (PDataNewtype PByteString))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PCurrencySymbol where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PCurrencySymbol Plutus.CurrencySymbol
  instance
    PLiftable PCurrencySymbol

-- | @since 3.1.0
instance PTryFrom PData PCurrencySymbol where
  type PTryFromExcess PData PCurrencySymbol = Mret PCurrencySymbol
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    len <- tcont . plet $ plengthBS # unwrapped
    tcont $ \f ->
      pif
        (len #== 0 #|| len #== 28)
        (f ())
        (ptraceInfoError "ptryFrom(CurrencySymbol): must be 28 bytes long or empty")
    pure (punsafeCoerce opq, pcon . PCurrencySymbol . pcon . PDataNewtype . pdata $ unwrapped)

-- | @since 2.0.0
instance PTryFrom PData (PAsData PCurrencySymbol) where
  type PTryFromExcess PData (PAsData PCurrencySymbol) = Mret PCurrencySymbol
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    len <- tcont . plet $ plengthBS # unwrapped
    tcont $ \f ->
      pif (len #== 0 #|| len #== 28) (f ()) (ptraceInfoError "ptryFrom(CurrencySymbol): must be 28 bytes long or empty")
    pure (punsafeCoerce opq, pcon . PCurrencySymbol . pcon . PDataNewtype . pdata $ unwrapped)

-- | @since 2.0.0
data AmountGuarantees = NoGuarantees | NonZero | Positive

-- | @since 2.0.0
newtype PValue (keys :: AssocMap.KeyGuarantees) (amounts :: AmountGuarantees) (s :: S)
  = PValue (Term s (AssocMap.PMap keys PCurrencySymbol (AssocMap.PMap keys PTokenName PInteger)))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PShow
    )

type role PValue nominal nominal nominal

-- | @since 2.0.0
instance DerivePlutusType (PValue keys amounts) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveNewtypePLiftable
    (PValue 'AssocMap.Unsorted 'NoGuarantees)
    Plutus.Value
  instance
    PLiftable (PValue 'AssocMap.Unsorted 'NoGuarantees)

-- | @since 2.0.0
instance PEq (PValue 'AssocMap.Sorted 'Positive) where
  a #== b = pto a #== pto b

-- | @since 2.0.0
instance PEq (PValue 'AssocMap.Sorted 'NonZero) where
  a #== b = pto a #== pto b

{- | Mimics the @lt@ operation on @plutus-ledger-api@'s @Value@.

@since WIP
-}
pltPositive ::
  forall (s :: S).
  Term s (PValue 'AssocMap.Sorted 'Positive) ->
  Term s (PValue 'AssocMap.Sorted 'Positive) ->
  Term s PBool
pltPositive t1 t2 = pltNonZero (pforgetPositive t1) (pforgetPositive t2)

{- | As 'pltPositive', but for nonzero guaranteed 'PValue's instead.

@since WIP
-}
pltNonZero ::
  forall (s :: S).
  Term s (PValue 'AssocMap.Sorted 'NonZero) ->
  Term s (PValue 'AssocMap.Sorted 'NonZero) ->
  Term s PBool
pltNonZero t1 t2 = pleqNonZero t1 t2 #&& (pnot # (t1 #== t2))

{- | Mimics the @leq@ operation on @plutus-ledger-api@'s @Value@.

@since WIP
-}
pleqPositive ::
  forall (s :: S).
  Term s (PValue 'AssocMap.Sorted 'Positive) ->
  Term s (PValue 'AssocMap.Sorted 'Positive) ->
  Term s PBool
pleqPositive t1 t2 = pleqNonZero (pforgetPositive t1) (pforgetPositive t2)

{- | As 'pletPositive', but for nonzero guaranteed 'PValue's instead.

@since WIP
-}
pleqNonZero ::
  forall (s :: S).
  Term s (PValue 'AssocMap.Sorted 'NonZero) ->
  Term s (PValue 'AssocMap.Sorted 'NonZero) ->
  Term s PBool
pleqNonZero t1 t2 =
  phoistAcyclic (pcheckBinRel #$ phoistAcyclic $ plam (#<=)) # t1 # t2

-- | @since 2.0.0
instance PEq (PValue 'AssocMap.Sorted 'NoGuarantees) where
  a #== b =
    AssocMap.pall
      # (AssocMap.pall # plam (#== 0))
      -- While '(-)' is not commutative, we don't need that property here.
      -- TODO benchmark with '(==)'
      # pto (punionResolvingCollisionsWith AssocMap.Commutative # plam (-) # a # b)

-- | @since WIP
instance PSemigroup (PValue 'AssocMap.Sorted 'Positive) where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (<>)

-- | @since 2.0.0
instance Semigroup (Term s (PValue 'AssocMap.Sorted 'Positive)) where
  a <> b =
    punsafeDowncast (pto $ punionResolvingCollisionsWith AssocMap.Commutative # plam (+) # a # b)

-- | @since 2.0.0
instance PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'Positive)) where
  a <> b =
    punsafeDowncast (pto $ punionResolvingCollisionsWith AssocMap.Commutative # plam (+) # a # b)

-- | @since WIP
instance PSemigroup (PValue 'AssocMap.Sorted 'NonZero) where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (<>)

-- | @since 2.0.0
instance Semigroup (Term s (PValue 'AssocMap.Sorted 'NonZero)) where
  a <> b =
    pnormalize #$ punionResolvingCollisionsWith AssocMap.Commutative # plam (+) # a # b

-- | @since 2.0.0
instance PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'NonZero)) where
  a <> b =
    pnormalize #$ punionResolvingCollisionsWith AssocMap.Commutative # plam (+) # a # b

-- | @since WIP
instance PSemigroup (PValue 'AssocMap.Sorted 'NoGuarantees) where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (<>)

-- | @since 2.0.0
instance Semigroup (Term s (PValue 'AssocMap.Sorted 'NoGuarantees)) where
  a <> b =
    punionResolvingCollisionsWith AssocMap.Commutative # plam (+) # a # b

-- | @since 2.0.0
instance PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'NoGuarantees)) where
  a <> b =
    punionResolvingCollisionsWith AssocMap.Commutative # plam (+) # a # b

-- | @since WIP
instance
  PSemigroup (PValue 'AssocMap.Sorted normalization) =>
  PMonoid (PValue 'AssocMap.Sorted normalization)
  where
  {-# INLINEABLE pmempty #-}
  pmempty = pcon (PValue AssocMap.pempty)

-- | @since 2.0.0
instance
  Semigroup (Term s (PValue 'AssocMap.Sorted normalization)) =>
  Monoid (Term s (PValue 'AssocMap.Sorted normalization))
  where
  mempty = pcon (PValue AssocMap.pempty)

-- | @since 2.0.0
instance
  PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted normalization)) =>
  PlutusTx.Monoid (Term s (PValue 'AssocMap.Sorted normalization))
  where
  mempty = pcon (PValue AssocMap.pempty)

-- | @since 2.0.0
instance
  PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'NoGuarantees)) =>
  PlutusTx.Group (Term s (PValue 'AssocMap.Sorted 'NoGuarantees))
  where
  inv a = pmapAmounts # plam negate # a

-- | @since 2.0.0
instance
  PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'NonZero)) =>
  PlutusTx.Group (Term s (PValue 'AssocMap.Sorted 'NonZero))
  where
  inv a =
    punsafeCoerce $ PlutusTx.inv (punsafeCoerce a :: Term s (PValue 'AssocMap.Sorted 'NoGuarantees))

-- | @since 2.0.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Unsorted 'NoGuarantees))

-- | @since 2.0.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Sorted 'NoGuarantees))

-- | @since 2.0.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Sorted 'Positive)) where
  type PTryFromExcess PData (PAsData (PValue 'AssocMap.Sorted 'Positive)) = Mret (PValue 'AssocMap.Sorted 'Positive)
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PValue 'AssocMap.Sorted 'NoGuarantees)) opq
    unwrapped <- tcont . plet . papp passertPositive . pfromData $ opq'
    pure (punsafeCoerce opq, unwrapped)

-- | @since 2.0.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Unsorted 'Positive)) where
  type PTryFromExcess PData (PAsData (PValue 'AssocMap.Unsorted 'Positive)) = Mret (PValue 'AssocMap.Unsorted 'Positive)
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PValue 'AssocMap.Unsorted 'NoGuarantees)) opq
    unwrapped <- tcont . plet . papp passertPositive . pfromData $ opq'
    pure (punsafeCoerce opq, unwrapped)

-- | @since 2.0.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Sorted 'NonZero)) where
  type PTryFromExcess PData (PAsData (PValue 'AssocMap.Sorted 'NonZero)) = Mret (PValue 'AssocMap.Sorted 'NonZero)
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PValue 'AssocMap.Sorted 'NoGuarantees)) opq
    unwrapped <- tcont . plet . papp passertNonZero . pfromData $ opq'
    pure (punsafeCoerce opq, unwrapped)

-- | @since 2.1.1
instance PTryFrom PData (PAsData (PValue 'AssocMap.Unsorted 'NonZero)) where
  type PTryFromExcess PData (PAsData (PValue 'AssocMap.Unsorted 'NonZero)) = Mret (PValue 'AssocMap.Unsorted 'NonZero)
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PValue 'AssocMap.Unsorted 'NoGuarantees)) opq
    unwrapped <- tcont . plet . papp passertNonZero . pfromData $ opq'
    pure (punsafeCoerce opq, unwrapped)

-- | @since WIP
newtype PAssetClass (s :: S) = PAssetClass (Term s (PDataNewtype (PBuiltinPair (PAsData PCurrencySymbol) (PAsData PTokenName))))
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    , -- | @since WIP
      PIsData
    , -- | @since WIP
      PEq
    , -- | @since WIP
      PShow
    , -- | @since WIP
      PTryFrom PData
    )

-- | @since WIP
instance POrd PAssetClass where
  {-# INLINEABLE (#<=) #-}
  ac1 #<= ac2 = pmatch ac1 $ \(PAssetClass ac1') ->
    pmatch ac2 $ \(PAssetClass ac2') ->
      pmatch ac1' $ \(PDataNewtype pair1) ->
        pmatch ac2' $ \(PDataNewtype pair2) ->
          plet (pfromData $ pfstBuiltin # pfromData pair1) $ \fst1 ->
            plet (pfromData $ pfstBuiltin # pfromData pair2) $ \fst2 ->
              (fst1 #< fst2)
                #|| ( (fst1 #== fst2)
                        #&& let snd1 = pfromData $ psndBuiltin # pfromData pair1
                                snd2 = pfromData $ psndBuiltin # pfromData pair2
                             in snd1 #<= snd2
                    )
  {-# INLINEABLE (#<) #-}
  ac1 #< ac2 = pmatch ac1 $ \(PAssetClass ac1') ->
    pmatch ac2 $ \(PAssetClass ac2') ->
      pmatch ac1' $ \(PDataNewtype pair1) ->
        pmatch ac2' $ \(PDataNewtype pair2) ->
          plet (pfromData $ pfstBuiltin # pfromData pair1) $ \fst1 ->
            plet (pfromData $ pfstBuiltin # pfromData pair2) $ \fst2 ->
              (fst1 #< fst2)
                #|| ( (fst1 #== fst2)
                        #&& let snd1 = pfromData $ psndBuiltin # pfromData pair1
                                snd2 = pfromData $ psndBuiltin # pfromData pair2
                             in snd1 #< snd2
                    )

-- | @since WIP
instance DerivePlutusType PAssetClass where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PAssetClass PlutusValue.AssetClass
  instance
    PLiftable PAssetClass

-- | @since WIP
instance PTryFrom PData (PAsData PAssetClass)

{- | \'Forget\' that a 'Value' has an only-positive guarantee.

@since 2.0.0
-}
pforgetPositive ::
  forall (a :: AmountGuarantees) (k :: AssocMap.KeyGuarantees) (s :: S).
  Term s (PValue k 'Positive) ->
  Term s (PValue k a)
pforgetPositive = punsafeCoerce

{- | Given a description of a relation on amounts, check whether that relation
holds over sorted 'PValue's.

= Important note

This is intended for use with boolean comparison functions, which must define
at least a partial order (total orders and equivalences are acceptable as
well). Use of this with anything else is not guaranteed to give anything
resembling a sensible answer. Use with extreme care.

@since 2.0.0
-}
pcheckBinRel ::
  forall (any0 :: AmountGuarantees) (any1 :: AmountGuarantees) (s :: S).
  Term
    s
    ( (PInteger :--> PInteger :--> PBool)
        :--> PValue 'AssocMap.Sorted any0
        :--> PValue 'AssocMap.Sorted any1
        :--> PBool
    )
pcheckBinRel = phoistAcyclic $
  plam $ \f ->
    punsafeCoerce @(PValue AssocMap.Sorted any0 :--> PValue AssocMap.Sorted any1 :--> PBool) $
      AssocMap.pcheckBinRel @PCurrencySymbol # (AssocMap.pcheckBinRel @PTokenName # f # 0) # AssocMap.pempty

{- | Combine two 'PValue's applying the given function to any pair of
 quantities with the same asset class. Note that the result is _not_
 'normalize'd and may contain zero quantities.

 @since 2.0.0
-}
punionResolvingCollisionsWith ::
  forall (any0 :: AmountGuarantees) (any1 :: AmountGuarantees) (s :: S).
  AssocMap.Commutativity ->
  Term
    s
    ( (PInteger :--> PInteger :--> PInteger)
        :--> PValue 'AssocMap.Sorted any0
        :--> PValue 'AssocMap.Sorted any1
        :--> PValue 'AssocMap.Sorted 'NoGuarantees
    )
punionResolvingCollisionsWith commutativity = phoistAcyclic $
  plam $ \combine x y ->
    pcon . PValue $
      AssocMap.punionResolvingCollisionsWith commutativity
        # plam (\x' y' -> AssocMap.punionResolvingCollisionsWith commutativity # combine # x' # y')
        # pto x
        # pto y

{- | Normalize the argument to contain no zero quantity nor empty token map.

@since 2.0.0
-}
pnormalize ::
  forall (any :: AmountGuarantees) (s :: S).
  Term s (PValue 'AssocMap.Sorted any :--> PValue 'AssocMap.Sorted 'NonZero)
pnormalize = phoistAcyclic $
  plam $ \value ->
    pcon . PValue $
      AssocMap.pmapMaybe # plam normalizeTokenMap # pto value
  where
    normalizeTokenMap ::
      forall (s' :: S) (k :: S -> Type) (any1 :: AssocMap.KeyGuarantees).
      Term s' (AssocMap.PMap any1 k PInteger) ->
      Term s' (PMaybe (AssocMap.PMap any1 k PInteger))
    normalizeTokenMap tokenMap =
      plet (AssocMap.pmapMaybeData # plam nonZero # tokenMap) $ \normalMap ->
        pif
          (AssocMap.pnull # normalMap)
          (pcon PNothing)
          (pcon $ PJust normalMap)
    nonZero ::
      forall (s' :: S).
      Term s' (PAsData PInteger) ->
      Term s' (PMaybe (PAsData PInteger))
    nonZero intData =
      pif (intData #== zeroData) (pcon PNothing) (pcon $ PJust intData)

{- | Given a 'PValue', either construct another 'PValue' with the same contents
and a proof that all amounts in it are positive, or error.

@since 2.0.0
-}
passertPositive ::
  forall (kg :: AssocMap.KeyGuarantees) (ag :: AmountGuarantees) (s :: S).
  Term s (PValue kg ag :--> PValue kg 'Positive)
passertPositive = phoistAcyclic $
  plam $ \value ->
    pif
      ( AssocMap.pall
          # plam (\submap -> AssocMap.pall # plam (0 #<) # submap)
          # pto value
      )
      (punsafeDowncast $ pto value)
      (ptraceInfoError "Negative amount in Value")

{- | Construct a constant singleton 'PValue' containing only the given
positive quantity of the given currency.

@since 2.1.1
-}
pconstantPositiveSingleton ::
  forall (s :: S).
  (forall (s' :: S). Term s' PCurrencySymbol) ->
  (forall (s' :: S). Term s' PTokenName) ->
  (forall (s' :: S). Term s' PInteger) ->
  Term s (PValue 'AssocMap.Sorted 'Positive)
pconstantPositiveSingleton symbol token amount
  | plift amount == 0 = mempty
  | plift amount < 0 = error "Negative amount"
  | otherwise = punsafeDowncast (AssocMap.psingleton # symbol #$ AssocMap.psingleton # token # amount)

{- | The 'PCurrencySymbol' of the Ada currency.

@since 2.1.1
-}
padaSymbol :: forall (s :: S). Term s PCurrencySymbol
padaSymbol = pconstant Plutus.adaSymbol

{- | Data-encoded 'PCurrencySymbol' of the Ada currency.

@since 2.1.1
-}
padaSymbolData :: forall (s :: S). Term s (PAsData PCurrencySymbol)
padaSymbolData = pdata padaSymbol

{- | The 'PTokenName' of the Ada currency.

@since 2.1.1
-}
padaToken :: Term s PTokenName
padaToken = pconstant Plutus.adaToken

{- | Forget the knowledge of all value's guarantees.

@since 2.1.1
-}
pforgetSorted ::
  forall (a :: AmountGuarantees) (k :: AssocMap.KeyGuarantees) (s :: S).
  Term s (PValue 'AssocMap.Sorted a) ->
  Term s (PValue k a)
pforgetSorted = punsafeCoerce

{- | Construct a singleton 'PValue' containing only the given quantity of the
given currency.

@since 2.1.1
-}
psingleton ::
  forall (s :: S).
  Term
    s
    (PCurrencySymbol :--> PTokenName :--> PInteger :--> PValue 'AssocMap.Sorted 'NonZero)
psingleton = phoistAcyclic $
  plam $ \symbol token amount ->
    pif
      (amount #== 0)
      mempty
      (punsafeDowncast $ AssocMap.psingleton # symbol #$ AssocMap.psingleton # token # amount)

{- | Construct a singleton 'PValue' containing only the given quantity of the
 given currency, taking data-encoded parameters.

 @since 2.1.1
-}
psingletonData ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PAsData PInteger
        :--> PValue 'AssocMap.Sorted 'NonZero
    )
psingletonData = phoistAcyclic $
  plam $ \symbol token amount ->
    pif
      (amount #== zeroData)
      mempty
      ( punsafeDowncast
          ( AssocMap.psingletonData
              # symbol
              #$ pdata
              $ AssocMap.psingletonData # token # amount
          )
      )

{- | Get the quantity of the given currency in the 'PValue'.

@since 2.1.1
-}
pvalueOf ::
  forall (anyKey :: AssocMap.KeyGuarantees) (anyAmount :: AmountGuarantees) (s :: S).
  Term s (PValue anyKey anyAmount :--> PCurrencySymbol :--> PTokenName :--> PInteger)
pvalueOf = phoistAcyclic $
  plam $ \value symbol token ->
    AssocMap.pfoldAt
      # symbol
      # 0
      # plam (\m -> AssocMap.pfoldAt # token # 0 # plam pfromData # pfromData m)
      # pto value

{- | Get the amount of Lovelace in the 'PValue'.

@since 2.1.1
-}
plovelaceValueOf ::
  forall (v :: AmountGuarantees) (s :: S).
  Term s (PValue 'AssocMap.Sorted v :--> PInteger)
plovelaceValueOf = phoistAcyclic $
  plam $ \value ->
    pmatch (pto $ pto value) $ \case
      PNil -> 0
      PCons x _ ->
        pif'
          # (pfstBuiltin # x #== padaSymbolData)
          # pfromData (psndBuiltin #$ phead #$ pto $ pfromData $ psndBuiltin # x)
          # 0

{- | Combine two 'PValue's, taking the tokens from the left only, if a
currency occurs on both sides.

@since 2.1.1
-}
pleftBiasedCurrencyUnion ::
  forall (any0 :: AmountGuarantees) (any1 :: AmountGuarantees) (s :: S).
  Term
    s
    ( PValue 'AssocMap.Sorted any0
        :--> PValue 'AssocMap.Sorted any1
        :--> PValue 'AssocMap.Sorted 'NoGuarantees
    )
pleftBiasedCurrencyUnion = phoistAcyclic $
  plam $
    \x y -> pcon . PValue $ AssocMap.pleftBiasedUnion # pto x # pto y

{- | Combine two 'PValue's, taking the tokens from the left only, if a token name
 of the same currency occurs on both sides.

 Prefer this over 'punionResolvingCollisionsWith NonCommutative # plam const'.
 It is equivalent, but performs better.

 @since 2.1.1
-}
pleftBiasedTokenUnion ::
  forall (any0 :: AmountGuarantees) (any1 :: AmountGuarantees) (s :: S).
  Term
    s
    ( PValue 'AssocMap.Sorted any0
        :--> PValue 'AssocMap.Sorted any1
        :--> PValue 'AssocMap.Sorted 'NoGuarantees
    )
pleftBiasedTokenUnion = phoistAcyclic $
  plam $ \x y ->
    pcon . PValue $
      AssocMap.punionResolvingCollisionsWith AssocMap.NonCommutative
        # plam (\x' y' -> AssocMap.pleftBiasedUnion # x' # y')
        # pto x
        # pto y

{- | Combine two 'PValue's applying the given function to any pair of
 data-encoded quantities with the same asset class. Note that the result is
 _not_ 'normalize'd and may contain zero quantities.

 @since 2.1.1
-}
punionResolvingCollisionsWithData ::
  forall (any0 :: AmountGuarantees) (any1 :: AmountGuarantees) (s :: S).
  AssocMap.Commutativity ->
  Term
    s
    ( (PAsData PInteger :--> PAsData PInteger :--> PAsData PInteger)
        :--> PValue 'AssocMap.Sorted any0
        :--> PValue 'AssocMap.Sorted any1
        :--> PValue 'AssocMap.Sorted 'NoGuarantees
    )
punionResolvingCollisionsWithData commutativity = phoistAcyclic $
  plam $ \combine x y ->
    pcon . PValue $
      AssocMap.punionResolvingCollisionsWith commutativity
        # plam (\x' y' -> AssocMap.punionResolvingCollisionsWithData commutativity # combine # x' # y')
        # pto x
        # pto y

{- | Assert the value is properly sorted and normalized.

@since 2.1.1
-}
passertSorted ::
  forall (anyKey :: AssocMap.KeyGuarantees) (anyAmount :: AmountGuarantees) (s :: S).
  Term s (PValue anyKey anyAmount :--> PValue 'AssocMap.Sorted 'NonZero)
passertSorted = phoistAcyclic $
  plam $ \value ->
    pif
      ( AssocMap.pany
          # plam
            ( \submap ->
                AssocMap.pnull
                  # (AssocMap.passertSorted # submap)
                  #|| AssocMap.pany
                  # plam (#== 0)
                  # submap
            )
          # pto value
      )
      (ptraceInfoError "Abnormal Value")
      . pcon
      . PValue
      $ AssocMap.passertSorted #$ punsafeCoerce
      $ pto value

{- | Test if the value contains nothing but Ada

@since 2.1.1
-}
pisAdaOnlyValue ::
  forall (s :: S).
  Term s (PValue 'AssocMap.Sorted 'Positive :--> PBool)
pisAdaOnlyValue = phoistAcyclic $
  plam $ \value ->
    pmatch (pto $ pto value) $ \case
      PNil -> pcon PTrue
      PCons x xs -> pand' # (pnull # xs) # (pfstBuiltin # x #== padaSymbolData)

{- | Strip all non-Ada from a 'PValue'.

@since 2.1.1
-}
padaOnlyValue ::
  forall (v :: AmountGuarantees) (s :: S).
  Term s (PValue 'AssocMap.Sorted v :--> PValue 'AssocMap.Sorted v)
padaOnlyValue = phoistAcyclic $
  plam $ \value ->
    pmatch (pto $ pto value) $ \case
      PNil -> value
      PCons x _ ->
        pif'
          # (pfstBuiltin # x #== padaSymbolData)
          # pcon (PValue $ pcon $ AssocMap.PMap $ PPrelude.psingleton # x)
          # pcon (PValue AssocMap.pempty)

{- | Strip all Ada from a 'PValue'.

@since 2.1.1
-}
pnoAdaValue ::
  forall (v :: AmountGuarantees) (s :: S).
  Term s (PValue 'AssocMap.Sorted v :--> PValue 'AssocMap.Sorted v)
pnoAdaValue = phoistAcyclic $
  plam $ \value ->
    pmatch (pto $ pto value) $ \case
      PNil -> value
      PCons x xs -> pif' # (pfstBuiltin # x #== padaSymbolData) # pcon (PValue $ pcon $ AssocMap.PMap xs) # value

-- Helpers

zeroData :: forall (s :: S). Term s (PAsData PInteger)
zeroData = pdata 0

-- Applies a function to every amount in the map.
pmapAmounts ::
  forall (k :: AssocMap.KeyGuarantees) (a :: AmountGuarantees) (s :: S).
  Term s ((PInteger :--> PInteger) :--> PValue k a :--> PValue k 'NoGuarantees)
pmapAmounts = phoistAcyclic $
  plam $
    \f v -> pcon $ PValue $ AssocMap.pmap # plam (AssocMap.pmap # f #) # pto v

passertNonZero ::
  forall (kg :: AssocMap.KeyGuarantees) (ag :: AmountGuarantees).
  ( forall (s :: S). Term s (PValue kg ag :--> PValue kg 'NonZero)
  )
passertNonZero = plam $ \val ->
  pif (outer #$ pto . pto $ val) (punsafeCoerce val) (ptraceInfoError "Zero amount in Value")
  where
    outer ::
      forall (s' :: S) (k :: AssocMap.KeyGuarantees).
      Term s' (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PMap k PTokenName PInteger))) :--> PBool)
    outer = pfix #$ plam $ \self m ->
      pmatch m $ \case
        PCons x xs -> inner # (pto . pfromData $ psndBuiltin # x) #&& self # xs
        PNil -> pcon PTrue
    inner :: ClosedTerm (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool)
    inner = pfix #$ plam $ \self m ->
      pmatch m $ \case
        PCons x xs -> pnot # (psndBuiltin # x #== pconstant @(PAsData PInteger) 0) #&& self # xs
        PNil -> pcon PTrue
