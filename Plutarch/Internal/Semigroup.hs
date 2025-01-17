{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Semigroup (
  -- * Type classes
  PSemigroup (..),
  PMonoid (..),

  -- * Helper newtypes
  PAnd (..),
  POr (..),
  PXor (..),
) where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.BLS (
  PBuiltinBLS12_381_G1_Element,
  PBuiltinBLS12_381_G2_Element,
  PBuiltinBLS12_381_MlResult,
 )
import Plutarch.Builtin.Bool (
  PBool (PFalse, PTrue),
  pif,
  pif',
  pnot,
 )
import Plutarch.Builtin.ByteString (
  PByteString,
  pandBS,
  pindexBS,
  plengthBS,
  porBS,
  ppadding,
  preplicateBS,
  pxorBS,
 )
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.String (PString)
import Plutarch.Builtin.Unit (PUnit, punit)
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Lift (
  DeriveNewtypePLiftable,
  PLiftable (AsHaskell),
  PLifted (PLifted),
 )
import Plutarch.Internal.Numeric (
  PNatural,
  PPositive,
  pbySquaringDefault,
  pdiv,
  pnaturalToPositiveCPS,
  pscaleNatural,
  pscalePositive,
  pzero,
  (#*),
  (#+),
 )
import Plutarch.Internal.Ord (POrd)
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PlutusType (PlutusType (PInner), pcon)
import Plutarch.Internal.Term (
  S,
  Term,
  plet,
  punsafeBuiltin,
  punsafeConstantInternal,
  (#),
  (#$),
 )
import Plutarch.Repr.Newtype (DeriveAsNewtype (DeriveAsNewtype))
import Plutarch.Unsafe (punsafeDowncast)
import PlutusCore qualified as PLC

{- | = Laws

The only mandatory law is that '#<>' must be associative:

@x #<> (y #<> z)@ @=@ @(x #<> y) #<> z@

If you define 'pstimes', ensure the following also hold:

1. @pstimes pone x@ @=@ @x@
2. @(pstimes p1 x) #<> (pstimes p2 x)@ @=@ @pstimes (p1 #+ p2) x@
3. @pstimes p1 (pstimes p2 x)@ @=@ @pstimes (p1 #* p2) x@

The default implementation automatically ensures these laws hold.

@since WIP
-}
class PSemigroup (a :: S -> Type) where
  (#<>) :: forall (s :: S). Term s a -> Term s a -> Term s a
  default (#<>) ::
    forall (s :: S).
    PSemigroup (PInner a) =>
    Term s a ->
    Term s a ->
    Term s a
  x #<> y = punsafeDowncast $ pto x #<> pto y
  {-# INLINEABLE pstimes #-}
  pstimes :: forall (s :: S). Term s PPositive -> Term s a -> Term s a
  pstimes p x = pbySquaringDefault (#<>) x p

infixr 6 #<>

-- | @since WIP
instance PSemigroup PUnit where
  {-# INLINEABLE (#<>) #-}
  x #<> y = plet x $ \_ -> plet y $ const punit
  {-# INLINEABLE pstimes #-}
  pstimes p x = plet p $ \_ -> plet x $ const punit

-- | @since WIP
instance PSemigroup PString where
  {-# INLINEABLE (#<>) #-}
  x #<> y = punsafeBuiltin PLC.AppendString # x # y

-- | @since WIP
instance PSemigroup PByteString where
  {-# INLINEABLE (#<>) #-}
  x #<> y = punsafeBuiltin PLC.AppendByteString # x # y
  {-# INLINEABLE pstimes #-}
  pstimes p bs = plet bs $ \bs' ->
    pif
      (plengthBS # bs' #== 1)
      (preplicateBS # pto p #$ pindexBS # bs' # 0)
      (pbySquaringDefault (#<>) bs' p)

{- | BLS points form a group technically, but a @PGroup@ notion would be too
niche to be useful. Unlike other types which could be semigroups (or monoids)
in many ways, BLS points have only one (essentially their additive
instances), so we can provide these.

@since WIP
-}
instance PSemigroup PBuiltinBLS12_381_G1_Element where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (#+)
  {-# INLINEABLE pstimes #-}
  pstimes p x = pscalePositive x p

-- | @since WIP
instance PSemigroup PBuiltinBLS12_381_G2_Element where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (#+)
  {-# INLINEABLE pstimes #-}
  pstimes p x = pscalePositive x p

{- | Since multiplication of Miller loop results exists, they are technically
semigroups, though confusingly in a /different/ way to BLS curve points.

@since WIP
-}
instance PSemigroup PBuiltinBLS12_381_MlResult where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (#*)

{- | = Laws

1. @pmempty #<> x@ @=@ @x #<> pmempty@ @=@ @x@
2. @pstimes n pmempty@ @=@ @pmempty@

If you define 'pmtimes', ensure the following as well:

3. @pmtimes (ppositiveToNatural # p) x@ @=@
   @pstimes p x@
4. @pmtimes pzero x@ @=@ @pmempty@

The default implementation of 'pmtimes' ensures these laws hold.

@since WIP
-}
class PSemigroup a => PMonoid (a :: S -> Type) where
  pmempty :: forall (s :: S). Term s a
  default pmempty ::
    forall (s :: S).
    PMonoid (PInner a) =>
    Term s a
  pmempty = punsafeDowncast pmempty
  {-# INLINEABLE pmtimes #-}
  pmtimes :: forall (s :: S). Term s PNatural -> Term s a -> Term s a
  pmtimes n x = pnaturalToPositiveCPS pmempty (`pstimes` x) n

-- | @since WIP
instance PMonoid PUnit where
  {-# INLINEABLE pmempty #-}
  pmempty = punit
  {-# INLINEABLE pmtimes #-}
  pmtimes n x = plet n $ \_ -> plet x $ const punit

-- | @since WIP
instance PMonoid PString where
  {-# INLINEABLE pmempty #-}
  pmempty = punsafeConstantInternal $ PLC.someValue Text.empty

-- | @since WIP
instance PMonoid PByteString where
  {-# INLINEABLE pmempty #-}
  pmempty = punsafeConstantInternal $ PLC.someValue BS.empty

-- | @since WIP
instance PMonoid PBuiltinBLS12_381_G1_Element where
  {-# INLINEABLE pmempty #-}
  pmempty = pzero
  {-# INLINEABLE pmtimes #-}
  pmtimes n x = pscaleNatural x n

-- | @since WIP
instance PMonoid PBuiltinBLS12_381_G2_Element where
  {-# INLINEABLE pmempty #-}
  pmempty = pzero
  {-# INLINEABLE pmtimes #-}
  pmtimes n x = pscaleNatural x n

{- | Wrapper for types which have logical AND semantics somehow.

@since WIP
-}
newtype PAnd (a :: S -> Type) (s :: S)
  = PAnd (Term s a)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      SOP.Generic
    , -- | @since WIP
      PEq
    , -- | @since WIP
      POrd
    )
  deriving
    ( -- | @since WIP
      PlutusType
    )
    via (DeriveAsNewtype (PAnd a))

-- | @since WIP
deriving via
  DeriveNewtypePLiftable (PAnd a) a (AsHaskell a)
  instance
    PLiftable a => PLiftable (PAnd a)

-- | @since WIP
instance PSemigroup (PAnd PBool) where
  {-# INLINEABLE (#<>) #-}
  x #<> y = pif' # pto x # y # x

  -- \| 'PBool' is idempotent under AND.
  {-# INLINEABLE pstimes #-}
  pstimes p x = plet p $ const x

-- | @since WIP
instance PMonoid (PAnd PBool) where
  {-# INLINEABLE pmempty #-}
  pmempty = pcon . PAnd . pcon $ PTrue

{- | This uses padding semantics as specified in CIP-122, as this allows a
'PMonoid' instance as well.

@since WIP
-}
instance PSemigroup (PAnd PByteString) where
  {-# INLINEABLE (#<>) #-}
  x #<> y = punsafeDowncast $ pandBS # ppadding # pto x # pto y

  -- \| 'PByteString' is idempotent under AND regardless of semantics.
  {-# INLINEABLE pstimes #-}
  pstimes p x = plet p $ const x

-- | @since WIP
instance PMonoid (PAnd PByteString) where
  {-# INLINEABLE pmempty #-}
  pmempty = pcon . PAnd $ mempty

{- | Wrapper for types which have logical OR semantics somehow.

@since WIP
-}
newtype POr (a :: S -> Type) (s :: S)
  = POr (Term s a)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      SOP.Generic
    , -- | @since WIP
      PEq
    , -- | @since WIP
      POrd
    )
  deriving
    ( -- | @since WIP
      PlutusType
    )
    via (DeriveAsNewtype (POr a))

-- | @since WIP
deriving via
  DeriveNewtypePLiftable (POr a) a (AsHaskell a)
  instance
    PLiftable a => PLiftable (POr a)

-- | @since WIP
instance PSemigroup (POr PBool) where
  {-# INLINEABLE (#<>) #-}
  x #<> y = pif (pto x) x y

  -- \| 'PBool' is idempotent under OR.
  {-# INLINEABLE pstimes #-}
  pstimes p x = plet p $ const x

-- | @since WIP
instance PMonoid (POr PBool) where
  {-# INLINEABLE pmempty #-}
  pmempty = pcon . POr . pcon $ PFalse

{- | This uses padding semantics as specified in CIP-122, as this allows a
'PMonoid' instance as well.

@since WIP
-}
instance PSemigroup (POr PByteString) where
  {-# INLINEABLE (#<>) #-}
  x #<> y = punsafeDowncast $ porBS # ppadding # pto x # pto y

  -- \| 'PByteString' is idempotent under OR regardless of semantics.
  {-# INLINEABLE pstimes #-}
  pstimes p x = plet p $ const x

-- | @since WIP
instance PMonoid (POr PByteString) where
  {-# INLINEABLE pmempty #-}
  pmempty = pcon . POr $ mempty

{- | Wrapper for types which have logical XOR semantics somehow.

@since WIP
-}
newtype PXor (a :: S -> Type) (s :: S)
  = PXor (Term s a)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      SOP.Generic
    , -- | @since WIP
      PEq
    , -- | @since WIP
      POrd
    )
  deriving
    ( -- | @since WIP
      PlutusType
    )
    via (DeriveAsNewtype (PXor a))

-- | @since WIP
deriving via
  DeriveNewtypePLiftable (PXor a) a (AsHaskell a)
  instance
    PLiftable a => PLiftable (PXor a)

-- | @since WIP
instance PSemigroup (PXor PBool) where
  {-# INLINEABLE (#<>) #-}
  x #<> y = pif' # pto x # (pcon . PXor $ pnot # pto y) # y

  -- \| Because XOR is self-inverting, there are only two outcomes: either the
  -- argument (if the exponent is odd) or 'PFalse' (if it's even).
  {-# INLINEABLE pstimes #-}
  pstimes = pxortimes (pcon PFalse)

-- | @since WIP
instance PMonoid (PXor PBool) where
  {-# INLINEABLE pmempty #-}
  pmempty = pcon . PXor . pcon $ PFalse
  {-# INLINEABLE pmtimes #-}
  pmtimes = pxortimes (pcon PFalse)

{- | This uses padding semantics as specified in CIP-122, as this allows a
'PMonoid' instance as well.

@since WIP
-}
instance PSemigroup (PXor PByteString) where
  {-# INLINEABLE (#<>) #-}
  x #<> y = punsafeDowncast $ pxorBS # ppadding # pto x # pto y

  -- \| Because XOR is self-inverting, there are only two outcomes: either the
  -- argument (if the exponent is odd) or the empty 'PByteString' (if it isn't).
  {-# INLINEABLE pstimes #-}
  pstimes = pxortimes mempty

-- | @since WIP
instance PMonoid (PXor PByteString) where
  {-# INLINEABLE pmempty #-}
  pmempty = pcon . PXor $ mempty

-- Helpers

pxortimes ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PInner a ~ PInteger =>
  Term s b ->
  Term s a ->
  Term s (PXor b) ->
  Term s (PXor b)
pxortimes def x =
  pif
    ((pdiv # pto x # 2) #== 0)
    (pcon . PXor $ def)
