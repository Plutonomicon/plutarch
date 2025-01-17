{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Internal.Numeric (
  -- * Types
  PPositive,
  Positive,
  PNatural,

  -- * Type classes
  PAdditiveSemigroup (..),
  PAdditiveMonoid (..),
  PAdditiveGroup (..),
  PMultiplicativeSemigroup (..),
  PMultiplicativeMonoid (..),
  PRing (..),
  PIntegralDomain (..),

  -- * Functions
  ptryPositive,
  ppositive,
  ptryNatural,
  pnatural,
  ppositiveToNatural,
  pnaturalToPositiveCPS,
  pbySquaringDefault,
  pdiv,
  pmod,
  pquot,
  prem,
) where

import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Numeric.Natural (Natural)
import Plutarch.Builtin.BLS (
  PBuiltinBLS12_381_G1_Element,
  PBuiltinBLS12_381_G2_Element,
  PBuiltinBLS12_381_MlResult,
  pbls12_381_G1_add,
  pbls12_381_G1_compressed_zero,
  pbls12_381_G1_neg,
  pbls12_381_G1_scalarMul,
  pbls12_381_G1_uncompress,
  pbls12_381_G2_add,
  pbls12_381_G2_compressed_zero,
  pbls12_381_G2_neg,
  pbls12_381_G2_scalarMul,
  pbls12_381_G2_uncompress,
  pbls12_381_mulMlResult,
 )
import Plutarch.Builtin.Bool (pcond, pif)
import Plutarch.Builtin.Integer (
  PInteger,
  paddInteger,
  pconstantInteger,
  pmultiplyInteger,
  pquotientInteger,
  premainderInteger,
  psubtractInteger,
 )
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Fix (pfix)
import Plutarch.Internal.IsData (PIsData)
import Plutarch.Internal.Lift (
  DeriveNewtypePLiftable,
  PLiftable (
    AsHaskell,
    PlutusRepr,
    fromPlutarch,
    fromPlutarchRepr,
    toPlutarch,
    toPlutarchRepr
  ),
  PLifted (PLifted),
  punsafeCoercePLifted,
 )
import Plutarch.Internal.Ord (POrd ((#<=)))
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  DeriveAsNewtype (DeriveAsNewtype),
  PlutusType (PInner),
  pcon,
 )
import Plutarch.Internal.Term (
  S,
  Term,
  perror,
  phoistAcyclic,
  plet,
  punsafeBuiltin,
  punsafeCoerce,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Internal.Trace (ptraceInfo)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Unsafe (punsafeDowncast)
import PlutusCore qualified as PLC
import Prettyprinter (Pretty)
import Test.QuickCheck (
  Arbitrary,
  CoArbitrary,
  Function,
  functionMap,
 )
import Test.QuickCheck qualified as QuickCheck

-- | @since WIP
newtype PPositive (s :: S) = PPositive (Term s PInteger)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      SOP.Generic
    , -- | @since WIP
      PIsData
    , -- | @since WIP
      PEq
    , -- | @since WIP
      POrd
    )
  deriving
    ( -- | @since WIP
      PlutusType
    )
    via (DeriveAsNewtype PPositive)

-- | @since WIP
deriving via
  DeriveNewtypePLiftable PPositive PInteger Positive
  instance
    PLiftable PPositive

-- | @since WIP
newtype Positive = UnsafeMkPositive {getPositive :: Integer}
  deriving stock
    ( -- | @since WIP
      Show
    , -- | @since WIP
      Eq
    , -- | @since WIP
      Ord
    )
  deriving
    ( -- | @since WIP
      Arbitrary
    )
    via QuickCheck.Positive Integer
  deriving
    ( -- | @since WIP
      CoArbitrary
    , -- | @since WIP
      Pretty
    )
    via Integer

-- | @since WIP
instance Function Positive where
  {-# INLINEABLE function #-}
  function = functionMap @Integer coerce coerce

-- | @since WIP
newtype PNatural (s :: S) = PNatural (Term s PInteger)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      SOP.Generic
    , -- | @since WIP
      PIsData
    , -- | @since WIP
      PEq
    , -- | @since WIP
      POrd
    )
  deriving (PlutusType) via DeriveAsNewtype PNatural

-- | @since WIP
instance PLiftable PNatural where
  type AsHaskell PNatural = Natural
  type PlutusRepr PNatural = Integer
  {-# INLINEABLE toPlutarchRepr #-}
  toPlutarchRepr = fromIntegral
  {-# INLINEABLE toPlutarch #-}
  toPlutarch = punsafeCoercePLifted . toPlutarch @PInteger . fromIntegral
  {-# INLINEABLE fromPlutarchRepr #-}
  fromPlutarchRepr x =
    if x < 0
      then Nothing
      else Just . fromIntegral $ x
  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch t = fromIntegral <$> fromPlutarch @PInteger (punsafeCoercePLifted t)

{- | The addition operation, and the notion of scaling by a positive.

= Laws

1. @x #+ y@ @=@ @y #+ x@ (commutativity of @#+@)
2. @x #+ (y #+ z)@ @=@ @(x #+ y) #+ z@ (associativity of @#+@)

If you define a custom @pscalePositive@, ensure the following also hold:

3. @pscalePositive x pone@ @=@ @x@
4. @(pscalePositive x n) #+ (pscalePositive x m)@ @=@
   @pscalePositive x (n #+ m)@
5. @pscalePositive (pscalePositive x n) m@ @=@
   @pscalePositive x (n #* m)@

The default implementation ensures these laws are satisfied.

@since WIP
-}
class PAdditiveSemigroup (a :: S -> Type) where
  (#+) :: forall (s :: S). Term s a -> Term s a -> Term s a
  default (#+) ::
    forall (s :: S).
    PAdditiveSemigroup (PInner a) =>
    Term s a ->
    Term s a ->
    Term s a
  x #+ y = punsafeDowncast $ pto x #+ pto y
  {-# INLINEABLE pscalePositive #-}

  -- | This defaults to exponentiation-by-squaring, which in general is the best
  -- we can do.
  pscalePositive ::
    forall (s :: S).
    Term s a ->
    Term s PPositive ->
    Term s a
  pscalePositive = pbySquaringDefault (#+)

-- | @since WIP
infix 6 #+

-- | @since WIP
instance PAdditiveSemigroup PPositive where
  {-# INLINEABLE (#+) #-}
  x #+ y = punsafeCoerce $ paddInteger # pto x # pto y
  {-# INLINEABLE pscalePositive #-}
  pscalePositive b e = b #* e

-- | @since WIP
instance PAdditiveSemigroup PNatural where
  {-# INLINEABLE (#+) #-}
  x #+ y = pcon . PNatural $ paddInteger # pto x # pto y
  {-# INLINEABLE pscalePositive #-}
  pscalePositive b e = b #* punsafeCoerce e

-- | @since WIP
instance PAdditiveSemigroup PInteger where
  {-# INLINEABLE (#+) #-}
  x #+ y = paddInteger # x # y
  {-# INLINEABLE pscalePositive #-}
  pscalePositive b e = b #* pto e

-- | @since WIP
instance PAdditiveSemigroup PBuiltinBLS12_381_G1_Element where
  {-# INLINEABLE (#+) #-}
  x #+ y = pbls12_381_G1_add # x # y
  {-# INLINEABLE pscalePositive #-}
  pscalePositive x p = pbls12_381_G1_scalarMul # pto p # x

-- | @since WIP
instance PAdditiveSemigroup PBuiltinBLS12_381_G2_Element where
  {-# INLINEABLE (#+) #-}
  x #+ y = pbls12_381_G2_add # x # y
  {-# INLINEABLE pscalePositive #-}
  pscalePositive x p = pbls12_381_G2_scalarMul # pto p # x

{- | The notion of zero, as well as a way to scale by naturals.

= Laws

1. @pzero #+ x@ @=@ @x@ (@pzero@ is the identity of @#+@)
2. @pscalePositive pzero n@ @=@
   @pzero@ (@pzero@ does not scale up)

If you define 'pscaleNatural', ensure the following as well:

3. @pscaleNatural x (ppositiveToNatural # p)@ @=@
   @pscalePositive x p@
4. @pscaleNatural x pzero@ @=@ @pzero@

The default implementation of 'pscaleNatural' ensures these laws hold.

@since WIP
-}
class PAdditiveSemigroup a => PAdditiveMonoid (a :: S -> Type) where
  pzero :: forall (s :: S). Term s a
  {-# INLINEABLE pscaleNatural #-}
  pscaleNatural ::
    forall (s :: S).
    Term s a ->
    Term s PNatural ->
    Term s a
  pscaleNatural x n =
    plet n $ \n' ->
      pif
        (n' #== pzero)
        pzero
        (pscalePositive x (punsafeCoerce n'))

-- | @since WIP
instance PAdditiveMonoid PInteger where
  {-# INLINEABLE pzero #-}
  pzero = pconstantInteger 0
  {-# INLINEABLE pscaleNatural #-}
  pscaleNatural i n = i #* punsafeCoerce n

-- | @since WIP
instance PAdditiveMonoid PNatural where
  {-# INLINEABLE pzero #-}
  pzero = pcon . PNatural . pconstantInteger $ 0
  {-# INLINEABLE pscaleNatural #-}
  pscaleNatural n1 n2 = n1 #* n2

-- | @since WIP
instance PAdditiveMonoid PBuiltinBLS12_381_G1_Element where
  {-# INLINEABLE pzero #-}
  pzero = pbls12_381_G1_uncompress # pbls12_381_G1_compressed_zero
  {-# INLINEABLE pscaleNatural #-}
  pscaleNatural x n = pbls12_381_G1_scalarMul # pto n # x

-- | @since WIP
instance PAdditiveMonoid PBuiltinBLS12_381_G2_Element where
  {-# INLINEABLE pzero #-}
  pzero = pbls12_381_G2_uncompress # pbls12_381_G2_compressed_zero
  {-# INLINEABLE pscaleNatural #-}
  pscaleNatural x n = pbls12_381_G2_scalarMul # pto n # x

{- | The notion of additive inverses, and the subtraction operation.

= Laws

If you define @pnegate@, the following laws must hold:

1. @(pnegate # x) #+ x@ @=@ @pzero@ (@pnegate@ is an additive inverse)
2. @pnegate #$ pnegate # x@ @=@ @x@ (@pnegate@ is self-inverting)

If you define @#-@, the following law must hold:

3. @x #- x@ @=@ @pzero@

Additionally, the following \'consistency laws\' must hold. Default
implementations of both @pnegate@ and @#-@ uphold these.

4. @pnegate # x@ @=@ @pzero #- x@
5. @x #- y@ @=@ @x #+ (pnegate # y)@

Lastly, if you define a custom @pscaleInteger@, the following laws
must hold:

6. @pscaleInteger x pzero@ @=@ @pzero@
7. @pscaleInteger x (pnegate # y)@ @=@
   @pnegate # (pscaleInteger x y)@

@since WIP
-}
class PAdditiveMonoid a => PAdditiveGroup (a :: S -> Type) where
  {-# MINIMAL pnegate | (#-) #-}
  {-# INLINEABLE pnegate #-}
  pnegate :: forall (s :: S). Term s (a :--> a)
  pnegate = phoistAcyclic $ plam $ \x -> pzero #- x
  {-# INLINEABLE (#-) #-}
  (#-) :: forall (s :: S). Term s a -> Term s a -> Term s a
  x' #- y' = inner # x' # y'
    where
      inner :: forall (s' :: S). Term s' (a :--> a :--> a)
      inner = phoistAcyclic $ plam $ \x y ->
        x #+ (pnegate # y)
  {-# INLINEABLE pscaleInteger #-}
  pscaleInteger ::
    forall (s :: S).
    Term s a ->
    Term s PInteger ->
    Term s a
  pscaleInteger b e =
    plet e $ \e' ->
      pif
        (e' #== pzero)
        pzero
        ( pif
            (e' #<= pzero)
            (pnegate # pscalePositive b (punsafeDowncast (pnegate # e')))
            (pscalePositive b (punsafeDowncast e'))
        )

-- | @since WIP
infix 6 #-

-- | @since WIP
instance PAdditiveGroup PInteger where
  {-# INLINEABLE (#-) #-}
  x #- y = psubtractInteger # x # y
  {-# INLINEABLE pscaleInteger #-}
  pscaleInteger = (#*)

-- | @since WIP
instance PAdditiveGroup PBuiltinBLS12_381_G1_Element where
  {-# INLINEABLE pnegate #-}
  pnegate = pbls12_381_G1_neg
  {-# INLINEABLE pscaleInteger #-}
  pscaleInteger b e = pbls12_381_G1_scalarMul # e # b

-- | @since WIP
instance PAdditiveGroup PBuiltinBLS12_381_G2_Element where
  {-# INLINEABLE pnegate #-}
  pnegate = pbls12_381_G2_neg
  {-# INLINEABLE pscaleInteger #-}
  pscaleInteger b e = pbls12_381_G2_scalarMul # e # b

{- | The multiplication operation.

= Laws

1. @x #* (y #* z)@ @=@ @(x #* y) #* z@ (associativity of @#*@)

If you define a custom @ppowPositive@, ensure the following also hold:

3. @ppowPositive x pone@ @=@ @x@
4. @(ppowPositive x n) #* (ppowPositive x m)@ @=@
   @ppowPositive x (n #+ m)@
5. @ppowPositive (ppowPositive x n) m@ @=@
   @ppowPositive x (n #* m)@

The default implementation ensures these laws are satisfied.

= Note

Unlike 'PAdditiveSemigroup', the multiplication operation doesn't need to be
commutative. Currently, all Plutarch-provided instances are, but this need
not be true for other instances.

@since WIP
-}
class PMultiplicativeSemigroup (a :: S -> Type) where
  {-# INLINEABLE (#*) #-}
  (#*) :: forall (s :: S). Term s a -> Term s a -> Term s a
  default (#*) ::
    forall (s :: S).
    PMultiplicativeSemigroup (PInner a) =>
    Term s a ->
    Term s a ->
    Term s a
  x #* y = punsafeDowncast $ pto x #* pto y
  {-# INLINEABLE ppowPositive #-}
  ppowPositive ::
    forall (s :: S).
    Term s a ->
    Term s PPositive ->
    Term s a
  ppowPositive = pbySquaringDefault (#*)

-- | @since WIP
infix 6 #*

-- | @since WIP
instance PMultiplicativeSemigroup PPositive where
  {-# INLINEABLE (#*) #-}
  x #* y = punsafeCoerce $ pmultiplyInteger # pto x # pto y

-- | @since WIP
instance PMultiplicativeSemigroup PNatural where
  {-# INLINEABLE (#*) #-}
  x #* y = pcon . PNatural $ pmultiplyInteger # pto x # pto y

-- | @since WIP
instance PMultiplicativeSemigroup PInteger where
  {-# INLINEABLE (#*) #-}
  x #* y = pmultiplyInteger # x # y

-- | @since WIP
instance PMultiplicativeSemigroup PBuiltinBLS12_381_MlResult where
  {-# INLINEABLE (#*) #-}
  x #* y = pbls12_381_mulMlResult # x # y

{- | The notion of one (multiplicative identity), and exponentiation by
 - naturals.

= Laws

1. @pone #* x@ @=@ @x@ (@pone@ is the left identity of @#*@)
2. @x #* pone@ @=@ @x@ (@pone@ is the right identity of @#*@)
3. @ppowPositive pone p@ @=@ @pone@ (@pone@ does not scale up)

If you define 'ppowNatural', ensure the following as well:

4. @ppowNatural x (ppositiveToNatural # p)@ @=@
   @ppowPositive x p@
5. @ppowNatural x pzero@ @=@ @pone@

@since WIP
-}
class PMultiplicativeSemigroup a => PMultiplicativeMonoid (a :: S -> Type) where
  pone :: forall (s :: S). Term s a
  {-# INLINEABLE ppowNatural #-}
  ppowNatural ::
    forall (s :: S).
    Term s a ->
    Term s PNatural ->
    Term s a
  ppowNatural x n = plet n $ \n' ->
    pif
      (n' #== pzero)
      pone
      (ppowPositive x (pcon (PPositive $ pto n')))

-- | @since WIP
instance PMultiplicativeMonoid PPositive where
  {-# INLINEABLE pone #-}
  pone = punsafeCoerce $ pconstantInteger 1

-- | @since WIP
instance PMultiplicativeMonoid PNatural where
  {-# INLINEABLE pone #-}
  pone = pcon . PNatural $ pconstantInteger 1

-- | @since WIP
instance PMultiplicativeMonoid PInteger where
  {-# INLINEABLE pone #-}
  pone = pconstantInteger 1

{- | Partial version of 'ppositive'. Errors if argument is not positive.

@since WIP
-}
ptryPositive :: forall (s :: S). Term s (PInteger :--> PPositive)
ptryPositive = phoistAcyclic $
  plam $ \i ->
    pif
      (i #<= pconstantInteger 0)
      (ptraceInfo "ptryPositive: building with non positive" perror)
      (punsafeCoerce i)

-- | Build a 'PPositive' from a 'PInteger'. Yields 'PNothing' if argument is not positive.
ppositive :: Term s (PInteger :--> PMaybe PPositive)
ppositive = phoistAcyclic $
  plam $ \i ->
    pif
      (i #<= pconstantInteger 0)
      (pcon PNothing)
      $ pcon . PJust . pcon
      $ PPositive i

{- | A default implementation of exponentiation-by-squaring with a
strictly-positive exponent.

= Important note

This implementation assumes that the operation argument is associative.

@since WIP
-}
pbySquaringDefault ::
  forall (a :: S -> Type) (s :: S).
  (forall (s' :: S). Term s' a -> Term s' a -> Term s' a) ->
  Term s a ->
  Term s PPositive ->
  Term s a
pbySquaringDefault f b e = go # b # e
  where
    go :: forall (s'' :: S). Term s'' (a :--> PPositive :--> a)
    go = phoistAcyclic $ pfix #$ plam $ \self b e -> plet e $ \e' ->
      pif
        (pto e' #== pconstantInteger 1)
        b
        ( plet (self # b #$ punsafeDowncast (pquotientInteger # pto e' # pconstantInteger 2)) $ \below ->
            plet (f below below) $ \res ->
              pif
                ((premainderInteger # pto e' # pconstantInteger 2) #== pconstantInteger 1)
                (f b res)
                res
        )

{- | = Laws

1. @pfromInteger 0@ @=@ @pzero@
2. @pfromInteger 1@ @=@ @pone@
3. @pfromInteger (x + y)@ @=@ @pfromInteger x #+ pfromInteger y@
4. @pfromInteger (x * y)@ @=@ @pfromInteger x #* pfromInteger y@

Additionally, the following \'interaction laws\' must hold between the
instances of 'PAdditiveGroup' and 'PMultiplicativeMonoid' for @a@:

5. @x #* (y #+ z)@ @=@ @(x #* y) #+ (x #* z)@ (@#*@ left-distributes over
   @#+@)
6. @(y #+ z) #* x@ @=@ @(y #* x) #+ (z #* x)@ (@#*@ right-distributes over
   @#+@)

@since WIP
-}
class
  (PAdditiveGroup a, PMultiplicativeMonoid a) =>
  PRing (a :: S -> Type)
  where
  pfromInteger :: forall (s :: S). Integer -> Term s a
  default pfromInteger :: forall (s :: S). PRing (PInner a) => Integer -> Term s a
  pfromInteger x = punsafeDowncast $ pfromInteger x

-- | @since WIP
instance PRing PInteger where
  {-# INLINEABLE pfromInteger #-}
  pfromInteger = pconstantInteger

{- | = Laws

= Pedantry note

Technically, the requirements here are too strong: we demand an /ordered/
ring, which integral domains don't necessarily have to be. However, in our
case, our hand is forced by expected semantics: in abstract algebra, both the
absolute value and the signum are real numbers (which are always totally
ordered) but in our case, both must be elements of the integral domain
itself. Thus, in order for the laws to make any sense, we have to ensure a
total order on the integral domain. Since all of our integral domains are
\'at least as big\' as the integers, this doesn't pose a huge problem.

@since WIP
-}
class (PRing a, POrd a) => PIntegralDomain (a :: S -> Type) where
  {-# INLINEABLE psignum #-}
  psignum :: forall (s :: S). Term s (a :--> a)
  default psignum :: forall (s :: S). Term s (a :--> a)
  psignum = phoistAcyclic $ plam $ \x ->
    pcond
      [ (x #== pzero, pzero)
      , (x #<= pzero, pnegate # pone)
      ]
      pone
  {-# INLINEABLE pabs #-}
  pabs :: forall (s :: S). Term s (a :--> a)
  default pabs :: forall (s :: S). Term s (a :--> a)
  pabs = phoistAcyclic $ plam $ \x ->
    pif
      (x #<= pzero)
      (pnegate # x)
      x

-- | @since WIP
instance PIntegralDomain PInteger where
  {-# INLINEABLE psignum #-}
  psignum = phoistAcyclic $ plam $ \x ->
    pcond
      [ (x #== pconstantInteger 0, pconstantInteger 0)
      , (x #<= pconstantInteger 0, pconstantInteger (-1))
      ]
      (pconstantInteger 1)

-- orphan instance, but only visibly orphan when importing internal modules
instance PIntegralDomain a => Num (Term s a) where
  {-# INLINEABLE (+) #-}
  (+) = (#+)
  {-# INLINEABLE (-) #-}
  (-) = (#-)
  {-# INLINEABLE (*) #-}
  (*) = (#*)
  {-# INLINEABLE abs #-}
  abs x = pabs # x
  {-# INLINEABLE negate #-}
  negate x = pnegate # x
  {-# INLINEABLE signum #-}
  signum x = psignum # x
  {-# INLINEABLE fromInteger #-}
  fromInteger = pfromInteger

-- | @since WIP
pdiv :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pdiv = punsafeBuiltin PLC.DivideInteger

-- | @since WIP
pmod :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pmod = punsafeBuiltin PLC.ModInteger

-- | @since WIP
pquot :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pquot = punsafeBuiltin PLC.QuotientInteger

-- | @since WIP
prem :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
prem = punsafeBuiltin PLC.RemainderInteger

{- | Specialized form of @pmaybe@ for 'PNatural'. Given a default, and a way to
turn a 'PPositive' into an answer, produce the default when given 'pzero',
and apply the function otherwise.

@since WIP
-}
pnaturalToPositiveCPS ::
  forall (a :: S -> Type) (s :: S).
  Term s a ->
  (Term s PPositive -> Term s a) ->
  Term s PNatural ->
  Term s a
pnaturalToPositiveCPS def f n = plet n $ \n' ->
  pif
    (n' #== pzero)
    def
    (f . punsafeCoerce $ n)

{- | Partial version of 'pnatural'. Errors if argument is negative.

@since WIP
-}
ptryNatural :: forall (s :: S). Term s (PInteger :--> PNatural)
ptryNatural = phoistAcyclic $ plam $ \i ->
  pif
    (i #<= pconstantInteger (-1))
    (ptraceInfo "ptryNatural: building with negative" perror)
    (pcon . PNatural $ i)

{- | Build a 'PNatural' from a 'PInteger'. Yields 'PNothing' if given a negative
value.

@since WIP
-}
pnatural :: forall (s :: S). Term s (PInteger :--> PMaybe PNatural)
pnatural = phoistAcyclic $ plam $ \i ->
  pif
    (i #<= pconstantInteger (-1))
    (pcon PNothing)
    (pcon . PJust . pcon . PNatural $ i)

{- | \'Relax\' a 'PPositive' to 'PNatural'. This uses 'punsafeCoerce'
underneath, but because any positive is also a natural, is safe.

@since WIP
-}
ppositiveToNatural :: forall (s :: S). Term s (PPositive :--> PNatural)
ppositiveToNatural = phoistAcyclic $ plam $ \x -> punsafeCoerce x
