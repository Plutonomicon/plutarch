{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch (
  (PI.:-->),
  PI.ClosedTerm,
  PI.compile,
  PI.Dig,
  PI.hashOpenTerm,
  PI.hashTerm,
  PI.papp,
  PI.pdelay,
  PI.PDelayed,
  PI.perror,
  PI.pforce,
  PI.phoistAcyclic,
  PI.plam',
  PI.plet,
  PI.Term,
  PI.TermCont (..),
  PI.S,
  PI.PType,
  PlutusType (..),
  printTerm,
  printScript,
  (#$),
  (#),
  pinl,
  PCon (..),
  PMatch (..),
  pto,
  pfix,
  POpaque (..),
  popaque,
  plam,
  DerivePNewtype (DerivePNewtype),
) where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Plutarch.Internal (ClosedTerm, PType, S, Term, compile, papp, phoistAcyclic, plam', punsafeCoerce, (:-->))
import qualified Plutarch.Internal as PI
import Plutus.V1.Ledger.Scripts (Script (Script))
import PlutusCore.Pretty (prettyPlcReadableDebug)

-- | Prettyprint a compiled Script via the PLC pretty printer
printScript :: Script -> String
printScript = show . prettyPlcReadableDebug . (\(Script s) -> s)

{- | Prettyprint a Term via the PLC pretty printer

  TODO: Heavily improve. It's unreadable right now.

  We could convert the de Bruijn indices into names with:

  > show . prettyPlcReadableDef . (\(Right p) -> p) . Scripts.mkTermToEvaluate . compile $ term
-}
printTerm :: ClosedTerm a -> String
printTerm term = printScript $ compile term

{- |
  High precedence infixl synonym of 'papp', to be used like
  function juxtaposition. e.g.:

  >>> f # x # y
  f x y
-}
(#) :: Term s (a :--> b) -> Term s a -> Term s b
(#) = papp

infixl 8 #

{- |
  Low precedence infixr synonym of 'papp', to be used like
  '$', in combination with '#'. e.g.:

  >>> f # x #$ g # y # z
  f x (g y z)
-}
(#$) :: Term s (a :--> b) -> Term s a -> Term s b
(#$) = papp

infixr 0 #$

{- $plam
 Lambda abstraction.

 The 'PLamN' constraint allows
 currying to work as expected for any number of arguments.

 > id :: Term s (a :--> a)
 > id = plam (\x -> x)

 > const :: Term s (a :--> b :-> a)
 > const = plam (\x y -> x)
-}

class PLamN (a :: Type) (b :: PType) (s :: S) | a -> b, s b -> a where
  plam :: forall c. (Term s c -> a) -> Term s (c :--> b)

instance (a' ~ Term s a) => PLamN a' a s where
  plam = plam'

instance {-# OVERLAPPING #-} (a' ~ Term s a, PLamN b' b s) => PLamN (a' -> b') (a :--> b) s where
  plam f = plam' $ \x -> plam (f x)

pinl :: Term s a -> (Term s a -> Term s b) -> Term s b
pinl v f = f v

{- |

  The 'PlutusType' class allows encoding Haskell data-types as plutus terms
  via constructors and destructors.

  A simple example, encoding a Sum type as an Enum via PInteger:

  > data AB (s :: S) = A | B
  >
  > instance PlutusType AB where
  >   type PInner AB _ = PInteger
  >
  >   pcon' A = 0
  >   pcon' B = 1
  >
  >   pmatch' x f =
  >     pif (x #== 0) (f A) (f B)
  >

  instead of using `pcon'` and `pmatch'` directly,
  use 'pcon' and 'pmatch', to hide the `PInner` type:

  > swap :: Term s AB -> Term s AB
  > swap x = pmatch x $ \case
  >  A -> pcon B
  >  B -> pcon A

  Further examples can be found in examples/PlutusType.hs
-}
class (PCon a, PMatch a) => PlutusType (a :: PType) where
  -- `b' :: k'` causes GHC to fail type checking at various places
  -- due to not being able to expand the type family.
  type PInner a (b' :: PType) :: PType
  pcon' :: forall s. a s -> forall b. Term s (PInner a b)
  pmatch' :: forall s c. (forall b. Term s (PInner a b)) -> (a s -> Term s c) -> Term s c

instance {-# OVERLAPPABLE #-} PlutusType a => PMatch a where
  pmatch x f = pmatch' (punsafeCoerce x) f

instance PlutusType a => PCon a where
  pcon x = punsafeCoerce (pcon' x)

class PCon a where
  -- | Construct a Plutarch Term via a Haskell datatype
  pcon :: a s -> Term s a

class PMatch a where
  -- | Pattern match over Plutarch Terms via a Haskell datatype
  pmatch :: Term s a -> (a s -> Term s b) -> Term s b

{- |
  Safely coerce from a Term to it's 'PInner' representation.
-}
pto :: Term s a -> (forall b. Term s (PInner a b))
pto x = punsafeCoerce x

-- | An Arbitrary Term with an unknown type
data POpaque s = POpaque (Term s POpaque)

instance PlutusType POpaque where
  type PInner POpaque _ = POpaque
  pcon' (POpaque x) = x
  pmatch' x f = f (POpaque x)

-- | Erase the type of a Term
popaque :: Term s a -> Term s POpaque
popaque = punsafeCoerce

{- |
  Fixpoint recursion. Used to encode recursive functions.

  Example:

  > iterateN' ::
  >  Term s (PInteger :--> (a :--> a) :--> a :--> a) ->
  >  Term s PInteger ->
  >  Term s (a :--> a) ->
  >  Term s a
  > iterateN' self n f x =
  >   pif (n #== 0) x (self # n - 1 #$ f x)
  >
  > iterateN :: Term s (PInteger :--> (a :--> a) :--> a :--> a)
  > iterateN = pfix #$ plam iterateN'
  >

  Further examples can be found in examples/Recursion.hs
-}
pfix :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
pfix = phoistAcyclic $
  punsafeCoerce $
    plam $ \f ->
      (plam $ \(x :: Term s POpaque) -> f # (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) # x # v))
        # punsafeCoerce (plam $ \(x :: Term s POpaque) -> f # (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) # x # v))

{- | Facilitates deriving 'PlutusType' and 'PIsData' for newtypes.

For any newtype represented as-
> newtype PFoo (s :: S) = PFoo (Term s PBar)

where 'PBar' has a 'PIsData' instance, you can derive 'PlutusType' and 'PIsData' using-
> deriving (PlutusType, PIsData) via (DerivePNewtype PFoo PBar)

This will make 'PFoo' simply be represnted as 'PBar' under the hood.
-}
newtype DerivePNewtype (a :: PType) (b :: PType) (s :: PI.S) = DerivePNewtype (a s)

instance (forall (s :: PI.S). Coercible (a s) (Term s b)) => PlutusType (DerivePNewtype a b) where
  type PInner (DerivePNewtype a b) _ = b
  pcon' (DerivePNewtype t) = ptypeInner t
  pmatch' x f = f . DerivePNewtype $ ptypeOuter x

instance Semigroup (Term s b) => Semigroup (Term s (DerivePNewtype a b)) where
  x <> y = punsafeFrom $ pto x <> pto y

instance Monoid (Term s b) => Monoid (Term s (DerivePNewtype a b)) where
  mempty = punsafeFrom $ mempty @(Term s b)

instance Num (Term s b) => Num (Term s (DerivePNewtype a b)) where
  x + y = punsafeFrom $ pto x + pto y
  x - y = punsafeFrom $ pto x - pto y
  x * y = punsafeFrom $ pto x * pto y
  abs x = punsafeFrom $ abs $ pto x
  negate x = punsafeFrom $ negate $ pto x
  signum x = punsafeFrom $ signum $ pto x
  fromInteger x = punsafeFrom $ fromInteger @(Term s b) x

ptypeInner :: forall (x :: PType) y s. Coercible (x s) (Term s y) => x s -> Term s y
ptypeInner = coerce

ptypeOuter :: forall (x :: PType) y s. Coercible (x s) (Term s y) => Term s y -> x s
ptypeOuter = coerce

punsafeFrom :: (forall b. Term s (PInner a b)) -> Term s a
punsafeFrom x = PI.punsafeCoerce x
