{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.PLam (
  plam,
  pinl,
) where

import Data.Kind (Type)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Plutarch.Internal (
  PType,
  S,
  Term,
  pgetConfig,
  plam',
  punsafeConstantInternal,
  tracingMode,
  (#->),
  pattern DoTracingAndBinds,
 )
import Plutarch.Internal.PrettyStack (prettyStack)
import Plutarch.Internal.Trace (ptrace)
import qualified PlutusCore as PLC

{- $plam
 Lambda abstraction.

 The 'PLamN' constraint allows
 currying to work as expected for any number of arguments.

 > idPPlutus' s => Term s (a #-> a)
 > id = plam (\x -> x)

 > constPPlutus' s => Term s (a #-> b :-> a)
 > const = plam (\x y -> x)
-}

mkstring :: Text.Text -> Term s a
mkstring x = punsafeConstantInternal $ PLC.someValue @Text.Text @PLC.DefaultUni x

class PLamN (a :: Type) (b :: PType) (s :: S) | a -> b, s b -> a where
  plam :: forall c. HasCallStack => (Term s c -> a) -> Term s (c #-> b)

instance {-# OVERLAPPABLE #-} (a' ~ Term s a) => PLamN a' a s where
  plam f =
    let cs = callStack
     in plam' \x -> pgetConfig \c -> case tracingMode c of
          DoTracingAndBinds -> ptrace (mkstring $ prettyStack "L" cs) $ f x
          _ -> f x

instance (a' ~ Term s a, PLamN b' b s) => PLamN (a' -> b') (a #-> b) s where
  plam f = withFrozenCallStack $ plam' $ \x -> plam (f x)

pinlPPlutus' s => Term s a -> (Term s a -> Term s b) -> Term s b
pinl v f = f v
