{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Other (
  printTerm,
  printScript,
  pto,
  pfix,
  POpaque (..),
  popaque,
) where

import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Plutarch.Internal (ClosedTerm, Config, Term, compile, phoistAcyclic, plam', punsafeCoerce, (#), (#->))
import Plutarch.Internal.PlutusType (
  PContravariant',
  PCovariant',
  PInner,
  PVariant',
  PlutusType,
  pcon',
  pmatch',
 )
import PlutusCore.Pretty (prettyPlcReadableDebug)
import PlutusLedgerApi.V1.Scripts (Script (Script))

-- | Prettyprint a compiled Script via the PLC pretty printer
printScript :: Script -> String
printScript = show . prettyPlcReadableDebug . (\(Script s) -> s)

{- | Prettyprint a Term via the PLC pretty printer

  TODO: Heavily improve. It's unreadable right now.

  We could convert the de Bruijn indices into names with:

  > show . prettyPlcReadableDef . (\(Right p) -> p) . Scripts.mkTermToEvaluate . compile $ term
-}
printTerm :: HasCallStack => Config -> ClosedTerm a -> String
printTerm config term = printScript $ either (error . T.unpack) id $ compile config term

{- |
  Safely coerce from a Term to it's 'PInner' representation.
-}
pto :: Term edsl a -> Term edsl (PInner a)
pto x = punsafeCoerce x

-- | An Arbitrary Term with an unknown type
data POpaque s = POpaque (Term s POpaque)

instance PlutusType POpaque where
  type PInner POpaque = POpaque
  type PCovariant' POpaque = ()
  type PContravariant' POpaque = ()
  type PVariant' POpaque = ()
  pcon' (POpaque x) = x
  pmatch' x f = f (POpaque x)

-- | Erase the type of a Term
popaquePPlutus' s => Term s a -> Term s POpaque
popaque = punsafeCoerce

{- |
  Fixpoint recursion. Used to encode recursive functions.

  Example:

  > iterateN' ::
  >  Term s (PInteger #-> (a #-> a) #-> a #-> a) ->
  >  Term s PInteger ->
  >  Term s (a #-> a) ->
  >  Term s a
  > iterateN' self n f x =
  >   pif (n #== 0) x (self # n - 1 #$ f x)
  >
  > iterateNPPlutus' s => Term s (PInteger #-> (a #-> a) #-> a #-> a)
  > iterateN = pfix #$ plam iterateN'
  >

  Further examples can be found in examples/Recursion.hs
-}
pfixPPlutus' s => Term s (((a #-> b) #-> a #-> b) #-> a #-> b)
pfix = phoistAcyclic $
  punsafeCoerce $
    plam' $ \f ->
      (plam' $ \(xPPlutus' s => Term s POpaque) -> f # (plam' $ \(vPPlutus' s => Term s POpaque) -> (punsafeCoerce x) # x # v))
        # punsafeCoerce (plam' $ \(xPPlutus' s => Term s POpaque) -> f # (plam' $ \(vPPlutus' s => Term s POpaque) -> (punsafeCoerce x) # x # v))
