{-# LANGUAGE AllowAmbiguousTypes #-}

-- | TermCont-related adapters for Plutarch functions.
module Plutarch.Extra.TermCont (
  pletC,
  pmatchC,
  pletFieldsC,
  ptraceC,
  pguardC,
  pguardC',
  ptryFromC,
) where

import Plutarch.DataRepr (HRec, PDataFields, PFields)
import Plutarch.DataRepr.Internal.Field (
  BindFields,
  Bindings,
  BoundTerms,
 )
import Plutarch.Prelude
import Plutarch.Reducible (Reduce)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess))

-- | Like `plet` but works in a `TermCont` monad
pletCPPlutus' s => Term s a -> TermCont s (Term s a)
pletC = tcont . plet

-- | Like `pmatch` but works in a `TermCont` monad
pmatchC :: PlutusType a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

-- | Like `pletFields` but works in a `TermCont` monad.
pletFieldsC ::
  forall fs a s b ps bs.
  ( PDataFields a
  , ps ~ PFields a
  , bs ~ Bindings ps fs
  , BindFields ps bs
  ) =>
  Term s a ->
  TermCont @b s (HRec (BoundTerms ps bs s))
pletFieldsC x = tcont $ pletFields @fs x

{- | Like `ptrace` but works in a `TermCont` monad.

=== Example ===

@
fooPPlutus' s => Term s PUnit
foo = unTermCont $ do
  ptraceC "returning unit!"
  pure $ pconstant ()
@
-}
ptraceCPPlutus' s => Term s PString -> TermCont s ()
ptraceC s = tcont $ \f -> ptrace s (f ())

{- | Trace a message and raise error if 'cond' is false. Otherwise, continue.

=== Example ===

@
onlyAllow42PPlutus' s => Term s (PInteger #-> PUnit)
onlyAllow42 = plam $ \i -> unTermCont $ do
  pguardC "expected 42" $ i #== 42
  pure $ pconstant ()
@
-}
pguardCPPlutus' s => Term s PString -> Term s PBool -> TermCont s ()
pguardC s cond = tcont $ \f -> pif cond (f ()) $ ptraceError s

{- | Stop computation and return given term if 'cond' is false. Otherwise, continue.

=== Example ===

@
is42PPlutus' s => Term s (PInteger #-> PBool)
is42 = plam $ \i -> unTermCont $ do
  pguardC "expected 42" (pconstant False) $ i #== 42
  pure $ pconstant True
@
-}
pguardC'PPlutus' s => Term s a -> Term s PBool -> TermCont @a s ()
pguardC' r cond = tcont $ \f -> pif cond (f ()) r

-- | 'TermCont' producing version of 'ptryFrom'.
ptryFromC :: forall b r a s. PTryFrom a b => Term s a -> TermCont @r s (Term s b, Reduce (PTryFromExcess a b s))
ptryFromC = tcont . ptryFrom
