{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Plutarch.TermCont (
  hashOpenTerm,
  TermCont (TermCont),
  runTermCont,
  unTermCont,
  tcont,
) where

import Data.Kind (Type)
import Data.String (fromString)
import Control.Monad.Cont (ContT(..), MonadCont(..))
import Plutarch.Internal (
  Dig,
  PType,
  S,
  Term (Term),
  asRawTerm,
  getTerm,
  hashRawTerm,
  pgetConfig,
  tracingMode,
  pattern DetTracing,
 )
import Plutarch.Trace (ptraceError)

newtype TermCont :: forall (r :: PType). S -> Type -> Type where
  TermCont :: forall r s a. {runTermCont :: ((a -> Term s r) -> Term s r)} -> TermCont @r s a
  deriving (Functor, Applicative, Monad)
  via ContT (r :: S -> Type) (Term s)

unTermCont :: TermCont @a s (Term s a) -> Term s a
unTermCont t = runTermCont t id

instance MonadCont (TermCont @r s) where
  callCC :: ((a -> TermCont @r s b) -> TermCont @r s a) -> TermCont @r s a
  callCC cont = TermCont \c -> runTermCont (cont \x -> TermCont \ _ -> c x) c

instance MonadFail (TermCont s) where
  fail s = TermCont $ \_ ->
    pgetConfig \c -> case tracingMode c of
      DetTracing -> ptraceError "Pattern matching failure in TermCont"
      _ -> ptraceError $ fromString s

tcont :: ((a -> Term s r) -> Term s r) -> TermCont @r s a
tcont = TermCont

hashOpenTermPPlutus' s => Term s a -> TermCont s Dig
hashOpenTerm x = TermCont $ \f -> Term $ \i -> do
  y <- asRawTerm x i
  asRawTerm (f . hashRawTerm . getTerm $ y) i
