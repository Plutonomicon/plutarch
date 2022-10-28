module Plutarch.Monadic ((>>=), (>>), fail) where

import Prelude hiding (fail, (>>), (>>=))

import Data.String (fromString)
import Plutarch (pgetConfig, tracingMode, pattern DetTracing)
import Plutarch.Prelude

{- | Bind function used within do syntax.

Enables elegant usage of 'pmatch' and similar.
@P.do { y <- x ; z }@ is equivalent to @x $ \y -> z@.

@
  import qualified Plutarch.Monadic as P

  fPPlutus' s => Term s (PTxInfo #-> PBuiltinList (PAsData PTxInInfo))
  f = plam $ \x -> P.do
    PTxInfo txInfoFields <- pmatch x
    pfromData $ pdhead # txInfoFields
@
-}
(>>=) :: (x -> Term s a) -> x -> Term s a
(>>=) = id

{- | Forgetful bind function used within do syntax.

Enables elegant usage of 'ptrace' and similar.
@P.do { x ; y }@ is equivalent to @x y@.

@
  import qualified Plutarch.Monadic as P

  P.do
    ptrace "yielding unit"
    pconstant ()
@
-}
(>>) :: (x -> Term s a) -> x -> Term s a
(>>) = id

{- | Implicitly invoked upon pattern match failure within do syntax.

@
  import qualified Plutarch.Monadic as P

  P.do
    -- calls 'P.fail', traces an error message, and invokes 'perror'.
    PTrue <- pconstant False
@
-}
fail :: String -> Term s a
fail s =
  pgetConfig \c -> case tracingMode c of
    DetTracing -> ptraceError "Pattern matching failure in QualifiedDo syntax"
    _ -> ptraceError $ fromString s
