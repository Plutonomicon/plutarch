module Plutarch.Trace2 where

-- module Plutarch.Trace (
--   ptrace,
--   ptraceShowId,
--   ptraceIfTrue,
--   ptraceIfFalse,
--   ptraceError,
-- ) where

import Plutarch.Bool2
import Plutarch.Core
import Plutarch.PType
-- import Plutarch.Internal.Trace (ptrace, ptrace')
-- import Plutarch.Show (PShow, pshow)
-- import Plutarch.String (PString)

-- -- | Like Haskell's `traceShowId` but for Plutarch
-- ptraceShowId :: PShow a => Term s a -> Term s a
-- ptraceShowId a = pgetConfig \c -> case tracingMode c of
--   NoTracing -> a
--   _ -> ptrace (pshow a) a

-- -- | Trace the given message and terminate evaluation with a 'perror'.
-- ptraceErrorPPlutus' s => Term s PString -> Term s a
-- ptraceError = flip ptrace perror

-- -- | Trace the given message if the argument evaluates to true.
-- ptraceIfTruePPlutus' s => Term s PString -> Term s PBool -> Term s PBool
-- ptraceIfTrue s a' = pgetConfig \c -> case tracingMode c of
--   NoTracing -> a'
--   _ -> plet a' $ \a -> pif a (ptrace' # s # a) a

-- -- | Trace the given message if the argument evaluates to False.
-- ptraceIfFalsePPlutus' s => Term s PString -> Term s PBool -> Term s PBool
-- ptraceIfFalse s a' = pgetConfig \c -> case tracingMode c of
--   NoTracing -> a'
--   _ -> plet a' $ \a -> pif a a (ptrace' # s # a)
