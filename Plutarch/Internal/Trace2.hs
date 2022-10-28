module Plutarch.Internal.Trace2 where

import Data.Kind (Constraint)
import Plutarch.Core
import Plutarch.String2
--   ptrace,
--   ptrace',
-- ) where

-- import Plutarch.Internal (
--   Term,
--   pdelay,
--   pforce,
--   pgetConfig,
--   phoistAcyclic,
--   tracingMode,
--   (#),
--   pattern NoTracing,
--   type (#->),
--  )
-- import {-# SOURCE #-} Plutarch.String (PString)

-- import Plutarch.Unsafe (punsafeBuiltin)
-- import qualified PlutusCore as PLC

type  PTrace :: PDSLKind -> Constraint
class PTrace edsl where
  ptrace' :: Term edsl (PString #-> a #-> a)

-- | Trace the given message before evaluating the argument.
ptrace 
  :: Term edsl PString 
  -> Term edsl a 
  -> Term edsl a
ptrace str a = pgetConfig \c -> case tracingMode c of
  -- NoTracing -> a
  -- _ -> pforce $ ptrace' # str # pdelay a
