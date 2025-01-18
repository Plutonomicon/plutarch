{-# LANGUAGE ImpredicativeTypes #-}

module Plutarch.Evaluate (
  E.evalScript,
  E.evalScriptHuge,
  E.evalScript',
  E.evalScriptUnlimited,
  E.EvalError,
  evalTerm,
  evalTerm',
  unsafeEvalTerm,
  applyArguments,
) where

import Control.Lens.Combinators (over)
import Data.Text (Text)
import Plutarch.Internal.Evaluate qualified as E
import Plutarch.Internal.Term (
  ClosedTerm,
  Config,
  RawTerm (RCompiled),
  Term (..),
  TermResult (TermResult),
  compile,
 )
import Plutarch.Script (Script (Script))
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import PlutusCore.MkPlc (mkConstant, mkIterAppNoAnn)
import PlutusLedgerApi.Common (Data)
import UntypedPlutusCore qualified as UPLC

-- | Compile and evaluate term.
evalTerm ::
  Config ->
  ClosedTerm a ->
  Either Text (Either E.EvalError (ClosedTerm a), ExBudget, [Text])
evalTerm config term =
  case compile config term of
    Right script ->
      let (s, b, t) = E.evalScriptHuge script
       in Right (fromScript <$> s, b, t)
    Left a -> Left a
  where
    fromScript :: Script -> ClosedTerm a
    fromScript (Script script) =
      Term $ const $ pure $ TermResult (RCompiled $ UPLC._progTerm script) []

-- | Same as `evalTerm` but without error handling
evalTerm' :: Config -> ClosedTerm a -> ClosedTerm a
evalTerm' config term =
  case evalTerm config term of
    Right (Right t, _, _) -> t
    Left err -> error $ "evalTerm' failed: " <> show err
    _ -> error "evalTerm' failed"

{- | Compile and evaluate a ClosedTerm
Useful for pre-evaluating terms so that they can be used as constants in
an onchain script. Consider the following:
 _________________________________________________________________________
     term :: Term _ PInteger
     term = unsafeEvalTerm NoTracing foo

     foo :: Term s PInteger
     foo = (pconstant 1 #+ pconstant 5) #* pconstant 3

     bar :: Term s (PInteger :--> PInteger)
     bar = plam \x ->
       x + foo

     bar2 :: Term s (PInteger :--> PInteger)
     bar2 = plam \x ->
       x + term


    PI.compile PI.NoTracing bar
    Right (Script {unScript = Program {_progAnn = (), _progVer = Version {_versionMajor = 1, _versionMinor = 0, _versionPatch = 0}, _progTerm = LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (Apply () (Builtin () AddInteger) (Var () (DeBruijn {dbnIndex = 1}))) (Apply () (Apply () (Builtin () MultiplyInteger) (Apply () (Apply () (Builtin () AddInteger) (Constant () (Some (ValueOf DefaultUniInteger 1)))) (Constant () (Some (ValueOf DefaultUniInteger 5))))) (Constant () (Some (ValueOf DefaultUniInteger 3)))))}})
    PI.compile PI.NoTracing bar2
    Right (Script {unScript = Program {_progAnn = (), _progVer = Version {_versionMajor = 1, _versionMinor = 0, _versionPatch = 0}, _progTerm = LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (Apply () (Builtin () AddInteger) (Var () (DeBruijn {dbnIndex = 1}))) (Constant () (Some (ValueOf DefaultUniInteger 18))))}})
 _________________________________________________________________________

In bar, foo is an unevaluated term and thus must be evaluated. In bar2, foo has been
pre-evaluated with `unsafeEvalTerm` and thus appears as a constant.

Error if the compilation or evaluation fails.
-}
unsafeEvalTerm :: Config -> ClosedTerm a -> ClosedTerm a
unsafeEvalTerm c t = extractResult $ evalTerm c t
  where
    extractResult :: Either Text (Either E.EvalError (ClosedTerm a), ExBudget, [Text]) -> ClosedTerm a
    extractResult (Right (Right term, _, _)) = term
    extractResult _ = error "unsafeEvalTerm: failed to evaluate or compile the term."

{- | Given a compiled 'Script' representing a function that takes arguments, and
a list of those 'Data'-encoded arguments, produce a new script with those
arguments applied.

@since 1.8.1
-}
applyArguments :: Script -> [Data] -> Script
applyArguments (Script p) args =
  let termArgs = mkConstant () <$> args
      applied t = mkIterAppNoAnn t termArgs
   in Script $ over UPLC.progTerm applied p
