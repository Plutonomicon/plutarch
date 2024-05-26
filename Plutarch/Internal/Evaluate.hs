{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Plutarch.Internal.Evaluate (
  uplcVersion,
  evalScript,
  evalScriptHuge,
  evalScript',
  evalTermNoEmit,
  evalScriptNoEmit,
  EvalError,
  NoEmitEvaluateError (..),
) where

import Data.Text (Text)
import Plutarch.Script (Script (Script))
import PlutusCore qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget (
  ExBudget (ExBudget),
  ExRestrictingBudget (ExRestrictingBudget),
  minusExBudget,
 )
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParameters)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (ExCPU), ExMemory (ExMemory))
import PlutusCore.Quote qualified as Quote
import UntypedPlutusCore (
  Program (Program),
  Term,
  Version (Version),
 )
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.DeBruijn qualified as DeBruijn
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

type EvalError = (Cek.CekEvaluationException PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun)

uplcVersion :: Version
uplcVersion = Version 1 0 0

-- | Evaluate a script with a big budget, returning the trace log and term result.
evalScript :: Script -> (Either EvalError Script, ExBudget, [Text])
evalScript = evalScript' budget
  where
    -- from https://github.com/input-output-hk/cardano-node/blob/master/configuration/cardano/mainnet-alonzo-genesis.json#L17
    budget = ExBudget (ExCPU 10000000000) (ExMemory 10000000)

-- | Evaluate a script with a huge budget, returning the trace log and term result.
evalScriptHuge :: Script -> (Either EvalError Script, ExBudget, [Text])
evalScriptHuge = evalScript' budget
  where
    -- from https://github.com/input-output-hk/cardano-node/blob/master/configuration/cardano/mainnet-alonzo-genesis.json#L17
    budget = ExBudget (ExCPU maxBound) (ExMemory maxBound)

-- | Evaluate a script with a specific budget, returning the trace log and term result.
evalScript' :: ExBudget -> Script -> (Either (Cek.CekEvaluationException PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun) Script, ExBudget, [Text])
evalScript' budget (Script (Program _ _ t)) = case evalTerm budget (UPLC.termMapNames UPLC.fakeNameDeBruijn t) of
  (res, remaining, logs) -> (Script . Program () uplcVersion . UPLC.termMapNames UPLC.unNameDeBruijn <$> res, remaining, logs)

evalTerm ::
  ExBudget ->
  Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun () ->
  ( Either
      EvalError
      (Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ())
  , ExBudget
  , [Text]
  )
evalTerm budget t =
  case Cek.runCekDeBruijn defaultCekParameters (Cek.restricting (ExRestrictingBudget budget)) Cek.logEmitter t of
    (errOrRes, Cek.RestrictingSt (ExRestrictingBudget final), logs) -> (errOrRes, budget `minusExBudget` final, logs)

data NoEmitEvaluateError
  = NoEmitEvaluateFreeVariableError DeBruijn.FreeVariableError
  | NoEmitEvaluateFailure

-- Evaluates given script using `unsafeEvaluateCekNoEmit`. This will run with unbound budget and without log output
evalTermNoEmit ::
  Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun () ->
  Either NoEmitEvaluateError (Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ())
evalTermNoEmit t =
  case Quote.runQuoteT $ DeBruijn.unDeBruijnTerm t of
    Right namedUPLC ->
      case Cek.unsafeEvaluateCekNoEmit defaultCekParameters namedUPLC of
        Cek.EvaluationSuccess evaluated ->
          case DeBruijn.deBruijnTerm evaluated of
            Right evaluatedTerm -> Right evaluatedTerm
            Left err -> Left $ NoEmitEvaluateFreeVariableError err
        Cek.EvaluationFailure -> Left NoEmitEvaluateFailure
    Left err -> Left $ NoEmitEvaluateFreeVariableError err

evalScriptNoEmit :: Script -> Either NoEmitEvaluateError Script
evalScriptNoEmit (Script (Program _ _ script)) =
  Script . Program () uplcVersion . UPLC.termMapNames UPLC.unNameDeBruijn
    <$> evalTermNoEmit (UPLC.termMapNames UPLC.fakeNameDeBruijn script)
