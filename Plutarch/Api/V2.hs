module Plutarch.Api.V2 (
  -- ** Contexts
  Contexts.PScriptContext (PScriptContext),
  Contexts.PTxInfo (PTxInfo),
  Contexts.PScriptPurpose (PMinting, PSpending, PRewarding, PCertifying),

  -- ** Tx
  Tx.PTxOutRef (PTxOutRef),
  Tx.PTxOut (PTxOut),
  Tx.PTxId (PTxId),
  Tx.PTxInInfo (PTxInInfo),
  Tx.POutputDatum (PNoOutputDatum, POutputDatumHash, POutputDatum),

  -- *** reexports for unchanged V1 ledger types
  V1.PMaybeData (PDNothing, PDJust),
  V1.PTuple,
  V1.PDatum (PDatum),
  V1.PDatumHash (PDatumHash),
  V1.PAddress (PAddress),
  V1.KeyGuarantees (Sorted, Unsorted),
  V1.AmountGuarantees (NoGuarantees, Positive, NonZero),
  V1.PScriptHash (PScriptHash),
  V1.PPubKeyHash (PPubKeyHash),
  V1.PStakingCredential (PStakingHash, PStakingPtr),
  V1.PValue (PValue),
  V1.PCurrencySymbol (PCurrencySymbol),
  V1.PTokenName (PTokenName),
  V1.PInterval (PInterval),
  V1.PExtended (PFinite, PPosInf, PNegInf),
  type V1.PClosure,
  V1.PLowerBound (PLowerBound),
  V1.PUpperBound (PUpperBound),
  V1.PPOSIXTime (PPOSIXTime),
  type V1.PPOSIXTimeRange,
  V1.PMap (PMap),

  -- ** Script Utils
  validatorHash,
  mintingPolicySymbol,
  stakeValidatorHash,
  scriptHash,
  datumHash,
  redeemerHash,
  dataHash,
  mkValidator,
  mkStakeValidator,
  mkMintingPolicy,
  type PValidator,
  type PMintingPolicy,
  type PStakeValidator,
) where

import Data.Coerce (coerce)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)

import qualified PlutusLedgerApi.V1.Scripts as Plutus

import Plutarch (Config, compile)
import Plutarch.Prelude

import qualified Plutarch.Api.V2.Contexts as Contexts
import qualified Plutarch.Api.V2.Tx as Tx

import Plutarch.Api.Internal.Hashing (hashScriptWithPrefix)
import Plutarch.Api.V1 (dataHash, datumHash, redeemerHash)
import qualified Plutarch.Api.V1.Address as V1
import qualified Plutarch.Api.V1.AssocMap as V1
import qualified Plutarch.Api.V1.Crypto as V1
import qualified Plutarch.Api.V1.Interval as V1
import qualified Plutarch.Api.V1.Maybe as V1
import qualified Plutarch.Api.V1.Scripts as V1
import qualified Plutarch.Api.V1.Time as V1
import qualified Plutarch.Api.V1.Tuple as V1
import qualified Plutarch.Api.V1.Value as V1
import qualified PlutusLedgerApi.V1.Value as Plutus

-- On-chain Script Types

-- | a Validator Term
type PValidator = PData #-> PData #-> Contexts.PScriptContext #-> POpaque

-- | a MintingPolicy Term
type PMintingPolicy = PData #-> Contexts.PScriptContext #-> POpaque

-- | a StakeValidator Term
type PStakeValidator = PData #-> Contexts.PScriptContext #-> POpaque

-- | Compile a Validator
mkValidator :: HasCallStack => Config -> ClosedTerm PValidator -> Plutus.Validator
mkValidator config s = Plutus.Validator $ either (error . T.unpack) id $ compile config s

-- | Compile a MintingPolicy
mkMintingPolicy :: HasCallStack => Config -> ClosedTerm PMintingPolicy -> Plutus.MintingPolicy
mkMintingPolicy config s = Plutus.MintingPolicy $ either (error . T.unpack) id $ compile config s

-- | Compile a StakeValidator
mkStakeValidator :: HasCallStack => Config -> ClosedTerm PStakeValidator -> Plutus.StakeValidator
mkStakeValidator config s = Plutus.StakeValidator $ either (error . T.unpack) id $ compile config s

-- | Hash a Script, with the correct prefix for Plutus V2
scriptHash :: Plutus.Script -> Plutus.ScriptHash
scriptHash = hashScriptWithPrefix "\x02"

-- | Hash a Validator, with the correct prefix for Plutus V2
validatorHash :: Plutus.Validator -> Plutus.ValidatorHash
validatorHash = coerce scriptHash

-- | Hash a MintingPolicy, with the correct prefix for Plutus V2
mintingPolicySymbol :: Plutus.MintingPolicy -> Plutus.CurrencySymbol
mintingPolicySymbol = coerce scriptHash

-- | Hash a StakeValidator, with the correct prefix for Plutus V2
stakeValidatorHash :: Plutus.StakeValidator -> Plutus.StakeValidatorHash
stakeValidatorHash = coerce scriptHash
