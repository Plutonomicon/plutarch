module Plutarch.Api.V1 (
  -- * Contexts
  Contexts.PScriptContext (PScriptContext),
  Contexts.PTxInfo (PTxInfo),
  Contexts.PScriptPurpose (PMinting, PSpending, PRewarding, PCertifying),

  -- ** Script
  Scripts.PDatum (PDatum),
  Scripts.PDatumHash (PDatumHash),
  Scripts.PRedeemer (PRedeemer),
  Scripts.PRedeemerHash (PRedeemerHash),
  Scripts.PStakeValidatorHash (PStakeValidatorHash),
  Scripts.PValidatorHash (PValidatorHash),

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

  -- ** Value
  Value.PValue (PValue),
  Value.PCurrencySymbol (PCurrencySymbol),
  Value.PTokenName (PTokenName),
  Value.KeyGuarantees (Unsorted, Sorted),
  Value.AmountGuarantees (NoGuarantees, NonZero, Positive),

  -- ** Crypto
  Crypto.PPubKeyHash (PPubKeyHash),
  Crypto.pubKeyHash,

  -- ** DCert
  DCert.PDCert (
    PDCertDelegDelegate,
    PDCertDelegDeRegKey,
    PDCertDelegRegKey,
    PDCertGenesis,
    PDCertMir,
    PDCertPoolRegister,
    PDCertPoolRetire
  ),

  -- ** Time
  Time.PPOSIXTime,
  type Time.PPOSIXTimeRange,

  -- ** Interval
  Interval.PInterval (PInterval),
  Interval.PLowerBound (PLowerBound),
  Interval.PUpperBound (PUpperBound),
  Interval.PExtended (PFinite, PPosInf, PNegInf),
  type Interval.PClosure,

  -- ** Address
  Address.PCredential (PPubKeyCredential, PScriptCredential),
  Address.PStakingCredential (PStakingHash, PStakingPtr),
  Address.PAddress (PAddress),

  -- ** Tx
  Tx.PTxOutRef (PTxOutRef),
  Tx.PTxOut (PTxOut),
  Tx.PTxId (PTxId),
  Tx.PTxInInfo (PTxInInfo),

  -- ** AssocMap
  AssocMap.PMap (PMap),

  -- ** Others
  Maybe.PMaybeData (PDJust, PDNothing),
  type Tuple.PTuple,

  -- ** Utility functions
  Tuple.ptuple,
) where

--------------------------------------------------------------------------------

import qualified Plutarch.Api.V1.Address as Address
import qualified Plutarch.Api.V1.AssocMap as AssocMap
import qualified Plutarch.Api.V1.Contexts as Contexts
import qualified Plutarch.Api.V1.Crypto as Crypto
import qualified Plutarch.Api.V1.DCert as DCert
import qualified Plutarch.Api.V1.Interval as Interval
import qualified Plutarch.Api.V1.Maybe as Maybe
import qualified Plutarch.Api.V1.Scripts as Scripts
import qualified Plutarch.Api.V1.Time as Time
import qualified Plutarch.Api.V1.Tuple as Tuple
import qualified Plutarch.Api.V1.Tx as Tx
import qualified Plutarch.Api.V1.Value as Value

import Data.Coerce (coerce)

-- note about V2: This should there are no changes in Scripts or V1 itself that affect this module
import qualified PlutusLedgerApi.V1 as Plutus
import qualified PlutusLedgerApi.V1.Scripts as Plutus

import Plutarch (Config, compile)
import Plutarch.Api.Internal.Hashing (hashData, hashScriptWithPrefix)
import Plutarch.Api.V1.Contexts (PScriptContext)
import Plutarch.Prelude

import qualified Data.Text as T
import GHC.Stack (HasCallStack)

-- On-chain Script Types

-- | a Validator Term
type PValidator = PData #-> PData #-> PScriptContext #-> POpaque

-- | a MintingPolicy Term
type PMintingPolicy = PData #-> PScriptContext #-> POpaque

-- | a StakeValidator Term
type PStakeValidator = PData #-> PScriptContext #-> POpaque

-- | Compile a Validator
mkValidator :: HasCallStack => Config -> ClosedTerm PValidator -> Plutus.Validator
mkValidator config s = Plutus.Validator $ either (error . T.unpack) id $ compile config s

-- | Compile a MintingPolicy
mkMintingPolicy :: HasCallStack => Config -> ClosedTerm PMintingPolicy -> Plutus.MintingPolicy
mkMintingPolicy config s = Plutus.MintingPolicy $ either (error . T.unpack) id $ compile config s

-- | Compile a StakeValidator
mkStakeValidator :: HasCallStack => Config -> ClosedTerm PStakeValidator -> Plutus.StakeValidator
mkStakeValidator config s = Plutus.StakeValidator $ either (error . T.unpack) id $ compile config s

-- | Hash a Script, with the correct prefix for Plutus V1
scriptHash :: Plutus.Script -> Plutus.ScriptHash
scriptHash = hashScriptWithPrefix "\x01"

-- | Hash a Validator, with the correct prefix for Plutus V1
validatorHash :: Plutus.Validator -> Plutus.ValidatorHash
validatorHash = coerce scriptHash

-- | Hash a MintingPolicy, with the correct prefix for Plutus V1
mintingPolicySymbol :: Plutus.MintingPolicy -> Plutus.CurrencySymbol
mintingPolicySymbol = coerce scriptHash

-- | Hash a StakeValidator, with the correct prefix for Plutus V1
stakeValidatorHash :: Plutus.StakeValidator -> Plutus.StakeValidatorHash
stakeValidatorHash = coerce scriptHash

-- | Hash a Datum.
datumHash :: Plutus.Datum -> Plutus.DatumHash
datumHash = coerce . dataHash

-- | Hash a Redeemer.
redeemerHash :: Plutus.Redeemer -> Plutus.RedeemerHash
redeemerHash = coerce . dataHash

-- | Hash the data encoded representation of given argument.
dataHash :: Plutus.ToData a => a -> Plutus.BuiltinByteString
dataHash = hashData . Plutus.toData
