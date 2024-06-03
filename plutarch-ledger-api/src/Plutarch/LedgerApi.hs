{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | = Note

The 'Value.PValue', 'AssocMap.PMap' and 'Interval.PInterval'-related
functionality can be found in other modules, as these clash with the
Plutarch prelude. These should be imported qualified.
-}
module Plutarch.LedgerApi (
  -- * Contexts
  PScriptContext (..),
  PTxInfo (..),
  PScriptPurpose (..),

  -- * Tx

  -- ** Types
  PTxOutRef (..),
  PTxOut (..),
  PTxId (..),
  PTxInInfo (..),
  POutputDatum (..),

  -- ** Functions
  pgetContinuingOutputs,
  pfindOwnInput,

  -- * Script

  -- ** Types
  PDatum (..),
  PDatumHash (..),
  PRedeemer (..),
  PRedeemerHash (..),
  PScriptHash (..),

  -- ** Functions
  scriptHash,
  datumHash,
  redeemerHash,
  dataHash,
  pparseDatum,

  -- * Value
  Value.PValue (..),
  Value.AmountGuarantees (..),
  Value.PCurrencySymbol (..),
  Value.PTokenName (..),

  -- * DCert
  PDCert (..),

  -- * Assoc map

  -- ** Types
  AssocMap.PMap (..),
  AssocMap.KeyGuarantees (..),
  AssocMap.Commutativity (..),

  -- * Address
  PCredential (..),
  PStakingCredential (..),
  PAddress (..),

  -- * Time
  PPosixTime (..),

  -- * Interval
  Interval.PInterval (..),
  Interval.PLowerBound (..),
  Interval.PUpperBound (..),
  Interval.PExtended (..),

  -- * Crypto

  -- ** Types
  PubKey (..),
  PPubKeyHash (..),
  pubKeyHash,

  -- * Utilities

  -- ** Types
  PMaybeData (..),

  -- ** Utilities
  pfromDJust,
  pisDJust,
  pmaybeData,
  pdjust,
  pdnothing,
  pmaybeToMaybeData,
  passertPDJust,
) where

import Codec.Serialise (serialise)
import Crypto.Hash (
  Blake2b_224 (Blake2b_224),
  Blake2b_256 (Blake2b_256),
  hashWith,
 )
import Data.ByteArray (convert)
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Short (fromShort)
import Data.Coerce (coerce)
import Plutarch.Builtin (pasConstr, pforgetData)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval qualified as Interval
import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Lift (
  DerivePConstantViaBuiltin (DerivePConstantViaBuiltin),
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl (PConstanted),
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Num (PNum)
import Plutarch.Prelude
import Plutarch.Script (Script (unScript))
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'), ptryFromInfo)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.Common (serialiseUPLC)
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx.Prelude qualified as PlutusTx

-- | @since 2.0.0
newtype PScriptContext (s :: S)
  = PScriptContext
      ( Term
          s
          ( PDataRecord
              '[ "txInfo" ':= PTxInfo
               , "purpose" ':= PScriptPurpose
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PScriptContext where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PScriptContext where
  type PLifted _ = Plutus.ScriptContext

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.ScriptContext PScriptContext)
  instance
    PConstantDecl Plutus.ScriptContext

-- A pending transaction. This is the view as seen by a validator script.
--
-- @since 2.0.0
newtype PTxInfo (s :: S)
  = PTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList PTxInInfo
               , "referenceInputs" ':= PBuiltinList PTxInInfo
               , "outputs" ':= PBuiltinList PTxOut
               , "fee" ':= Value.PValue 'AssocMap.Sorted 'Value.Positive
               , "mint" ':= Value.PValue 'AssocMap.Sorted 'Value.NoGuarantees -- value minted by transaction
               , "dcert" ':= PBuiltinList PDCert -- Digests of certificates included in this transaction
               , "wdrl" ':= AssocMap.PMap 'AssocMap.Unsorted PStakingCredential PInteger -- Staking withdrawals
               , "validRange" ':= Interval.PInterval PPosixTime
               , "signatories" ':= PBuiltinList (PAsData PPubKeyHash)
               , "redeemers" ':= AssocMap.PMap 'AssocMap.Unsorted PScriptPurpose PRedeemer
               , "datums" ':= AssocMap.PMap 'AssocMap.Unsorted PDatumHash PDatum
               , "id" ':= PTxId -- hash of the pending transaction
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTxInfo where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PTxInfo where
  type PLifted _ = Plutus.TxInfo

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.TxInfo PTxInfo)
  instance
    PConstantDecl Plutus.TxInfo

-- | @since 2.0.0
data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= Value.PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | PRewarding (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= PDCert]))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PScriptPurpose where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PScriptPurpose where
  type PLifted PScriptPurpose = Plutus.ScriptPurpose

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.ScriptPurpose PScriptPurpose)
  instance
    PConstantDecl Plutus.ScriptPurpose

-- | @since 2.0.0
newtype PTxOut (s :: S)
  = PTxOut
      ( Term
          s
          ( PDataRecord
              '[ "address" ':= PAddress
               , "value" ':= Value.PValue 'AssocMap.Sorted 'Value.Positive
               , "datum" ':= POutputDatum
               , "referenceScript" ':= PMaybeData PScriptHash
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTxOut where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PTxOut where
  type PLifted PTxOut = Plutus.TxOut

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.TxOut PTxOut)
  instance
    PConstantDecl Plutus.TxOut

{- | An input of the pending transaction.

@since 2.0.0
-}
newtype PTxInInfo (s :: S)
  = PTxInInfo
      ( Term
          s
          ( PDataRecord
              '[ "outRef" ':= PTxOutRef
               , "resolved" ':= PTxOut
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTxInInfo where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PTxInInfo where
  type PLifted PTxInInfo = Plutus.TxInInfo

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.TxInInfo PTxInInfo)
  instance
    PConstantDecl Plutus.TxInInfo

-- | @since 2.0.0
data POutputDatum (s :: S)
  = PNoOutputDatum (Term s (PDataRecord '[]))
  | POutputDatumHash (Term s (PDataRecord '["datumHash" ':= PDatumHash]))
  | -- | Inline datum as per
    -- [CIP-0032](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0032/README.md)
    POutputDatum (Term s (PDataRecord '["outputDatum" ':= PDatum]))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType POutputDatum where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl POutputDatum where
  type PLifted POutputDatum = Plutus.OutputDatum

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.OutputDatum POutputDatum)
  instance
    PConstantDecl Plutus.OutputDatum

-- | @since 2.0.0
newtype PDatum (s :: S) = PDatum (Term s PData)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PDatum where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PDatum where
  type PLifted PDatum = Plutus.Datum

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.Datum PDatum PData)
  instance
    PConstantDecl Plutus.Datum

-- | @since 2.0.0
newtype PRedeemer (s :: S) = PRedeemer (Term s PData)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PRedeemer where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PRedeemer where
  type PLifted PRedeemer = Plutus.Redeemer

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.Redeemer PRedeemer PData)
  instance
    PConstantDecl Plutus.Redeemer

-- | @since 2.0.0
newtype PDatumHash (s :: S) = PDatumHash (Term s PByteString)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PDatumHash where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PDatumHash where
  type PLifted PDatumHash = Plutus.DatumHash

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.DatumHash PDatumHash PByteString)
  instance
    PConstantDecl Plutus.DatumHash

-- | @since 2.0.0
newtype PRedeemerHash (s :: S) = PRedeemerHash (Term s PByteString)

{- | Hash a script, appending the Plutus V2 prefix.

@since 2.0.0
-}
scriptHash :: Script -> Plutus.ScriptHash
scriptHash = hashScriptWithPrefix "\x02"

-- | @since 2.0.0
data PDCert (s :: S)
  = PDCertDelegRegKey (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PDCertDelegDeRegKey (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PDCertDelegDelegate
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PStakingCredential
               , "_1" ':= PPubKeyHash
               ]
          )
      )
  | PDCertPoolRegister (Term s (PDataRecord '["_0" ':= PPubKeyHash, "_1" ':= PPubKeyHash]))
  | PDCertPoolRetire (Term s (PDataRecord '["_0" ':= PPubKeyHash, "_1" ':= PInteger]))
  | PDCertGenesis (Term s (PDataRecord '[]))
  | PDCertMir (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PDCert where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PDCert where
  type PLifted PDCert = Plutus.DCert

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.DCert PDCert)
  instance
    PConstantDecl Plutus.DCert

-- | @since 2.0.0
data PCredential (s :: S)
  = PPubKeyCredential (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  | PScriptCredential (Term s (PDataRecord '["_0" ':= PScriptHash]))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    , -- | @since 2.0.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PCredential where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PCredential where
  type PLifted PCredential = Plutus.Credential

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.Credential PCredential)
  instance
    PConstantDecl Plutus.Credential

-- | @since 2.0.0
instance PTryFrom PData (PAsData PCredential)

-- | @since 2.0.0
data PStakingCredential (s :: S)
  = PStakingHash (Term s (PDataRecord '["_0" ':= PCredential]))
  | PStakingPtr
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PInteger
               , "_1" ':= PInteger
               , "_2" ':= PInteger
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    , -- | @since 2.0.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PStakingCredential where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PStakingCredential where
  type PLifted PStakingCredential = Plutus.StakingCredential

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.StakingCredential PStakingCredential)
  instance
    PConstantDecl Plutus.StakingCredential

-- | @since 2.0.0
instance PTryFrom PData (PAsData PStakingCredential)

-- | @since 2.0.0
newtype PPosixTime (s :: S) = PPosixTime (Term s PInteger)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PIntegral
    , -- | @since 2.0.0
      PNum
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PPosixTime where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PPosixTime where
  type PLifted PPosixTime = Plutus.POSIXTime

-- | @since 2.0.0
deriving via
  (DerivePConstantViaNewtype Plutus.POSIXTime PPosixTime PInteger)
  instance
    PConstantDecl Plutus.POSIXTime

-- | @since 2.0.0
instance PTryFrom PData (PAsData PPosixTime) where
  type PTryFromExcess PData (PAsData PPosixTime) = Mret PPosixTime
  ptryFrom' opq x f = ptryFrom' @_ @(PAsData PInteger) opq x $ \wrapped unwrapped ->
    pif
      (0 #<= unwrapped)
      (f (punsafeCoerce wrapped) (pcon $ PPosixTime unwrapped))
      (ptraceInfoError "ptryFrom(PosixTime): must be positive")

-- | @since 2.0.0
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PPubKeyHash where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PPubKeyHash where
  type PLifted PPubKeyHash = Plutus.PubKeyHash

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.PubKeyHash PPubKeyHash PByteString)
  instance
    PConstantDecl Plutus.PubKeyHash

-- | @since 2.0.0
instance PTryFrom PData (PAsData PPubKeyHash) where
  type PTryFromExcess PData (PAsData PPubKeyHash) = Mret PPubKeyHash
  ptryFrom' opq x f = ptryFrom' @_ @(PAsData PByteString) opq x $ \_ unwrapped ->
    pif
      (plengthBS # unwrapped #== 28)
      (f (punsafeCoerce opq) (pcon . PPubKeyHash $ unwrapped))
      (ptraceInfoError "ptryFrom(PPubKeyHash): must be 28 bytes long")

-- | @since 2.0.0
newtype PubKey = PubKey
  { getPubKey :: Plutus.LedgerBytes
  -- ^ @since 2.0.0
  }
  deriving stock
    ( -- | @since 2.0.0
      Eq
    , -- | @since 2.0.0
      Ord
    )
  deriving stock
    ( -- | @since 2.0.0
      Show
    )

-- | @since 2.0.0
pubKeyHash :: PubKey -> Plutus.PubKeyHash
pubKeyHash = coerce hashLedgerBytes

-- | @since 2.0.0
newtype PTxId (s :: S) = PTxId (Term s (PDataRecord '["_0" ':= PByteString]))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTxId where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PTxId where
  type PLifted PTxId = Plutus.TxId

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.TxId PTxId)
  instance
    PConstantDecl Plutus.TxId

-- | @since 2.0.0
instance PTryFrom PData PTxId where
  type PTryFromExcess PData PTxId = Mret PByteString
  ptryFrom' opq x cont = ptryFrom' @_ @(PAsData PTxId) opq x (cont . punsafeCoerce)

-- | @since 2.0.0
instance PTryFrom PData (PAsData PTxId) where
  type PTryFromExcess PData (PAsData PTxId) = Mret PByteString
  ptryFrom' opq x f = plet (pasConstr # opq) $ \opq' ->
    pif
      (pfstBuiltin # opq' #== 0)
      ( plet (psndBuiltin # opq') $ \flds ->
          let dataBs = phead # flds
           in pif
                (pnil #== ptail # flds)
                ( ptryFrom' @_ @(PAsData PByteString) dataBs x $ \_ unwrapped ->
                    pif
                      (plengthBS # unwrapped #== 32)
                      (f (punsafeCoerce opq) unwrapped)
                      (ptraceInfoError "ptryFrom(TxId): must be 32 bytes long")
                )
                (ptraceInfoError "ptryFrom(TxId): constructor fields len > 1")
      )
      (ptraceInfoError "ptryFrom(TxId): invalid constructor id")

{- | Reference to a transaction output, with an index referencing which exact
output we mean.

@since 2.0.0
-}
newtype PTxOutRef (s :: S)
  = PTxOutRef
      ( Term
          s
          ( PDataRecord
              '[ "id" ':= PTxId
               , "idx" ':= PInteger
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PTryFrom PData
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTxOutRef where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PTxOutRef where
  type PLifted PTxOutRef = Plutus.TxOutRef

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.TxOutRef PTxOutRef)
  instance
    PConstantDecl Plutus.TxOutRef

-- | @since 2.0.0
newtype PAddress (s :: S)
  = PAddress
      ( Term
          s
          ( PDataRecord
              '[ "credential" ':= PCredential
               , "stakingCredential" ':= PMaybeData PStakingCredential
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    , -- | @since 2.0.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PAddress where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PAddress where
  type PLifted PAddress = Plutus.Address

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.Address PAddress)
  instance
    PConstantDecl Plutus.Address

-- | @since 2.0.0
instance PTryFrom PData (PAsData PAddress)

-- | @since 2.0.0
data PMaybeData (a :: S -> Type) (s :: S)
  = PDJust (Term s (PDataRecord '["_0" ':= a]))
  | PDNothing (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType (PMaybeData a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PTryFrom PData a => PTryFrom PData (PMaybeData a)

-- | @since 2.0.0
instance PTryFrom PData a => PTryFrom PData (PAsData (PMaybeData a))

-- | @since 2.0.0
instance PLiftData a => PUnsafeLiftDecl (PMaybeData a) where
  type PLifted (PMaybeData a) = Maybe (PLifted a)

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData (Maybe a) (PMaybeData (PConstanted a)))
  instance
    PConstantData a => PConstantDecl (Maybe a)

-- Have to manually write this instance because the constructor id ordering is screwed for 'Maybe'....

-- | @since 2.0.0
instance (PIsData a, POrd a) => PPartialOrd (PMaybeData a) where
  x #< y = pmaybeLT False (#<) # x # y
  x #<= y = pmaybeLT True (#<=) # x # y

-- | @since 2.0.0
instance (PIsData a, POrd a) => POrd (PMaybeData a)

-- | @since 2.0.0
newtype PScriptHash (s :: S) = PScriptHash (Term s PByteString)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PScriptHash where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PScriptHash where
  type PLifted PScriptHash = Plutus.ScriptHash

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.ScriptHash PScriptHash PByteString)
  instance
    PConstantDecl Plutus.ScriptHash

-- | @since 2.0.0
instance PTryFrom PData (PAsData PScriptHash) where
  type PTryFromExcess PData (PAsData PScriptHash) = Mret PScriptHash
  ptryFrom' opq x f = ptryFrom' @_ @(PAsData PByteString) opq x $ \_ unwrapped ->
    pif
      (plengthBS # unwrapped #== 28)
      (f (punsafeCoerce opq) (pcon . PScriptHash $ unwrapped))
      (ptraceInfoError "ptryFrom(PScriptHash): must be 28 bytes long")

-- | @since 2.0.0
datumHash :: Plutus.Datum -> Plutus.DatumHash
datumHash = coerce . dataHash

-- | @since 2.0.0
dataHash ::
  forall (a :: Type).
  Plutus.ToData a =>
  a ->
  PlutusTx.BuiltinByteString
dataHash = hashData . Plutus.toData

-- | @since 2.0.0
redeemerHash :: Plutus.Redeemer -> Plutus.RedeemerHash
redeemerHash = coerce . dataHash

{- | Find the output txns corresponding to the input being validated.

  Takes as arguments the inputs, outputs and the spending transaction referenced
  from `PScriptPurpose`.

  __Example:__

  @
  ctx <- tcont $ pletFields @["txInfo", "purpose"] sc
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # (getField @"txInfo" ctx)
          outputs = pfield @"outputs" # (getField @"txInfo" ctx)
      pure $ pgetContinuingOutputs # inputs # outputs # outRef
    _ ->
      pure $ ptraceInfoError "not a spending tx"
  @

  @since 2.1.0
-}
pgetContinuingOutputs ::
  forall (s :: S).
  Term s (PBuiltinList PTxInInfo :--> PBuiltinList PTxOut :--> PTxOutRef :--> PBuiltinList PTxOut)
pgetContinuingOutputs = phoistAcyclic $
  plam $ \inputs outputs outRef ->
    pmatch (pfindOwnInput # inputs # outRef) $ \case
      PJust tx -> do
        let resolved = pfield @"resolved" # tx
            outAddr = pfield @"address" # resolved
        pfilter # (matches # outAddr) # outputs
      PNothing ->
        ptraceInfoError "can't get any continuing outputs"
  where
    matches ::
      forall (s' :: S).
      Term s' (PAddress :--> PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut ->
        adr #== pfield @"address" # txOut

{- | Find the input being spent in the current transaction.

  Takes as arguments the inputs, as well as the spending transaction referenced from `PScriptPurpose`.

  __Example:__

  @
  ctx <- tcont $ pletFields @["txInfo", "purpose"] sc
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # (getField @"txInfo" ctx)
      pure $ pfindOwnInput # inputs # outRef
    _ ->
      pure $ ptraceInfoError "not a spending tx"
  @

  @since 2.1.0
-}
pfindOwnInput ::
  forall (s :: S).
  Term s (PBuiltinList PTxInInfo :--> PTxOutRef :--> PMaybe PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \inputs outRef ->
    pfind # (matches # outRef) # inputs
  where
    matches ::
      forall (s' :: S).
      Term s' (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        outref #== pfield @"outRef" # txininfo

{- | Lookup up the datum given the datum hash.

  Takes as argument the datum assoc list from a `PTxInfo`. Validates the datum
  using `PTryFrom`.

  __Example:__

  @
  pparseDatum @MyType # datumHash #$ pfield @"datums" # txinfo
  @

  @since 2.1.2
-}
pparseDatum ::
  forall (a :: S -> Type) (s :: S).
  PTryFrom PData (PAsData a) =>
  Term s (PDatumHash :--> AssocMap.PMap 'AssocMap.Unsorted PDatumHash PDatum :--> PMaybe (PAsData a))
pparseDatum = phoistAcyclic $ plam $ \dh datums ->
  pmatch (AssocMap.plookup # dh # datums) $ \case
    PNothing -> pcon PNothing
    PJust datum -> pcon . PJust $ ptryFromInfo (pto datum) "could not parse datum" const

{- | Extracts the element out of a 'PDJust' and throws an error if its
argument is 'PDNothing'.

@since 2.1.1
-}
pfromDJust ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (PMaybeData a :--> a)
pfromDJust = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PDNothing _ -> ptraceInfoError "pfromDJust: found PDNothing"
    PDJust x -> pfromData $ pfield @"_0" # x

{- | Yield 'PTrue' if a given 'PMaybeData' is of the form @'PDJust' _@.

@since 2.1.1
-}
pisDJust ::
  forall (a :: PType) (s :: S).
  Term s (PMaybeData a :--> PBool)
pisDJust = phoistAcyclic $
  plam $ \x -> pmatch x $ \case
    PDJust _ -> pconstant True
    _ -> pconstant False

{- | Special version of 'pmaybe' that works with 'PMaybeData'.

@since 2.1.1
-}
pmaybeData ::
  forall (a :: PType) (b :: PType) (s :: S).
  PIsData a =>
  Term s (b :--> (a :--> b) :--> PMaybeData a :--> b)
pmaybeData = phoistAcyclic $
  plam $ \d f m -> pmatch m $
    \case
      PDJust x -> f #$ pfield @"_0" # x
      _ -> d

{- | Construct a 'PDJust' value.

@since 2.1.1
-}
pdjust ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (a :--> PMaybeData a)
pdjust = phoistAcyclic $
  plam $
    \x -> pcon $ PDJust $ pdcons @"_0" # pdata x #$ pdnil

{- | Construct a 'PDNothing' value.

@since 2.1.1
-}
pdnothing ::
  forall (a :: PType) (s :: S).
  Term s (PMaybeData a)
pdnothing = phoistAcyclic $ pcon $ PDNothing pdnil

{- | Construct a 'PMaybeData' given a 'PMaybe'. Could be useful if you want to
"lift" from 'PMaybe' to 'Maybe'.

@since 2.1.1
-}
pmaybeToMaybeData ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (PMaybe a :--> PMaybeData a)
pmaybeToMaybeData = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PNothing -> pcon $ PDNothing pdnil
    PJust x -> pcon $ PDJust $ pdcons @"_0" # pdata x # pdnil

{- | Extract the value stored in a 'PMaybeData' container. If there's no value,
throw an error with the given message.

@since 2.1.1
-}
passertPDJust ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (PString :--> PMaybeData a :--> a)
passertPDJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    PDJust ((pfield @"_0" #) -> v) -> v
    _ -> ptraceInfoError emsg

-- Helpers

hashScriptWithPrefix :: ByteString -> Script -> Plutus.ScriptHash
hashScriptWithPrefix prefix scr =
  Plutus.ScriptHash . hashBlake2b_224 $
    prefix <> (fromShort . serialiseUPLC . unScript $ scr)

hashLedgerBytes :: Plutus.LedgerBytes -> PlutusTx.BuiltinByteString
hashLedgerBytes = hashBlake2b_224 . PlutusTx.fromBuiltin . Plutus.getLedgerBytes

hashBlake2b_224 :: ByteString -> PlutusTx.BuiltinByteString
hashBlake2b_224 = PlutusTx.toBuiltin . convert @_ @ByteString . hashWith Blake2b_224

hashBlake2b_256 :: ByteString -> PlutusTx.BuiltinByteString
hashBlake2b_256 = PlutusTx.toBuiltin . convert @_ @ByteString . hashWith Blake2b_256

hashData :: Plutus.Data -> PlutusTx.BuiltinByteString
hashData = hashBlake2b_256 . toStrict . serialise

pmaybeLT ::
  forall (a :: S -> Type) (s :: S).
  Bool ->
  ( forall (s' :: S) (rec_ :: [PLabeledType]).
    rec_ ~ '["_0" ':= a] =>
    Term s' (PDataRecord rec_) ->
    Term s' (PDataRecord rec_) ->
    Term s' PBool
  ) ->
  Term s (PMaybeData a :--> PMaybeData a :--> PBool)
pmaybeLT whenBothNothing ltF = phoistAcyclic $
  plam $ \x y -> unTermCont $ do
    a <- tcont . plet $ pasConstr #$ pforgetData $ pdata x
    b <- tcont . plet $ pasConstr #$ pforgetData $ pdata y
    cid1 <- tcont . plet $ pfstBuiltin # a
    cid2 <- tcont . plet $ pfstBuiltin # b
    pure
      $ pif
        (cid1 #< cid2)
        (pconstant False)
      $ pif
        (cid1 #== cid2)
        {- Some hand optimization here: usually, the fields would be 'plet'ed here if using 'POrd' derivation
          machinery. However, in this case - there's no need for the fields for the 'Nothing' case.

          Would be nice if this could be done on the auto derivation case....
        -}
        ( pif
            (cid1 #== 0)
            (ltF (punsafeCoerce $ psndBuiltin # a) (punsafeCoerce $ psndBuiltin # b))
            -- Both are 'Nothing'. Let caller choose answer.
            $ pconstant whenBothNothing
        )
      $ pconstant True
