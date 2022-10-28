{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Extra.RationalData (
  PRationalData,
  prationalFromData,
) where

import Data.Bifunctor (first)

import Plutarch.Builtin (pasConstr)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Positive (PPositive)
import Plutarch.Prelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import qualified Plutarch.Unsafe as PUNSAFE
import qualified PlutusTx.Ratio as Plutus

-- | A Rational type that corresponds to the data encoding used by 'Plutus.Rational'.
newtype PRationalData s
  = PRationalData
      ( Term
          s
          ( PDataRecord
              '[ "numerator" ':= PInteger
               , "denominator" ':= PPositive
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PPartialOrd, POrd)

instance DerivePlutusType PRationalData where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PRationalData where type PLifted PRationalData = Plutus.Rational
deriving via (DerivePConstantViaData Plutus.Rational PRationalData) instance PConstantDecl Plutus.Rational

instance PTryFrom PData PRationalData where
  type PTryFromExcess PData PRationalData = Flip Term PPositive
  ptryFrom' opq cont = ptryFrom @(PAsData PRationalData) opq (cont . first PUNSAFE.punsafeCoerce)

-- | NOTE: This instance produces a verified positive denominator as the excess output.
instance PTryFrom PData (PAsData PRationalData) where
  type PTryFromExcess PData (PAsData PRationalData) = Flip Term PPositive
  ptryFrom' opq = runTermCont $ do
    opq' <- pletC $ pasConstr # opq
    pguardC "ptryFrom(PRationalData): invalid constructor id" $ pfstBuiltin # opq' #== 0
    flds <- pletC $ psndBuiltin # opq'
    _numr <- pletC $ ptryFrom @(PAsData PInteger) (phead # flds) snd
    ratTail <- pletC $ ptail # flds
    denm <- pletC $ ptryFrom @(PAsData PPositive) (phead # ratTail) snd
    pguardC "ptryFrom(PRationalData): constructor fields len > 2" $ ptail # ratTail #== pnil
    pure (PUNSAFE.punsafeCoerce opq, denm)

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

prationalFromData :: ClosedTerm (PRationalData #-> PRational)
prationalFromData = phoistAcyclic $
  plam $ \x -> unTermCont $ do
    l <- pletFieldsC @'["numerator", "denominator"] x
    pure . pcon $ PRational (getField @"numerator" l) (getField @"denominator" l)
