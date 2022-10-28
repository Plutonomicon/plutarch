{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Maybe (
  PMaybeData (PDJust, PDNothing),
) where

import Plutarch.Builtin (pasConstr, pforgetData)
import Plutarch.DataRepr.Internal (
  DerivePConstantViaData (DerivePConstantViaData),
 )
import Plutarch.Lift (
  PConstantDecl (PConstanted),
  PUnsafeLiftDecl (..),
 )
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

-- | Data encoded Maybe type. Used in various ledger api types.
data PMaybeData a (s :: S)
  = PDJust (Term s (PDataRecord '["_0" ':= a]))
  | PDNothing (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType (PMaybeData a) where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData a => PTryFrom PData (PMaybeData a)
instance PTryFrom PData a => PTryFrom PData (PAsData (PMaybeData a))

instance PLiftData a => PUnsafeLiftDecl (PMaybeData a) where
  type PLifted (PMaybeData a) = Maybe (PLifted a)

deriving via
  (DerivePConstantViaData (Maybe a) (PMaybeData (PConstanted a)))
  instance
    PConstantData a => PConstantDecl (Maybe a)

-- Have to manually write this instance because the constructor id ordering is screwed for 'Maybe'....
instance (PIsData a, POrd a) => PPartialOrd (PMaybeData a) where
  x #< y = _pmaybeLT False (#<) # x # y
  x #<= y = _pmaybeLT True (#<=) # x # y

instance (PIsData a, POrd a) => POrd (PMaybeData a)

_pmaybeLT ::
  Bool ->
  ( forall s rec.
    rec ~ '["_0" ':= a] =>
    Term s (PDataRecord rec) ->
    Term s (PDataRecord rec) ->
    Term s PBool
  ) ->
  Term s (PMaybeData a #-> PMaybeData a #-> PBool)
_pmaybeLT whenBothNothing ltF = phoistAcyclic $
  plam $ \x y -> unTermCont $ do
    a <- tcont . plet $ pasConstr #$ pforgetData $ pdata x
    b <- tcont . plet $ pasConstr #$ pforgetData $ pdata y

    cid1 <- tcont . plet $ pfstBuiltin # a
    cid2 <- tcont . plet $ pfstBuiltin # b

    pure $
      pif
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
