{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Internal.PlutusType (
  PlutusType,
  PlutusTypeStratConstraint,
  PCon,
  PMatch,
  pcon',
  pmatch',
  pmatch,
  pcon,
  PInner,
  PlutusTypeStrat,
  DerivePlutusType,
  DPTStrat,
  DerivedPInner,
  derivedPCon,
  derivedPMatch,
  PVariant,
  PCovariant,
  PContravariant,
  PVariant',
  PCovariant',
  PContravariant',
  PVariant'',
  PCovariant'',
  PContravariant'',
) where

import Plutarch.Builtin.BLS (
  PBuiltinBLS12_381_G1_Element,
  PBuiltinBLS12_381_G2_Element,
  PBuiltinBLS12_381_MlResult,
 )
import Plutarch.Builtin.Bool (PBool (PFalse, PTrue), pfalse, pif', ptrue)
import Plutarch.Builtin.ByteString (
  PByte,
  PByteString,
  PEndianness,
  PLogicOpSemantics,
 )
import Plutarch.Builtin.Data (
  PAsData (PAsData),
  PBuiltinList (PCons, PNil),
  PBuiltinPair (PBuiltinPair),
  PData (PData),
  pchooseListBuiltin,
  pconsBuiltin,
  pheadBuiltin,
  ptailBuiltin,
 )
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.Opaque (POpaque (POpaque))
import Plutarch.Builtin.String (PString)
import Plutarch.Builtin.Unit (PUnit (PUnit), punit)

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import Generics.SOP (All2)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Generic (PCode, PGeneric, gpfrom, gpto)
import {-# SOURCE #-} Plutarch.Internal.IsData (
  PIsData,
  pdata,
  pfromData,
 )
import {-# SOURCE #-} Plutarch.Internal.Lift (
  PlutusRepr,
  getPLifted,
  unsafeHaskToUni,
 )
import Plutarch.Internal.Quantification (PFix (PFix), PForall (PForall), PSome (PSome))
import Plutarch.Internal.Term (PType, S, Term, pdelay, pforce, plam', plet, punsafeCoerce, (#), (:-->) (PLam))
import Plutarch.Internal.Witness (witness)
import PlutusCore qualified as PLC

class PlutusTypeStrat (strategy :: Type) where
  type PlutusTypeStratConstraint strategy :: PType -> Constraint
  type DerivedPInner strategy (a :: PType) :: PType
  derivedPCon :: forall a s. (DerivePlutusType a, DPTStrat a ~ strategy) => a s -> Term s (DerivedPInner strategy a)
  derivedPMatch :: forall a s b. (DerivePlutusType a, DPTStrat a ~ strategy) => Term s (DerivedPInner strategy a) -> (a s -> Term s b) -> Term s b

class
  ( PInner a ~ DerivedPInner (DPTStrat a) a
  , PlutusTypeStrat (DPTStrat a)
  , PlutusTypeStratConstraint (DPTStrat a) a
  , PlutusType a
  ) =>
  DerivePlutusType (a :: PType)
  where
  type DPTStrat a :: Type
  type DPTStrat a = TypeError ('Text "Please specify a strategy for deriving PlutusType for type " ':<>: 'ShowType a)

class PlutusType (a :: PType) where
  type PInner a :: PType
  type PInner a = DerivedPInner (DPTStrat a) a
  type PCovariant' a :: Constraint
  type PCovariant' a = All2 PCovariant'' (PCode a)
  type PContravariant' a :: Constraint
  type PContravariant' a = All2 PContravariant'' (PCode a)
  type PVariant' a :: Constraint
  type PVariant' a = All2 PVariant'' (PCode a)
  pcon' :: forall s. a s -> Term s (PInner a)
  default pcon' :: DerivePlutusType a => forall s. a s -> Term s (PInner a)
  pcon' = let _ = witness (Proxy @(PlutusType a)) in derivedPCon

  pmatch' :: forall s b. Term s (PInner a) -> (a s -> Term s b) -> Term s b
  -- FIXME buggy GHC, needs AllowAmbiguousTypes
  default pmatch' :: DerivePlutusType a => forall s b. Term s (PInner a) -> (a s -> Term s b) -> Term s b
  pmatch' = derivedPMatch

{-# DEPRECATED PCon "Use PlutusType" #-}
type PCon = PlutusType
{-# DEPRECATED PMatch "Use PlutusType" #-}
type PMatch = PlutusType

-- | Construct a Plutarch Term via a Haskell datatype
pcon :: PlutusType a => a s -> Term s a
pcon x = punsafeCoerce (pcon' x)

-- | Pattern match over Plutarch Terms via a Haskell datatype
pmatch :: PlutusType a => Term s a -> (a s -> Term s b) -> Term s b
pmatch x = pmatch' (punsafeCoerce x)

class PCovariant' a => PCovariant'' a
instance PCovariant' a => PCovariant'' a

class PContravariant' a => PContravariant'' a
instance PContravariant' a => PContravariant'' a

class PVariant' a => PVariant'' a
instance PVariant' a => PVariant'' a

class (forall t. PCovariant'' t => PCovariant'' (a t)) => PCovariant a
instance (forall t. PCovariant'' t => PCovariant'' (a t)) => PCovariant a

class (forall t. PCovariant'' t => PContravariant'' (a t)) => PContravariant a
instance (forall t. PCovariant'' t => PContravariant'' (a t)) => PContravariant a

class (forall t. PVariant'' t => PVariant'' (a t)) => PVariant a
instance (forall t. PVariant'' t => PVariant'' (a t)) => PVariant a

instance PlutusType (a :--> b) where
  type PInner (a :--> b) = a :--> b
  type PCovariant' (a :--> b) = (PContravariant' a, PCovariant' b)
  type PContravariant' (a :--> b) = (PCovariant' a, PContravariant' b)
  type PVariant' (a :--> b) = (PVariant' a, PVariant' b)
  pcon' (PLam f) = plam' f
  pmatch' f g = plet f \f' -> g (PLam (f' #))

instance PlutusType (PForall f) where
  type PInner (PForall f) = PForall f
  pcon' (PForall x) = punsafeCoerce x
  pmatch' x f = f (PForall $ punsafeCoerce x)

instance PlutusType (PSome f) where
  type PInner (PSome f) = PSome f
  pcon' (PSome x) = punsafeCoerce x
  pmatch' x f = f (PSome $ punsafeCoerce x)

instance PlutusType (PFix f) where
  type PInner (PFix f) = f (PFix f)
  pcon' (PFix x) = x
  pmatch' x f = f (PFix x)

--------------------------------------------------------------------------------

data PlutusTypeNewtype

class (PGeneric a, PCode a ~ '[ '[GetPNewtype a]]) => Helper (a :: PType)
instance (PGeneric a, PCode a ~ '[ '[GetPNewtype a]]) => Helper (a :: PType)

instance PlutusTypeStrat PlutusTypeNewtype where
  type PlutusTypeStratConstraint PlutusTypeNewtype = Helper
  type DerivedPInner PlutusTypeNewtype a = GetPNewtype a
  derivedPCon x = case gpfrom x of
    SOP.SOP (SOP.Z (x SOP.:* SOP.Nil)) -> x
    SOP.SOP (SOP.S x) -> case x of {}
  derivedPMatch x f = f (gpto $ SOP.SOP $ SOP.Z $ x SOP.:* SOP.Nil)

type family GetPNewtype' (a :: [[PType]]) :: PType where
  GetPNewtype' '[ '[a]] = a

type family GetPNewtype (a :: PType) :: PType where
  GetPNewtype a = GetPNewtype' (PCode a)

--------------------------------------------------------------------------------

instance DerivePlutusType PInteger where type DPTStrat _ = PlutusTypeNewtype
deriving anyclass instance PlutusType PInteger

instance PlutusType POpaque where
  type PInner POpaque = POpaque
  type PCovariant' POpaque = ()
  type PContravariant' POpaque = ()
  type PVariant' POpaque = ()
  pcon' (POpaque x) = x
  pmatch' x f = f (POpaque x)

-- | @since WIP
instance PlutusType PBool where
  type PInner PBool = PBool
  {-# INLINEABLE pcon' #-}
  pcon' PTrue = ptrue
  pcon' PFalse = pfalse
  {-# INLINEABLE pmatch' #-}
  pmatch' b f = pforce $ pif' # b # pdelay (f PTrue) # pdelay (f PFalse)

instance PlutusType PData where
  type PInner PData = PData
  type PCovariant' PData = ()
  type PContravariant' PData = ()
  type PVariant' PData = ()
  pcon' (PData t) = t
  pmatch' t f = f (PData t)

instance PlutusType (PBuiltinPair a b) where
  type PInner (PBuiltinPair a b) = PBuiltinPair a b
  type PCovariant' (PBuiltinPair a b) = (PCovariant' a, PCovariant' b)
  type PContravariant' (PBuiltinPair a b) = (PContravariant' a, PContravariant' b)
  type PVariant' (PBuiltinPair a b) = (PVariant' a, PVariant' b)
  pcon' (PBuiltinPair x) = x
  pmatch' x f = f (PBuiltinPair x)

instance PLC.Contains PLC.DefaultUni (PlutusRepr a) => PlutusType (PBuiltinList a) where
  type PInner (PBuiltinList a) = PBuiltinList a
  type PCovariant' (PBuiltinList a) = PCovariant' a
  type PContravariant' (PBuiltinList a) = PContravariant' a
  type PVariant' (PBuiltinList a) = PVariant' a
  pcon' (PCons x xs) = pconsBuiltin # x # xs
  pcon' PNil = getPLifted $ unsafeHaskToUni @[PlutusRepr a] []
  pmatch' xs' f = plet xs' $ \xs ->
    pforce $
      pchooseListBuiltin
        # xs
        # pdelay (f PNil)
        # pdelay (f (PCons (pheadBuiltin # xs) (ptailBuiltin # xs)))

type family IfSameThenData (a :: S -> Type) (b :: S -> Type) :: S -> Type where
  IfSameThenData a a = PData
  IfSameThenData _ POpaque = PData
  IfSameThenData _ b = PAsData b

instance PIsData a => PlutusType (PAsData a) where
  type PInner (PAsData a) = IfSameThenData a (PInner a)
  type PCovariant' (PAsData a) = PCovariant' a
  type PContravariant' (PAsData a) = PContravariant' a
  type PVariant' (PAsData a) = PVariant' a
  pcon' (PAsData t) = punsafeCoerce $ pdata t
  pmatch' t f = f (PAsData $ pfromData $ punsafeCoerce t)

instance DerivePlutusType PByteString where type DPTStrat _ = PlutusTypeNewtype
deriving anyclass instance PlutusType PByteString

instance DerivePlutusType PByte where type DPTStrat _ = PlutusTypeNewtype
deriving anyclass instance PlutusType PByte

instance DerivePlutusType PLogicOpSemantics where type DPTStrat _ = PlutusTypeNewtype
deriving anyclass instance PlutusType PLogicOpSemantics

instance DerivePlutusType PString where type DPTStrat _ = PlutusTypeNewtype
deriving anyclass instance PlutusType PString

instance PlutusType PUnit where
  type PInner PUnit = PUnit
  pcon' PUnit = punit
  pmatch' x f = plet x \_ -> f PUnit

-- | @since WIP
instance DerivePlutusType PBuiltinBLS12_381_G1_Element where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving anyclass instance PlutusType PBuiltinBLS12_381_G1_Element

-- | @since WIP
instance DerivePlutusType PBuiltinBLS12_381_G2_Element where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving anyclass instance PlutusType PBuiltinBLS12_381_G2_Element

-- | @since WIP
instance DerivePlutusType PBuiltinBLS12_381_MlResult where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving anyclass instance PlutusType PBuiltinBLS12_381_MlResult

-- | @since WIP
instance DerivePlutusType PEndianness where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving anyclass instance PlutusType PEndianness
