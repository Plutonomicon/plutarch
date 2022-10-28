{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.DataRepr.Internal.Field (
  -- * PDataField class & deriving utils
  PDataFields (..),
  pletFields,
  pfield,

  -- * BindFields class mechanism
  BindFields (..),
  type Bindings,
  type BoundTerms,
  type Drop,
  type HRecOf,
  type PMemberFields,
  type PMemberField,

  -- * Re-exports
  HRec (..),
  Labeled (Labeled, unLabeled),
  hrecField,
) where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (
  KnownNat,
  Symbol,
 )

import Data.Kind (Constraint, Type)
import Plutarch (
  PInner,
  PType,
  S,
  Term,
  plam,
  plet,
  pto,
  (#),
  type (:-->),
 )

import Plutarch.Builtin (
  PAsData,
  PIsData,
  pfromData,
 )
import Plutarch.DataRepr.Internal (
  PDataRecord,
  PDataSum,
  PLabeledType ((:=)),
  pdropDataRecord,
  pindexDataRecord,
  punDataSum,
  type PLabelIndex,
  type PLookupLabel,
  type PUnLabel,
 )
import Plutarch.DataRepr.Internal.FromData (PFromDataable, pmaybeFromAsData)
import Plutarch.DataRepr.Internal.HList (
  HRec (HCons, HNil),
  Labeled (Labeled, unLabeled),
  hrecField,
  type Drop,
  type ElemOf,
  type IndexLabel,
  type IndexList,
 )
import Plutarch.Internal.Witness (witness)
import Plutarch.TermCont (TermCont (TermCont), runTermCont)

--------------------------------------------------------------------------------
---------- PDataField class & deriving utils

type family Helper (x :: PType) :: [PLabeledType] where
  Helper (PDataSum '[y]) = y
  Helper (PDataRecord y) = y

{- |
  Class allowing 'letFields' to work for a PType, usually via
  `PIsDataRepr`, but is derived for some other types for convenience.
-}
class PDataFields (a :: PType) where
  -- | Fields in HRec bound by 'letFields'
  type PFields a :: [PLabeledType]

  type PFields a = Helper (PInner a)

  -- | Convert a Term to a 'PDataList'
  ptoFields :: Term s a -> Term s (PDataRecord (PFields a))
  default ptoFields :: (PDataFields (PInner a), PFields (PInner a) ~ PFields a) => Term s a -> Term s (PDataRecord (PFields a))
  ptoFields x = ptoFields $ pto x

instance PDataFields (PDataRecord as) where
  type PFields (PDataRecord as) = as
  ptoFields = id

instance PDataFields (PDataSum '[as]) where
  type PFields (PDataSum '[as]) = as
  ptoFields = (punDataSum #)

instance
  forall a.
  ( PIsData a
  , PDataFields a
  ) =>
  PDataFields (PAsData a)
  where
  type PFields (PAsData a) = PFields a
  ptoFields = ptoFields . pfromData

-- | The 'HRec' yielded by 'pletFields @fs t'.
type HRecOf t fs s =
  HRec
    ( BoundTerms
        (PFields t)
        (Bindings (PFields t) fs)
        s
    )

{- | Constrain an 'HRec' to contain the specified fields from the given Plutarch type.

=== Example ===

@
import qualified GHC.Generics as GHC
import Generics.SOP

import Plutarch.Prelude
import Plutarch.DataRepr

newtype PFooType s = PFooType (Term s (PDataRecord '["frst" ':= PInteger, "scnd" ':= PBool, "thrd" ':= PString]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields, PEq)
    via PIsDataReprInstances PFooType

foo :: PMemberFields PFooType '["scnd", "frst"] s as => HRec as -> Term s PInteger
foo h = pif (getField @"scnd" h) (getField @"frst" h) 0
@
-}
type PMemberFields :: PType -> [Symbol] -> S -> [(Symbol, Type)] -> Constraint
type family PMemberFields t fs s as where
  PMemberFields _ '[] _ _ = ()
  PMemberFields t (name ': rest) s as = (PMemberField t name s as, PMemberFields t rest s as)

-- | Single field version of 'PMemberFields'.
type PMemberField :: PType -> Symbol -> S -> [(Symbol, Type)] -> Constraint
type family PMemberField t name s as where
  PMemberField t name s as =
    ( IndexLabel name as ~ Term s (PAsData (PLookupLabel name (PFields t)))
    , ElemOf name (Term s (PAsData (PLookupLabel name (PFields t)))) as
    )

{- |
  Bind a HRec of named fields containing all the specified
  fields.
-}
pletFields ::
  forall fs a s b ps bs.
  ( PDataFields a
  , ps ~ (PFields a)
  , bs ~ (Bindings ps fs)
  , BindFields ps bs
  ) =>
  Term s a ->
  (HRecOf a fs s -> Term s b) ->
  Term s b
pletFields t =
  runTermCont $
    bindFields (Proxy @bs) $ ptoFields @a t

data ToBind = Bind | Skip

{- | Get whether a field should be bound based on its inclusion in a
     list of fields.
-}
type family BindField (p :: Symbol) (fs :: [Symbol]) :: ToBind where
  BindField _ '[] = 'Skip
  BindField name (name ': _) = 'Bind
  BindField name (_ ': as) = BindField name as

-- | Map 'BindField' over @[PLabeledType]@, with 'Skips' removed at tail
type family Bindings (ps :: [PLabeledType]) (fs :: [Symbol]) :: [ToBind] where
  Bindings '[] _ = '[]
  Bindings ((name ':= _) ': ps) fs = (BindField name fs) ': (CutSkip (Bindings ps fs))

-- | Remove 'Skip's at tail
type family CutSkip (bs :: [ToBind]) :: [ToBind] where
  CutSkip '[ 'Skip] = '[]
  CutSkip bs = bs

{- |
  Get the 'Term' representations to be bound based on the
  result of 'Bindings'.
-}
type BoundTerms :: [PLabeledType] -> [ToBind] -> S -> [(Symbol, Type)]
type family BoundTerms ps bs s where
  BoundTerms '[] _ _ = '[]
  BoundTerms _ '[] _ = '[]
  BoundTerms (_ ': ps) ( 'Skip ': bs) s = BoundTerms ps bs s
  BoundTerms ((name ':= p) ': ps) ( 'Bind ': bs) s = '(name, Term s (PAsData p)) ': BoundTerms ps bs s

class BindFields (ps :: [PLabeledType]) (bs :: [ToBind]) where
  -- |
  --    Bind all the fields in a 'PDataList' term to a corresponding
  --    HList of Terms.
  --
  --    A continuation is returned to enable sharing of
  --    the generated bound-variables.
  bindFields :: Proxy bs -> Term s (PDataRecord ps) -> TermCont s (HRec (BoundTerms ps bs s))

instance {-# OVERLAPPABLE #-} BindFields ((l ':= p) ': ps) ( 'Bind ': '[]) where
  bindFields _ t =
    pure $ HCons (Labeled $ pindexDataRecord (Proxy @0) t) HNil

instance {-# OVERLAPPABLE #-} (BindFields ps bs) => BindFields ((l ':= p) ': ps) ( 'Bind ': bs) where
  bindFields _ t = do
    t' <- TermCont $ plet t
    xs <- bindFields (Proxy @bs) (pdropDataRecord (Proxy @1) t')
    pure $ HCons (Labeled $ pindexDataRecord (Proxy @0) t') xs

instance {-# OVERLAPPABLE #-} (BindFields ps bs) => BindFields (p1 ': ps) ( 'Skip ': bs) where
  bindFields _ t = do
    bindFields (Proxy @bs) $ pdropDataRecord (Proxy @1) t

instance {-# OVERLAPPABLE #-} (BindFields ps bs) => BindFields (p1 ': p2 ': ps) ( 'Skip ': 'Skip ': bs) where
  bindFields _ t = do
    bindFields (Proxy @bs) $ pdropDataRecord (Proxy @2) t

instance {-# OVERLAPPABLE #-} (BindFields ps bs) => BindFields (p1 ': p2 ': p3 ': ps) ( 'Skip ': 'Skip ': 'Skip ': bs) where
  bindFields _ t = do
    bindFields (Proxy @bs) $ pdropDataRecord (Proxy @3) t

instance {-# OVERLAPPABLE #-} (BindFields ps bs) => BindFields (p1 ': p2 ': p3 ': p4 ': ps) ( 'Skip ': 'Skip ': 'Skip ': 'Skip ': bs) where
  bindFields _ t = do
    bindFields (Proxy @bs) $ pdropDataRecord (Proxy @4) t

instance {-# OVERLAPPABLE #-} (BindFields ps bs) => BindFields (p1 ': p2 ': p3 ': p4 ': p5 ': ps) ( 'Skip ': 'Skip ': 'Skip ': 'Skip ': 'Skip ': bs) where
  bindFields _ t = do
    bindFields (Proxy @bs) $ pdropDataRecord (Proxy @5) t

instance {-# OVERLAPPABLE #-} (BindFields ps bs) => BindFields (p1 ': p2 ': p3 ': p4 ': p5 ': p6 ': ps) ( 'Skip ': 'Skip ': 'Skip ': 'Skip ': 'Skip ': 'Skip ': bs) where
  bindFields _ t = do
    bindFields (Proxy @bs) $ pdropDataRecord (Proxy @6) t

instance {-# OVERLAPPABLE #-} (BindFields ps bs) => BindFields (p1 ': p2 ': p3 ': p4 ': p5 ': p6 ': p7 ': ps) ( 'Skip ': 'Skip ': 'Skip ': 'Skip ': 'Skip ': 'Skip ': 'Skip ': bs) where
  bindFields _ t = do
    bindFields (Proxy @bs) $ pdropDataRecord (Proxy @7) t

--------------------------------------------------------------------------------

{- |
  Get a single field from a Term.

  *NB*: If you access more than one field from
  the same value you should use 'pletFields' instead,
  which will generate the bindings more efficiently.
-}
pfield ::
  forall name b p s a as n.
  ( PDataFields p
  , as ~ PFields p
  , n ~ PLabelIndex name as
  , KnownNat n
  , a ~ PUnLabel (IndexList n as)
  , PFromDataable a b
  ) =>
  Term s (p #-> b)
pfield =
  let _ = witness (Proxy @(n ~ PLabelIndex name as))
   in plam $ \i ->
        pmaybeFromAsData $ pindexDataRecord (Proxy @n) $ ptoFields @p i
