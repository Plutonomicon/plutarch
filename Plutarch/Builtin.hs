{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Builtin (
  PData,
  pfstBuiltin,
  psndBuiltin,
  pasConstr,
  pasMap,
  pasList,
  pasInt,
  pconstantData,
  pconstrBuiltin,
  pasByteStr,
  PBuiltinPair,
  PBuiltinList (..),
  pdataLiteral,
  PIsData (..),
  PAsData,
  pforgetData,
  ppairDataBuiltin,
  pchooseListBuiltin,
  type PBuiltinMap,
) where

import Data.Coerce (Coercible, coerce)
import Data.Proxy (Proxy (Proxy))
import Plutarch (
  DerivePNewtype,
  PInner,
  PType,
  PlutusType,
  S,
  Term,
  pcon,
  pcon',
  pdelay,
  pforce,
  phoistAcyclic,
  plam,
  plet,
  pmatch,
  pmatch',
  pto,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Bool (PBool (..), PEq, pif', (#==))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstant,
  PConstantDecl,
  PConstantRepr,
  PConstanted,
  PLift,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
  pconstantFromRepr,
  pconstantToRepr,
 )
import Plutarch.List (
  PListLike (
    PElemConstraint,
    pcons,
    pelimList,
    phead,
    pnil,
    pnull,
    ptail
  ),
  plistEquals,
  pshowList,
 )
import Plutarch.Show (PShow (pshow'))
import Plutarch.Unit (PUnit)
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce, punsafeFrom)
import qualified PlutusCore as PLC
import PlutusTx (Data (Constr), ToData)
import qualified PlutusTx

-- | Plutus 'BuiltinPair'
data PBuiltinPair (a :: PType) (b :: PType) (s :: S)

instance (PLift a, PLift b) => PUnsafeLiftDecl (PBuiltinPair a b) where
  type PLifted (PBuiltinPair a b) = (PLifted a, PLifted b)

-- FIXME: figure out good way of deriving this
instance (PConstant a, PConstant b) => PConstantDecl (a, b) where
  type PConstantRepr (a, b) = (PConstantRepr a, PConstantRepr b)
  type PConstanted (a, b) = PBuiltinPair (PConstanted a) (PConstanted b)
  pconstantToRepr (x, y) = (pconstantToRepr x, pconstantToRepr y)
  pconstantFromRepr (x, y) = do
    x' <- pconstantFromRepr @a x
    y' <- pconstantFromRepr @b y
    Just (x', y')

pfstBuiltin :: Term s (PBuiltinPair a b :--> a)
pfstBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.FstPair

psndBuiltin :: Term s (PBuiltinPair a b :--> b)
psndBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.SndPair

{- | Construct a builtin pair of 'PData' elements.

Uses 'PAsData' to preserve more information about the underlying 'PData'.
-}
ppairDataBuiltin :: Term s (PAsData a :--> PAsData b :--> PBuiltinPair (PAsData a) (PAsData b))
ppairDataBuiltin = punsafeBuiltin PLC.MkPairData

-- | Plutus 'BuiltinList'
data PBuiltinList (a :: PType) (s :: S)
  = PCons (Term s a) (Term s (PBuiltinList a))
  | PNil

instance (PShow a, PLift a) => PShow (PBuiltinList a) where
  pshow' _ x = pshowList @PBuiltinList @a # x

pheadBuiltin :: Term s (PBuiltinList a :--> a)
pheadBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.HeadList

ptailBuiltin :: Term s (PBuiltinList a :--> PBuiltinList a)
ptailBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.TailList

pchooseListBuiltin :: Term s (PBuiltinList a :--> b :--> b :--> b)
pchooseListBuiltin = phoistAcyclic $ pforce $ pforce $ punsafeBuiltin PLC.ChooseList

pnullBuiltin :: Term s (PBuiltinList a :--> PBool)
pnullBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.NullList

pconsBuiltin :: Term s (a :--> PBuiltinList a :--> PBuiltinList a)
pconsBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.MkCons

instance PConstant a => PConstantDecl [a] where
  type PConstantRepr [a] = [PConstantRepr a]
  type PConstanted [a] = PBuiltinList (PConstanted a)
  pconstantToRepr x = pconstantToRepr <$> x
  pconstantFromRepr x = traverse (pconstantFromRepr @a) x

instance PUnsafeLiftDecl a => PUnsafeLiftDecl (PBuiltinList a) where
  type PLifted (PBuiltinList a) = [PLifted a]

instance PLift a => PlutusType (PBuiltinList a) where
  type PInner (PBuiltinList a) _ = PBuiltinList a
  pcon' (PCons x xs) = pconsBuiltin # x # xs
  pcon' PNil = pconstant []
  pmatch' xs' f = plet xs' $ \xs ->
    pforce $
      pchooseListBuiltin
        # xs
        # pdelay (f PNil)
        # pdelay (f (PCons (pheadBuiltin # xs) (ptailBuiltin # xs)))

instance PListLike PBuiltinList where
  type PElemConstraint PBuiltinList a = PLift a

  pelimList match_cons match_nil ls = pmatch ls $ \case
    PCons x xs -> match_cons x xs
    PNil -> match_nil
  pcons = plam $ \x xs -> pcon (PCons x xs)
  pnil = pcon PNil
  phead = pheadBuiltin
  ptail = ptailBuiltin
  pnull = pnullBuiltin

instance (PLift a, PEq a) => PEq (PBuiltinList a) where
  (#==) xs ys = plistEquals # xs # ys

instance {-# OVERLAPPING #-} PIsData a => PEq (PBuiltinList (PAsData a)) where
  xs #== ys = pdata xs #== pdata ys

instance {-# OVERLAPPING #-} PEq (PBuiltinList PData) where
  xs #== ys = pdata xs #== pdata ys

data PData (s :: S) = PData (Term s PData)

instance PlutusType PData where
  type PInner PData _ = PData
  pcon' (PData t) = t
  pmatch' t f = f (PData t)

instance PUnsafeLiftDecl PData where type PLifted PData = Data
deriving via (DerivePConstantDirect Data PData) instance PConstantDecl Data

instance PEq PData where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y

{- |
  Map type used for Plutus `Data`'s Map constructor.

  Note that the Plutus API doesn't use this most of the time,
  instead encoding as a List of Tuple constructors.

  Not to be confused with `PlutusTx.AssocMap.Map` / `PMap`
-}
type PBuiltinMap a b = (PBuiltinList (PBuiltinPair (PAsData a) (PAsData b)))

pasConstr :: Term s (PData :--> PBuiltinPair PInteger (PBuiltinList PData))
pasConstr = punsafeBuiltin PLC.UnConstrData

pasMap :: Term s (PData :--> PBuiltinList (PBuiltinPair PData PData))
pasMap = punsafeBuiltin PLC.UnMapData

pasList :: Term s (PData :--> PBuiltinList PData)
pasList = punsafeBuiltin PLC.UnListData

pasInt :: Term s (PData :--> PInteger)
pasInt = punsafeBuiltin PLC.UnIData

pasByteStr :: Term s (PData :--> PByteString)
pasByteStr = punsafeBuiltin PLC.UnBData

{-# DEPRECATED pdataLiteral "Use `pconstant` instead." #-}
pdataLiteral :: Data -> Term s PData
pdataLiteral = pconstant

type role PAsData representational phantom
data PAsData (a :: PType) (s :: S)

type role PAsDataLifted representational
data PAsDataLifted (a :: PType)

instance PConstantDecl (PAsDataLifted a) where
  type PConstantRepr (PAsDataLifted a) = Data
  type PConstanted (PAsDataLifted a) = PAsData a
  pconstantToRepr = \case {}
  pconstantFromRepr _ = Nothing

instance PUnsafeLiftDecl (PAsData a) where type PLifted (PAsData a) = PAsDataLifted a

newtype Helper1 (a :: PType) (s :: S) = Helper1 (a s)

pforgetData :: forall s a. Term s (PAsData a) -> Term s PData
pforgetData x = coerce $ pforgetData' Proxy (coerce x :: Term s (Helper1 (PAsData a)))

-- | Like 'pforgetData', except it works for complex types.
pforgetData' :: forall a (p :: PType -> PType) s. Proxy p -> Term s (p (PAsData a)) -> Term s (p PData)
pforgetData' Proxy = punsafeCoerce

-- | Inverse of 'pforgetData''.
prememberData :: forall (p :: PType -> PType) s. Proxy p -> Term s (p PData) -> Term s (p (PAsData PData))
prememberData Proxy = punsafeCoerce

class PIsData a where
  pfromData :: Term s (PAsData a) -> Term s a
  pdata :: Term s a -> Term s (PAsData a)

instance PIsData PData where
  pfromData = punsafeCoerce
  pdata = punsafeCoerce

instance PIsData a => PIsData (PBuiltinList (PAsData a)) where
  pfromData x = punsafeCoerce $ pasList # pforgetData x
  pdata x = punsafeBuiltin PLC.ListData # x

newtype Helper2 f a s = Helper2 (PAsData (f a) s)

instance PIsData (PBuiltinList PData) where
  pfromData = pforgetData' @PData (Proxy @PBuiltinList) . pfromData . coerce (prememberData (Proxy @(Helper2 PBuiltinList)))
  pdata = coerce (pforgetData' @PData (Proxy @(Helper2 PBuiltinList))) . pdata . (prememberData (Proxy @PBuiltinList))

instance PIsData (PBuiltinMap k v) where
  pfromData x = punsafeCoerce $ pasMap # pforgetData x
  pdata x = punsafeBuiltin PLC.MapData # x

instance PIsData PInteger where
  pfromData x = pasInt # pforgetData x
  pdata x = punsafeBuiltin PLC.IData # x

instance PIsData PByteString where
  pfromData x = pasByteStr # pforgetData x
  pdata x = punsafeBuiltin PLC.BData # x

{- |
  Instance for PBool following the Plutus IsData repr
  given by @makeIsDataIndexed ''Bool [('False,0),('True,1)]@,
  which is used in 'TxInfo' via 'Closure'.
-}
instance PIsData PBool where
  pfromData x =
    (phoistAcyclic $ plam toBool) # pforgetData x
    where
      toBool :: Term s PData -> Term s PBool
      toBool d = pfstBuiltin # (pasConstr # d) #== 1

  pdata x =
    (phoistAcyclic $ plam toData) # x
    where
      toData :: Term s PBool -> Term s (PAsData PBool)
      toData b =
        punsafeBuiltin PLC.ConstrData
          # (pif' # b # 1 #$ 0 :: Term _ PInteger)
          # nil

      nil :: Term s (PBuiltinList PData)
      nil = pnil

-- | NB: `PAsData (PBuiltinPair (PAsData a) (PAsData b))` and `PAsData (PTuple a b)` have the same representation.
instance PIsData (PBuiltinPair (PAsData a) (PAsData b)) where
  pfromData x = f # x
    where
      f = phoistAcyclic $
        plam $ \pairDat -> plet (psndBuiltin #$ pasConstr # pforgetData pairDat) $
          \pd -> ppairDataBuiltin # punsafeCoerce (phead # pd) #$ punsafeCoerce (phead #$ ptail # pd)
  pdata x = punsafeCoerce target
    where
      target :: Term _ (PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
      target = f # punsafeCoerce x
      f = phoistAcyclic $
        plam $ \pair -> pconstrBuiltin # 0 #$ pcons # (pfstBuiltin # pair) #$ pcons # (psndBuiltin # pair) # pnil

newtype Helper3 f b a s = Helper3 (PAsData (f a b) s)

newtype Helper4 f b a s = Helper4 (f a b s)

instance PIsData (PBuiltinPair PData PData) where
  pfromData = f . pfromData . g
    where
      g :: Term s (PAsData (PBuiltinPair PData PData)) -> Term s (PAsData (PBuiltinPair (PAsData PData) (PAsData PData)))
      g =
        (coerce (prememberData (Proxy @(Helper3 PBuiltinPair (PAsData PData)))) :: Term s (PAsData (PBuiltinPair PData (PAsData PData))) -> Term s (PAsData (PBuiltinPair (PAsData PData) (PAsData PData))))
          . coerce (prememberData (Proxy @(Helper2 (PBuiltinPair PData))))

      f :: Term s (PBuiltinPair (PAsData PData) (PAsData PData)) -> Term s (PBuiltinPair PData PData)
      f =
        coerce (pforgetData' (Proxy @(Helper4 PBuiltinPair PData)))
          . (pforgetData' @PData (Proxy @(PBuiltinPair (PAsData PData))))
  pdata = f . pdata . g
    where
      g :: Term s (PBuiltinPair PData PData) -> Term s (PBuiltinPair (PAsData PData) (PAsData PData))
      g = coerce (prememberData (Proxy @(Helper4 PBuiltinPair (PAsData PData)))) . prememberData (Proxy @(PBuiltinPair PData))

      f :: Term s (PAsData (PBuiltinPair (PAsData PData) (PAsData PData))) -> Term s (PAsData (PBuiltinPair PData PData))
      f =
        (coerce (pforgetData' @PData (Proxy @(Helper3 PBuiltinPair PData))) :: Term s (PAsData (PBuiltinPair (PAsData PData) PData)) -> Term s (PAsData (PBuiltinPair PData PData)))
          . coerce (pforgetData' @PData (Proxy @(Helper2 (PBuiltinPair (PAsData PData)))))

instance PIsData PUnit where
  pfromData _ = pconstant ()
  pdata _ = punsafeCoerce $ pconstant (Constr 0 [])

-- This instance is kind of useless. There's no safe way to use 'pdata'.
instance PIsData (PBuiltinPair PInteger (PBuiltinList PData)) where
  pfromData x = pasConstr # pforgetData x
  pdata x' = plet x' $ \x -> pconstrBuiltin # (pfstBuiltin # x) #$ psndBuiltin # x

instance PEq (PAsData a) where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y

instance (forall (s :: S). Coercible (a s) (Term s b), PIsData b) => PIsData (DerivePNewtype a b) where
  pfromData x = punsafeFrom target
    where
      target :: Term _ b
      target = pfromData $ pinnerData x
  pdata x = pouterData . pdata $ pto x

pinnerData :: Term s (PAsData a) -> Term s (PAsData (PInner a b))
pinnerData = punsafeCoerce

pouterData :: Term s (PAsData (PInner a b)) -> Term s (PAsData a)
pouterData = punsafeCoerce

pconstrBuiltin :: Term s (PInteger :--> PBuiltinList PData :--> PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
pconstrBuiltin = punsafeBuiltin $ PLC.ConstrData

{- | Create a Plutarch-level 'PAsData' constant, from a Haskell value.
Example:
> pconstantData @PInteger 42
-}
pconstantData :: forall p h s. (ToData h, PLifted p ~ h, PConstanted h ~ p) => h -> Term s (PAsData p)
pconstantData x = punsafeCoerce $ pconstant $ PlutusTx.toData x
