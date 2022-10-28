{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Builtin2 () where
--   PData,
--   pfstBuiltin,
--   psndBuiltin,
--   pasConstr,
--   pasMap,
--   pasList,
--   pasInt,
--   plistData,
--   pconstantData,
--   pconstrBuiltin,
--   pasByteStr,
--   PBuiltinPair,
--   PBuiltinList (..),
--   pdataLiteral,
--   PIsData (..),
--   pdata,
--   pfromData,
--   PAsData,
--   pforgetData,
--   prememberData,
--   prememberData',
--   pserialiseData,
--   ppairDataBuiltin,
--   pchooseListBuiltin,
--   pchooseData,
-- ) where

-- import Data.Functor.Const (Const)
-- import Data.Proxy (Proxy (Proxy))
-- import GHC.Generics (Generic)
-- import Plutarch (
--   DPTStrat,
--   DerivePlutusType,
--   PContravariant',
--   PCovariant,
--   PCovariant',
--   PInner,
--   PType,
--   PVariant,
--   PVariant',
--   PlutusType,
--   PlutusTypeNewtype,
--   S,
--   Term,
--   pcon,
--   pdelay,
--   pfix,
--   pforce,
--   phoistAcyclic,
--   plam,
--   plet,
--   pmatch,
--   pto,
--   (#),
--   (#$),
--   type (#->),
--  )
-- import Plutarch.Bool (PBool (..), PEq, pif, pif', (#&&), (#==))
-- import Plutarch.ByteString (PByteString)
-- import Plutarch.Integer (PInteger)
-- import Plutarch.Internal.PlutusType (pcon', pmatch')
-- import Plutarch.Internal.Witness (witness)
-- import Plutarch.Lift (
--   DerivePConstantDirect (DerivePConstantDirect),
--   PConstant,
--   PConstantDecl,
--   PConstantRepr,
--   PConstanted,
--   PLift,
--   PLifted,
--   PUnsafeLiftDecl,
--   pconstant,
--   pconstantFromRepr,
--   pconstantToRepr,
--  )
-- import Plutarch.List (
--   PListLike (
--     PElemConstraint,
--     pcons,
--     pelimList,
--     phead,
--     pnil,
--     pnull,
--     ptail
--   ),
--   pfoldr',
--   phead,
--   plistEquals,
--   pmap,
--   pshowList,
--   ptail,
--  )
-- import Plutarch.Show (PShow (pshow'), pshow)
-- import Plutarch.String (PString)
-- import Plutarch.TermCont (TermCont (runTermCont), tcont, unTermCont)
-- import Plutarch.TryFrom (PSubtype, PTryFrom, PTryFromExcess, ptryFrom, ptryFrom', pupcast, pupcastF)
-- import Plutarch.Unit (PUnit)
-- import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce, punsafeDowncast)
-- import qualified PlutusCore as PLC
-- import PlutusTx (Data (Constr), ToData)
-- import qualified PlutusTx

import Plutarch.Core

-- | Plutus 'BuiltinPair'
type PBuiltinPair :: PDSLKind -> PType -> PType -> PType
data PBuiltinPair edsl a b ef = PBuiltinPair (Term edsl (PBuiltinPair edsl a b))

-- instance PlutusType (PBuiltinPair a b) where
--   type PInner (PBuiltinPair a b) = PBuiltinPair a b
--   type PCovariant' (PBuiltinPair a b) = (PCovariant' a, PCovariant' b)
--   type PContravariant' (PBuiltinPair a b) = (PContravariant' a, PContravariant' b)
--   type PVariant' (PBuiltinPair a b) = (PVariant' a, PVariant' b)
--   pcon' (PBuiltinPair x) = x
--   pmatch' x f = f (PBuiltinPair x)

-- instance (PLift a, PLift b) => PUnsafeLiftDecl (PBuiltinPair a b) where
--   type PLifted (PBuiltinPair a b) = (PLifted a, PLifted b)

-- -- FIXME: figure out good way of deriving this
-- instance (PConstant a, PConstant b) => PConstantDecl (a, b) where
--   type PConstantRepr (a, b) = (PConstantRepr a, PConstantRepr b)
--   type PConstanted (a, b) = PBuiltinPair (PConstanted a) (PConstanted b)
--   pconstantToRepr (x, y) = (pconstantToRepr x, pconstantToRepr y)
--   pconstantFromRepr (x, y) = do
--     x' <- pconstantFromRepr @a x
--     y' <- pconstantFromRepr @b y
--     Just (x', y')

-- pfstBuiltinPPlutus' s => Term s (PBuiltinPair a b #-> a)
-- pfstBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.FstPair

-- psndBuiltinPPlutus' s => Term s (PBuiltinPair a b #-> b)
-- psndBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.SndPair

-- {- | Construct a builtin pair of 'PData' elements.

-- Uses 'PAsData' to preserve more information about the underlying 'PData'.
-- -}
-- ppairDataBuiltinPPlutus' s => Term s (PAsData a #-> PAsData b #-> PBuiltinPair (PAsData a) (PAsData b))
-- ppairDataBuiltin = punsafeBuiltin PLC.MkPairData

-- -- | Plutus 'BuiltinList'
-- data PBuiltinList (a :: PType) (s :: S)
--   = PCons (Term s a) (Term s (PBuiltinList a))
--   | PNil

-- instance (PShow a, PLift a) => PShow (PBuiltinList a) where
--   pshow' _ x = pshowList @PBuiltinList @a # x

-- pheadBuiltinPPlutus' s => Term s (PBuiltinList a #-> a)
-- pheadBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.HeadList

-- ptailBuiltinPPlutus' s => Term s (PBuiltinList a #-> PBuiltinList a)
-- ptailBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.TailList

-- pchooseListBuiltinPPlutus' s => Term s (PBuiltinList a #-> b #-> b #-> b)
-- pchooseListBuiltin = phoistAcyclic $ pforce $ pforce $ punsafeBuiltin PLC.ChooseList

-- pnullBuiltinPPlutus' s => Term s (PBuiltinList a #-> PBool)
-- pnullBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.NullList

-- pconsBuiltinPPlutus' s => Term s (a #-> PBuiltinList a #-> PBuiltinList a)
-- pconsBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.MkCons

-- instance PConstant a => PConstantDecl [a] where
--   type PConstantRepr [a] = [PConstantRepr a]
--   type PConstanted [a] = PBuiltinList (PConstanted a)
--   pconstantToRepr x = pconstantToRepr <$> x
--   pconstantFromRepr = traverse (pconstantFromRepr @a)

-- instance PUnsafeLiftDecl a => PUnsafeLiftDecl (PBuiltinList a) where
--   type PLifted (PBuiltinList a) = [PLifted a]

-- instance PLift a => PlutusType (PBuiltinList a) where
--   type PInner (PBuiltinList a) = PBuiltinList a
--   type PCovariant' (PBuiltinList a) = PCovariant' a
--   type PContravariant' (PBuiltinList a) = PContravariant' a
--   type PVariant' (PBuiltinList a) = PVariant' a
--   pcon' (PCons x xs) = pconsBuiltin # x # xs
--   pcon' PNil = pconstant []
--   pmatch' xs' f = plet xs' $ \xs ->
--     pforce $
--       pchooseListBuiltin
--         # xs
--         # pdelay (f PNil)
--         # pdelay (f (PCons (pheadBuiltin # xs) (ptailBuiltin # xs)))

-- instance PListLike PBuiltinList where
--   type PElemConstraint PBuiltinList a = PLift a

--   pelimList match_cons match_nil ls = pmatch ls $ \case
--     PCons x xs -> match_cons x xs
--     PNil -> match_nil
--   pcons = plam $ \x xs -> pcon (PCons x xs)
--   pnil = pcon PNil
--   phead = pheadBuiltin
--   ptail = ptailBuiltin
--   pnull = pnullBuiltin

-- type family F (a :: PType) :: Bool where
--   F PData = 'True
--   F (PAsData _) = 'True
--   F _ = 'False

-- class Fc (x :: Bool) (a :: PType) where
--   fc :: Proxy x -> Term s (PBuiltinList a) -> Term s (PBuiltinList a) -> Term s PBool

-- instance (PLift a, PEq a) => Fc 'False a where
--   fc _ xs ys = plistEquals # xs # ys

-- instance PIsData (PBuiltinList a) => Fc 'True a where
--   fc _ xs ys = pdata xs #== pdata ys

-- instance Fc (F a) a => PEq (PBuiltinList a) where
--   (#==) = fc (Proxy @(F a))

-- data PData (s :: S) = PData (Term s PData)

-- instance PShow PData where
--   pshow' b t0 = wrap (go0 # t0)
--     where
--       wrap s = pif (pconstant b) ("(" <> s <> ")") s
--       go0PPlutus' s => Term s (PData #-> PString)
--       go0 = phoistAcyclic $
--         pfix #$ plam $ \go t ->
--           let pshowConstr pp0 = plet pp0 $ \pp ->
--                 "Constr "
--                   <> pshow' False (pfstBuiltin # pp)
--                   <> " "
--                   <> pshowListPString # (pmap # go # (psndBuiltin # pp))
--               pshowMap pplist =
--                 "Map " <> pshowListPString # (pmap # pshowPair # pplist)
--               pshowPair = plam $ \pp0 -> plet pp0 $ \pp ->
--                 "(" <> (go # (pfstBuiltin # pp))
--                   <> ", "
--                   <> (go # (psndBuiltin # pp))
--                   <> ")"
--               pshowList xs = "List " <> pshowListPString # (pmap # go # xs)
--               pshowListPString = phoistAcyclic $
--                 plam $ \plist ->
--                   "["
--                     <> pelimList
--                       ( \x0 xs0 ->
--                           x0 <> (pfoldr' (\x r -> ", " <> x <> r) # (""PPlutus' s => Term s PString) # xs0)
--                       )
--                       ""
--                       plist
--                     <> "]"
--            in pforce $
--                 pchooseData
--                   # t
--                   # pdelay (pshowConstr (pasConstr # t))
--                   # pdelay (pshowMap (pasMap # t))
--                   # pdelay (pshowList (pasList # t))
--                   # pdelay ("I " <> pshow (pasInt # t))
--                   # pdelay ("B " <> pshow (pasByteStr # t))

-- instance PlutusType PData where
--   type PInner PData = PData
--   type PCovariant' PData = ()
--   type PContravariant' PData = ()
--   type PVariant' PData = ()
--   pcon' (PData t) = t
--   pmatch' t f = f (PData t)

-- instance PUnsafeLiftDecl PData where type PLifted PData = Data
-- deriving via (DerivePConstantDirect Data PData) instance PConstantDecl Data

-- instance PEq PData where
--   x #== y = punsafeBuiltin PLC.EqualsData # x # y

-- pchooseDataPPlutus' s => Term s (PData #-> a #-> a #-> a #-> a #-> a #-> a)
-- pchooseData = phoistAcyclic $ pforce $ punsafeBuiltin PLC.ChooseData

-- pasConstrPPlutus' s => Term s (PData #-> PBuiltinPair PInteger (PBuiltinList PData))
-- pasConstr = punsafeBuiltin PLC.UnConstrData

-- pasMapPPlutus' s => Term s (PData #-> PBuiltinList (PBuiltinPair PData PData))
-- pasMap = punsafeBuiltin PLC.UnMapData

-- plistDataPPlutus' s => Term s (PBuiltinList PData #-> PData)
-- plistData = punsafeBuiltin PLC.ListData

-- pasListPPlutus' s => Term s (PData #-> PBuiltinList PData)
-- pasList = punsafeBuiltin PLC.UnListData

-- pasIntPPlutus' s => Term s (PData #-> PInteger)
-- pasInt = punsafeBuiltin PLC.UnIData

-- pasByteStrPPlutus' s => Term s (PData #-> PByteString)
-- pasByteStr = punsafeBuiltin PLC.UnBData

-- -- | Serialise any builtin data to its cbor represented by a builtin bytestring
-- pserialiseDataPPlutus' s => Term s (PData #-> PByteString)
-- pserialiseData = punsafeBuiltin PLC.SerialiseData

-- {-# DEPRECATED pdataLiteral "Use `pconstant` instead." #-}
-- pdataLiteral :: Data -> Term s PData
-- pdataLiteral = pconstant

-- data PAsData (a :: PType) (s :: S) = PAsData (Term s a)

-- type family IfSameThenData (a :: PType) (b :: PType) :: PType where
--   IfSameThenData a a = PData
--   IfSameThenData _ b = PAsData b

-- instance PIsData a => PlutusType (PAsData a) where
--   type PInner (PAsData a) = IfSameThenData a (PInner a)
--   type PCovariant' (PAsData a) = PCovariant' a
--   type PContravariant' (PAsData a) = PContravariant' a
--   type PVariant' (PAsData a) = PVariant' a
--   pcon' (PAsData t) = punsafeCoerce $ pdata t
--   pmatch' t f = f (PAsData $ pfromData $ punsafeCoerce t)

-- type role PAsDataLifted nominal
-- data PAsDataLifted (a :: PType)

-- instance PConstantDecl (PAsDataLifted a) where
--   type PConstantRepr (PAsDataLifted a) = Data
--   type PConstanted (PAsDataLifted a) = PAsData a
--   pconstantToRepr = \case {}
--   pconstantFromRepr _ = Nothing

-- instance PUnsafeLiftDecl (PAsData a) where type PLifted (PAsData a) = PAsDataLifted a

-- pforgetData :: forall s a. Term s (PAsData a) -> Term s PData
-- pforgetData = punsafeCoerce

-- -- FIXME: remove, broken

-- {- | Like 'pforgetData', except it works for complex types.
--  Equivalent to 'pupcastF'.
-- -}
-- pforgetData' :: forall a (p :: PType -> PType) s. PCovariant p => Proxy p -> Term s (p (PAsData a)) -> Term s (p PData)
-- pforgetData' _ = let _ = witness (Proxy @(PCovariant p)) in punsafeCoerce

-- -- | Inverse of 'pforgetData''.
-- prememberData :: forall (p :: PType -> PType) s. PVariant p => Proxy p -> Term s (p PData) -> Term s (p (PAsData PData))
-- prememberData Proxy = let _ = witness (Proxy @(PVariant p)) in punsafeCoerce

-- -- | Like 'prememberData' but generalised.
-- prememberData' :: forall a (p :: PType -> PType) s. (PSubtype PData a, PVariant p) => Proxy p -> Term s (p a) -> Term s (p (PAsData a))
-- prememberData' Proxy = let _ = witness (Proxy @(PSubtype PData a, PVariant p)) in punsafeCoerce

-- {- | Laws:
--  - If @PSubtype PData a@, then @pdataImpl a@ must be `pupcast`.
--  - pdataImpl . pupcast . pfromDataImpl ≡ id
--  - pfromDataImpl . punsafeDowncast . pdataImpl ≡ id
-- -}
-- class PIsData a where
--   pfromDataImplPPlutus' s => Term s (PAsData a) -> Term s a
--   default pfromDataImpl :: PIsData (PInner a) => Term s (PAsData a) -> Term s a
--   pfromDataImpl x = punsafeDowncast $ pfromDataImpl (punsafeCoerce x :: Term _ (PAsData (PInner a)))

--   pdataImplPPlutus' s => Term s a -> Term s PData
--   default pdataImpl :: PIsData (PInner a) => Term s a -> Term s PData
--   pdataImpl x = pdataImpl $ pto x

-- pfromData :: PIsData a => Term s (PAsData a) -> Term s a
-- pfromData = pfromDataImpl
-- pdata :: PIsData a => Term s a -> Term s (PAsData a)
-- pdata = punsafeCoerce . pdataImpl

-- instance PIsData PData where
--   pfromDataImpl = pupcast
--   pdataImpl = id

-- instance forall (a :: PType). PSubtype PData a => PIsData (PBuiltinList a) where
--   pfromDataImpl x = punsafeCoerce $ pasList # pforgetData x
--   pdataImpl x = plistData # pupcastF @PData @a (Proxy @PBuiltinList) x

-- newtype Helper2 f a s = Helper2 (Term s (PAsData (f a)))
--   deriving stock (Generic)
--   deriving anyclass (PlutusType)
-- instance DerivePlutusType (Helper2 f a) where type DPTStrat _ = PlutusTypeNewtype

-- instance PIsData PInteger where
--   pfromDataImpl x = pasInt # pforgetData x
--   pdataImpl x = punsafeBuiltin PLC.IData # x

-- instance PIsData PByteString where
--   pfromDataImpl x = pasByteStr # pforgetData x
--   pdataImpl x = punsafeBuiltin PLC.BData # x

-- {- |
--   Instance for PBool following the Plutus IsData repr
--   given by @makeIsDataIndexed ''Bool [('False,0),('True,1)]@,
--   which is used in 'TxInfo' via 'Closure'.
-- -}
-- instance PIsData PBool where
--   pfromDataImpl x =
--     phoistAcyclic (plam toBool) # pforgetData x
--     where
--       toBoolPPlutus' s => Term s PData -> Term s PBool
--       toBool d = pfstBuiltin # (pasConstr # d) #== 1

--   pdataImpl x =
--     phoistAcyclic (plam toData) # x
--     where
--       toDataPPlutus' s => Term s PBool -> Term s PData
--       toData b =
--         punsafeBuiltin PLC.ConstrData
--           # (pif' # b # 1 # (0PPlutus' s => Term s PInteger))
--           # nil

--       nilPPlutus' s => Term s (PBuiltinList PData)
--       nil = pnil

-- -- | NB: `PAsData (PBuiltinPair (PAsData a) (PAsData b))` and `PAsData (PTuple a b)` have the same representation.
-- instance PIsData (PBuiltinPair (PAsData a) (PAsData b)) where
--   pfromDataImpl x = f # x
--     where
--       f = phoistAcyclic $
--         plam $ \pairDat -> plet (psndBuiltin #$ pasConstr # pforgetData pairDat) $
--           \pd -> ppairDataBuiltin # punsafeCoerce (phead # pd) #$ punsafeCoerce (phead #$ ptail # pd)
--   pdataImpl x = pupcast target
--     where
--       target :: Term _ (PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
--       target = f # punsafeCoerce x
--       f = phoistAcyclic $
--         plam $ \pair -> pconstrBuiltin # 0 #$ pcons # (pfstBuiltin # pair) #$ pcons # (psndBuiltin # pair) # pnil

-- newtype Helper3 f b a s = Helper3 (Term s (PAsData (f a b)))
--   deriving stock (Generic)
--   deriving anyclass (PlutusType)
-- instance DerivePlutusType (Helper3 f b a) where type DPTStrat _ = PlutusTypeNewtype

-- newtype Helper4 f b a s = Helper4 (Term s (f a b))
--   deriving stock (Generic)
--   deriving anyclass (PlutusType)
-- instance DerivePlutusType (Helper4 f b a) where type DPTStrat _ = PlutusTypeNewtype

-- instance PIsData (PBuiltinPair PData PData) where
--   pfromDataImpl = f . pfromData . g
--     where
--       gPPlutus' s => Term s (PAsData (PBuiltinPair PData PData)) -> Term s (PAsData (PBuiltinPair (PAsData PData) (PAsData PData)))
--       g x = pto $ prememberData (Proxy @(Helper3 PBuiltinPair (PAsData PData))) $ pcon $ Helper3 $ pto $ prememberData (Proxy @(Helper2 (PBuiltinPair PData))) $ pcon $ Helper2 x

--       fPPlutus' s => Term s (PBuiltinPair (PAsData PData) (PAsData PData)) -> Term s (PBuiltinPair PData PData)
--       f x = pto $ pforgetData' (Proxy @(Helper4 PBuiltinPair PData)) $ pcon $ Helper4 $ pforgetData' @PData (Proxy @(PBuiltinPair (PAsData PData))) x
--   pdataImpl = pupcast . f . pdata . g
--     where
--       gPPlutus' s => Term s (PBuiltinPair PData PData) -> Term s (PBuiltinPair (PAsData PData) (PAsData PData))
--       g x = pto $ prememberData (Proxy @(Helper4 PBuiltinPair (PAsData PData))) $ pcon $ Helper4 $ prememberData (Proxy @(PBuiltinPair PData)) x

--       fPPlutus' s => Term s (PAsData (PBuiltinPair (PAsData PData) (PAsData PData))) -> Term s (PAsData (PBuiltinPair PData PData))
--       f x = pto $ pforgetData' @PData (Proxy @(Helper3 PBuiltinPair PData)) $ pcon $ Helper3 $ pto $ pforgetData' @PData (Proxy @(Helper2 (PBuiltinPair (PAsData PData)))) $ pcon $ Helper2 x

-- instance (PShow a, PShow b) => PShow (PBuiltinPair a b) where
--   pshow' _ pair = "(" <> pshow (pfstBuiltin # pair) <> "," <> pshow (psndBuiltin # pair) <> ")"

-- instance (PEq a, PEq b) => PEq (PBuiltinPair a b) where
--   p1 #== p2 = pfstBuiltin # p1 #== pfstBuiltin # p2 #&& psndBuiltin # p1 #== psndBuiltin # p2

-- instance PIsData PUnit where
--   pfromDataImpl _ = pconstant ()
--   pdataImpl _ = pconstant (Constr 0 [])

-- -- This instance is kind of useless. There's no safe way to use 'pdata'.
-- instance PIsData (PBuiltinPair PInteger (PBuiltinList PData)) where
--   pfromDataImpl x = pasConstr # pupcast x
--   pdataImpl x' = pupcast $ plet x' $ \x -> pconstrBuiltin # (pfstBuiltin # x) #$ psndBuiltin # x

-- instance PEq (PAsData a) where
--   x #== y = punsafeBuiltin PLC.EqualsData # x # y

-- instance (PIsData a, PShow a) => PShow (PAsData a) where
--   pshow' w x = pshow' w (pfromData x)

-- pconstrBuiltinPPlutus' s => Term s (PInteger #-> PBuiltinList PData #-> PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
-- pconstrBuiltin = punsafeBuiltin PLC.ConstrData

-- {- | Create a Plutarch-level 'PAsData' constant, from a Haskell value.
-- Example:
-- > pconstantData @PInteger 42
-- -}
-- pconstantData :: forall p h s. (ToData h, PLifted p ~ h, PConstanted h ~ p) => h -> Term s (PAsData p)
-- pconstantData x = let _ = witness (Proxy @(PLifted p ~ h, PConstanted h ~ p)) in punsafeCoerce $ pconstant $ PlutusTx.toData x

-- newtype Flip f a b = Flip (f b a) deriving stock (Generic)

-- instance PTryFrom PData (PAsData PInteger) where
--   type PTryFromExcess PData (PAsData PInteger) = Flip Term PInteger
--   ptryFrom' opq = runTermCont $ do
--     ver <- tcont $ plet (pasInt # opq)
--     pure (punsafeCoerce opq, ver)

-- instance PTryFrom PData (PAsData PByteString) where
--   type PTryFromExcess PData (PAsData PByteString) = Flip Term PByteString
--   ptryFrom' opq = runTermCont $ do
--     ver <- tcont $ plet (pasByteStr # opq)
--     pure (punsafeCoerce opq, ver)

-- {- |
--     This verifies a list to be indeed a list but doesn't recover the inner data
--     use this instance instead of the one for `PData (PAsData (PBuiltinList (PAsData a)))`
--     as this is O(1) instead of O(n)
-- -}

-- -- TODO: add the excess inner type list
-- instance PTryFrom PData (PAsData (PBuiltinList PData)) where
--   type PTryFromExcess PData (PAsData (PBuiltinList PData)) = Flip Term (PBuiltinList PData)
--   ptryFrom' opq = runTermCont $ do
--     ver <- tcont $ plet (pasList # opq)
--     pure (punsafeCoerce opq, ver)

-- {- |
--     Recover a `PBuiltinList (PAsData a)`
-- -}
-- instance
--   ( PTryFrom PData (PAsData a)
--   , PIsData a
--   ) =>
--   PTryFrom PData (PAsData (PBuiltinList (PAsData a)))
--   where
--   type PTryFromExcess PData (PAsData (PBuiltinList (PAsData a))) = Flip Term (PBuiltinList (PAsData a))
--   ptryFrom' opq = runTermCont $ do
--     let lst :: Term _ (PBuiltinList PData)
--         lst = pasList # opq
--         verify :: Term _ (PData #-> PAsData a)
--         verify = plam $ \e ->
--           unTermCont $ do
--             (wrapped, _) <- tcont $ ptryFrom @(PAsData a) $ e
--             pure wrapped
--     ver <- tcont $ plet $ pmap # verify # lst
--     pure (punsafeCoerce opq, ver)

-- {- |
--     Recover a `PAsData (PBuiltinPair a b)`
-- -}
-- instance
--   ( PTryFrom PData a
--   , a ~ PAsData a'
--   , PIsData a'
--   , PTryFrom PData b
--   , b ~ PAsData b'
--   , PIsData b'
--   ) =>
--   PTryFrom PData (PAsData (PBuiltinPair a b))
--   where
--   type PTryFromExcess PData (PAsData (PBuiltinPair a b)) = Flip Term (PBuiltinPair a b)
--   ptryFrom' opq = runTermCont $ do
--     tup <- tcont $ plet (pfromData $ punsafeCoerce opq)
--     let fst' :: Term _ a
--         fst' = unTermCont $ fst <$> tcont (ptryFrom @a $ pforgetData $ pfstBuiltin # tup)
--         snd' :: Term _ b
--         snd' = unTermCont $ fst <$> tcont (ptryFrom @b $ pforgetData $ psndBuiltin # tup)
--     ver <- tcont $ plet $ ppairDataBuiltin # fst' # snd'
--     pure (punsafeCoerce opq, ver)

-- ----------------------- other utility functions -----------------------------------------

-- instance PTryFrom PData (PAsData PData) where
--   type PTryFromExcess PData (PAsData PData) = Const ()
--   ptryFrom' opq = runTermCont $ pure (pdata opq, ())

-- instance PTryFrom PData PData where
--   type PTryFromExcess PData PData = Const ()
--   ptryFrom' opq f = f (opq, ())
