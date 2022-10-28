{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.AssocMap (
  PMap (PMap),
  KeyGuarantees (Unsorted, Sorted),

  -- * Creation
  pempty,
  psingleton,
  psingletonData,
  pinsert,
  pinsertData,
  pdelete,
  pfromAscList,
  passertSorted,
  pforgetSorted,

  -- * Lookups
  plookup,
  plookupData,
  pfindWithDefault,
  pfoldAt,
  pnull,

  -- * Folds
  pall,
  pany,

  -- * Filters and traversals
  pfilter,
  pmap,
  pmapData,
  pmapMaybe,
  pmapMaybeData,

  -- * Combining
  pdifference,
  punionWith,
  punionWithData,

  -- * Partial order operations
  pcheckBinRel,
) where

import qualified PlutusLedgerApi.V1 as Plutus
import qualified PlutusTx.AssocMap as PlutusMap
import qualified PlutusTx.Monoid as PlutusTx
import qualified PlutusTx.Semigroup as PlutusTx

import Plutarch.Builtin (
  pasMap,
  pdataImpl,
  pforgetData,
  pfromDataImpl,
  ppairDataBuiltin,
 )
import Plutarch.Internal (punsafeBuiltin)
import Plutarch.Internal.Witness (witness)
import Plutarch.Lift (
  PConstantDecl,
  PConstantRepr,
  PConstanted,
  PLifted,
  PUnsafeLiftDecl,
  pconstantFromRepr,
  pconstantToRepr,
 )
import qualified Plutarch.List as List
import Plutarch.Prelude hiding (pall, pany, pfilter, pmap, pnull, psingleton)
import qualified Plutarch.Prelude as PPrelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import qualified PlutusCore as PLC

import Data.Foldable (foldl')
import GHC.Exts (IsList (Item, fromList, toList))
import Prelude hiding (all, any, filter, lookup, null)

import Data.Proxy (Proxy (Proxy))
import Data.Traversable (for)

data KeyGuarantees = Sorted | Unsorted

type PBuiltinListOfPairs k v = PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

type role PMap nominal nominal nominal nominal
newtype PMap (keysort :: KeyGuarantees) (k :: PType) (v :: PType) (s :: S) = PMap (Term s (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow)
instance DerivePlutusType (PMap keysort k v) where type DPTStrat _ = PlutusTypeNewtype

instance PIsData (PMap keysort k v) where
  pfromDataImpl x = punsafeCoerce $ pasMap # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.MapData # x

instance PEq (PMap 'Sorted k v) where
  x #== y = peqViaData # x # y
    where
      peqViaDataPPlutus' s => Term s (PMap 'Sorted k v #-> PMap 'Sorted k v #-> PBool)
      peqViaData = phoistAcyclic $ plam $ \m0 m1 -> pdata m0 #== pdata m1

instance
  ( PLiftData k
  , PLiftData v
  , Ord (PLifted k)
  ) =>
  PUnsafeLiftDecl (PMap 'Unsorted k v)
  where
  type PLifted (PMap 'Unsorted k v) = PlutusMap.Map (PLifted k) (PLifted v)

instance
  ( PConstantData k
  , PConstantData v
  , Ord k
  ) =>
  PConstantDecl (PlutusMap.Map k v)
  where
  type PConstantRepr (PlutusMap.Map k v) = [(Plutus.Data, Plutus.Data)]
  type PConstanted (PlutusMap.Map k v) = PMap 'Unsorted (PConstanted k) (PConstanted v)
  pconstantToRepr m = (\(x, y) -> (Plutus.toData x, Plutus.toData y)) <$> PlutusMap.toList m
  pconstantFromRepr m = fmap PlutusMap.fromList $
    for m $ \(x, y) -> do
      x' <- Plutus.fromData x
      y' <- Plutus.fromData y
      Just (x', y')

instance
  ( PTryFrom PData (PAsData k)
  , PTryFrom PData (PAsData v)
  ) =>
  PTryFrom PData (PAsData (PMap 'Unsorted k v))
  where
  type PTryFromExcess PData (PAsData (PMap 'Unsorted k v)) = Flip Term (PMap 'Unsorted k v)
  ptryFrom' opq = runTermCont $ do
    opq' <- tcont . plet $ pasMap # opq
    unwrapped <- tcont . plet $ List.pmap # ptryFromPair # opq'
    pure (punsafeCoerce opq, pcon . PMap $ unwrapped)
    where
      ptryFromPairPPlutus' s => Term s (PBuiltinPair PData PData #-> PBuiltinPair (PAsData k) (PAsData v))
      ptryFromPair = plam $ \p ->
        ppairDataBuiltin # ptryFrom (pfstBuiltin # p) fst
          # ptryFrom (psndBuiltin # p) fst

instance
  ( POrd k
  , PIsData k
  , PIsData v
  , PTryFrom PData (PAsData k)
  , PTryFrom PData (PAsData v)
  ) =>
  PTryFrom PData (PAsData (PMap 'Sorted k v))
  where
  type PTryFromExcess PData (PAsData (PMap 'Sorted k v)) = Flip Term (PMap 'Sorted k v)
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PMap 'Unsorted k v)) opq
    unwrapped <- tcont $ plet . papp passertSorted . pfromData $ opq'
    pure (punsafeCoerce opq, unwrapped)

-- | Tests whether the map is empty.
pnullPPlutus' s => Term s (PMap any k v #-> PBool)
pnull = plam (\map -> List.pnull # pto map)

-- | Look up the given key in a 'PMap'.
plookup :: (PIsData k, PIsData v) => Term s (k #-> PMap any k v #-> PMaybe v)
plookup = phoistAcyclic $
  plam $ \key ->
    plookupDataWith
      # phoistAcyclic (plam $ \pair -> pcon $ PJust $ pfromData $ psndBuiltin # pair)
      # pdata key

-- | Look up the given key data in a 'PMap'.
plookupDataPPlutus' s => Term s (PAsData k #-> PMap any k v #-> PMaybe (PAsData v))
plookupData = plookupDataWith # phoistAcyclic (plam $ \pair -> pcon $ PJust $ psndBuiltin # pair)

-- | Look up the given key data in a 'PMap', applying the given function to the found key-value pair.
plookupDataWith ::
  Term
    s
    ( (PBuiltinPair (PAsData k) (PAsData v) #-> PMaybe x)
        #-> PAsData k
        #-> PMap any k v
        #-> PMaybe x
    )
plookupDataWith = phoistAcyclic $
  plam $ \unwrap key map ->
    precList
      ( \self x xs ->
          pif
            (pfstBuiltin # x #== key)
            (unwrap # x)
            (self # xs)
      )
      (const $ pcon PNothing)
      # pto map

-- | Look up the given key in a 'PMap', returning the default value if the key is absent.
pfindWithDefault :: (PIsData k, PIsData v) => Term s (v #-> k #-> PMap any k v #-> v)
pfindWithDefault = phoistAcyclic $ plam $ \def key -> foldAtData # pdata key # def # plam pfromData

{- | Look up the given key in a 'PMap'; return the default if the key is
 absent or apply the argument function to the value data if present.
-}
pfoldAt :: PIsData k => Term s (k #-> r #-> (PAsData v #-> r) #-> PMap any k v #-> r)
pfoldAt = phoistAcyclic $
  plam $ \key -> foldAtData # pdata key

{- | Look up the given key data in a 'PMap'; return the default if the key is
 absent or apply the argument function to the value data if present.
-}
foldAtDataPPlutus' s => Term s (PAsData k #-> r #-> (PAsData v #-> r) #-> PMap any k v #-> r)
foldAtData = phoistAcyclic $
  plam $ \key def apply map ->
    precList
      ( \self x xs ->
          pif
            (pfstBuiltin # x #== key)
            (apply #$ psndBuiltin # x)
            (self # xs)
      )
      (const def)
      # pto map

-- | Insert a new key/value pair into the map, overiding the previous if any.
pinsert :: (POrd k, PIsData k, PIsData v) => Term s (k #-> v #-> PMap 'Sorted k v #-> PMap 'Sorted k v)
pinsert = phoistAcyclic $
  plam $ \key val ->
    rebuildAtKey # plam (pcons # (ppairDataBuiltin # pdata key # pdata val) #) # key

-- | Insert a new data-encoded key/value pair into the map, overiding the previous if any.
pinsertData ::
  (POrd k, PIsData k) =>
  Term s (PAsData k #-> PAsData v #-> PMap 'Sorted k v #-> PMap 'Sorted k v)
pinsertData = phoistAcyclic $
  plam $ \key val ->
    rebuildAtKey # plam (pcons # (ppairDataBuiltin # key # val) #) # pfromData key

-- | Delete a key from the map.
pdelete :: (POrd k, PIsData k) => Term s (k #-> PMap 'Sorted k v #-> PMap 'Sorted k v)
pdelete = rebuildAtKey # plam id

-- | Rebuild the map at the given key.
rebuildAtKey ::
  (POrd k, PIsData k) =>
  Term
    s
    ( ( PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
          #-> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
      )
        #-> k
        #-> PMap g k v
        #-> PMap g k v
    )
rebuildAtKey = phoistAcyclic $
  plam $ \handler key map ->
    punsafeDowncast $
      precList
        ( \self x xs ->
            plet (pfromData $ pfstBuiltin # x) $ \k ->
              plam $ \prefix ->
                pif
                  (k #< key)
                  (self # xs #$ plam $ \suffix -> prefix #$ pcons # x # suffix)
                  ( pif
                      (k #== key)
                      (prefix #$ handler # xs)
                      (prefix #$ handler #$ pcons # x # xs)
                  )
        )
        (const $ plam (#$ handler # pnil))
        # pto map
        # plam id

-- | Construct an empty 'PMap'.
pemptyPPlutus' s => Term s (PMap 'Sorted k v)
pempty = punsafeDowncast pnil

-- | Construct a singleton 'PMap' with the given key and value.
psingleton :: (PIsData k, PIsData v) => Term s (k #-> v #-> PMap 'Sorted k v)
psingleton = phoistAcyclic $ plam $ \key value -> psingletonData # pdata key # pdata value

-- | Construct a singleton 'PMap' with the given data-encoded key and value.
psingletonDataPPlutus' s => Term s (PAsData k #-> PAsData v #-> PMap 'Sorted k v)
psingletonData = phoistAcyclic $
  plam $ \key value -> punsafeDowncast (pcons # (ppairDataBuiltin # key # value) # pnil)

-- | Construct a 'PMap' from a list of key-value pairs, sorted by ascending key data.
pfromAscList :: (POrd k, PIsData k, PIsData v) => Term s (PBuiltinListOfPairs k v #-> PMap 'Sorted k v)
pfromAscList = plam $ (passertSorted #) . pcon . PMap

-- | Assert the map is properly sorted.
passertSorted :: forall k v any s. (POrd k, PIsData k, PIsData v) => Term s (PMap any k v #-> PMap 'Sorted k v)
passertSorted =
  let _ = witness (Proxy :: Proxy (PIsData v))
   in phoistAcyclic $
        plam $ \map ->
          precList
            ( \self x xs ->
                plet (pfromData $ pfstBuiltin # x) $ \k ->
                  plam $ \badKey ->
                    pif
                      (badKey # k)
                      (ptraceError "unsorted map")
                      (self # xs # plam (#< k))
            )
            -- this is actually the empty map so we can
            -- safely assum that it is sorted
            (const . plam . const $ punsafeCoerce map)
            # pto map
            # plam (const $ pcon PFalse)

-- | Forget the knowledge that keys were sorted.
pforgetSortedPPlutus' s => Term s (PMap 'Sorted k v) -> Term s (PMap g k v)
pforgetSorted v = punsafeDowncast (pto v)

{- | Given a 'Foldable' of key-value pairs, construct an unsorted 'PMap'.
 Performs linearly with respect to its argument.

 = Note

 If there are duplicate keys in the input, the /last/ key will \'win\' in a
 lookup.
-}
punsortedMapFromFoldable ::
  forall (k :: PType) (v :: PType) (f :: Type -> Type) (s :: S).
  (Foldable f, PIsData k, PIsData v) =>
  f (Term s k, Term s v) ->
  Term s (PMap 'Unsorted k v)
punsortedMapFromFoldable = pcon . PMap . foldl' go (pcon PNil)
  where
    go ::
      forall (s' :: S).
      Term s' (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))) ->
      (Term s' k, Term s' v) ->
      Term s' (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v)))
    go acc (key, val) =
      pcon . PCons (ppairDataBuiltin # pdata key # pdata val) $ acc

{- | Given a 'Foldable' of (not necessarily sorted) key-value pairs, construct a
 'PMap' which is guaranteed sorted. Performs a linear number of ordered
 insertions with respect to the length of its argument.

 = Note

 If there are duplicate keys, only the /last/ key-value pair will remain in
 the result.
-}
psortedMapFromFoldable ::
  forall (k :: PType) (v :: PType) (f :: Type -> Type) (s :: S).
  (Foldable f, POrd k, PIsData k, PIsData v) =>
  f (Term s k, Term s v) ->
  Term s (PMap 'Sorted k v)
psortedMapFromFoldable = foldl' go pempty
  where
    go ::
      forall (s' :: S).
      Term s' (PMap 'Sorted k v) ->
      (Term s' k, Term s' v) ->
      Term s' (PMap 'Sorted k v)
    go acc (key, val) = pinsert # key # val # acc

instance (PIsData k, PIsData v, POrd k) => IsList (Term s (PMap 'Unsorted k v)) where
  type Item (Term s (PMap 'Unsorted k v)) = (Term s k, Term s v)
  fromList = punsortedMapFromFoldable
  toList = error "unimplemented"

instance (PIsData k, PIsData v, POrd k) => IsList (Term s (PMap 'Sorted k v)) where
  type Item (Term s (PMap 'Sorted k v)) = (Term s k, Term s v)
  fromList = psortedMapFromFoldable
  toList = error "unimplemented"

instance
  (POrd k, PIsData k, PIsData v, Semigroup (Term s v)) =>
  Semigroup (Term s (PMap 'Sorted k v))
  where
  a <> b = punionWith # plam (<>) # a # b

instance
  (POrd k, PIsData k, PIsData v, Semigroup (Term s v)) =>
  Monoid (Term s (PMap 'Sorted k v))
  where
  mempty = pempty

instance
  (POrd k, PIsData k, PIsData v, PlutusTx.Semigroup (Term s v)) =>
  PlutusTx.Semigroup (Term s (PMap 'Sorted k v))
  where
  a <> b = punionWith # plam (PlutusTx.<>) # a # b

instance
  (POrd k, PIsData k, PIsData v, PlutusTx.Semigroup (Term s v)) =>
  PlutusTx.Monoid (Term s (PMap 'Sorted k v))
  where
  mempty = pempty

instance
  (POrd k, PIsData k, PIsData v, PlutusTx.Group (Term s v)) =>
  PlutusTx.Group (Term s (PMap 'Sorted k v))
  where
  inv a = pmap # plam PlutusTx.inv # a

{- | Combine two 'PMap's applying the given function to any two values that
 share the same key.
-}
punionWith ::
  (POrd k, PIsData k, PIsData v) =>
  Term s ((v #-> v #-> v) #-> PMap 'Sorted k v #-> PMap 'Sorted k v #-> PMap 'Sorted k v)
punionWith = phoistAcyclic $
  plam $
    \combine -> punionWithData #$ plam $
      \x y -> pdata (combine # pfromData x # pfromData y)

data MapUnionCarrier k v s = MapUnionCarrier
  { mergePPlutus' s => Term s (PBuiltinListOfPairs k v #-> PBuiltinListOfPairs k v #-> PBuiltinListOfPairs k v)
  , mergeInsertPPlutus' s => Term s (PBuiltinPair (PAsData k) (PAsData v) #-> PBuiltinListOfPairs k v #-> PBuiltinListOfPairs k v #-> PBuiltinListOfPairs k v)
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)
instance DerivePlutusType (MapUnionCarrier k v) where type DPTStrat _ = PlutusTypeScott

mapUnionCarrier :: (POrd k, PIsData k) => Term s ((PAsData v #-> PAsData v #-> PAsData v) #-> MapUnionCarrier k v #-> MapUnionCarrier k v)
mapUnionCarrier = phoistAcyclic $ plam \combine self ->
  let mergeInsert = pmatch self \(MapUnionCarrier {mergeInsert}) -> mergeInsert
      merge = pmatch self \(MapUnionCarrier {merge}) -> merge
   in pcon $
        MapUnionCarrier
          { merge = plam $ \xs ys -> pmatch xs $ \case
              PNil -> ys
              PCons x xs' -> mergeInsert # x # xs' # ys
          , mergeInsert = plam $ \x xs ys ->
              pmatch ys $ \case
                PNil -> pcons # x # xs
                PCons y1 ys' ->
                  plet y1 $ \y ->
                    plet (pfstBuiltin # x) $ \xk ->
                      plet (pfstBuiltin # y) $ \yk ->
                        pif
                          (xk #== yk)
                          ( pcons
                              # (ppairDataBuiltin # xk #$ combine # (psndBuiltin # x) # (psndBuiltin # y))
                              #$ merge
                              # xs
                              # ys'
                          )
                          ( pif
                              (pfromData xk #< pfromData yk)
                              ( pcons
                                  # x
                                  # (mergeInsert # y # ys' # xs)
                              )
                              ( pcons
                                  # y
                                  # (mergeInsert # x # xs # ys')
                              )
                          )
          }

mapUnion :: forall k v s. (POrd k, PIsData k) => Term s ((PAsData v #-> PAsData v #-> PAsData v) #-> MapUnionCarrier k v)
mapUnion = phoistAcyclic $ plam \combine -> punsafeCoerce pfix # (mapUnionCarrier # combine :: Term _ (MapUnionCarrier k v #-> MapUnionCarrier k v))

{- | Combine two 'PMap's applying the given function to any two data-encoded
 values that share the same key.
-}
punionWithData ::
  (POrd k, PIsData k) =>
  Term
    s
    ( (PAsData v #-> PAsData v #-> PAsData v)
        #-> PMap 'Sorted k v
        #-> PMap 'Sorted k v
        #-> PMap 'Sorted k v
    )
punionWithData = phoistAcyclic $
  plam $ \combine x y ->
    pcon $ PMap $ (pmatch (mapUnion # combine) \(MapUnionCarrier {merge}) -> merge) # pto x # pto y

-- | Difference of two maps. Return elements of the first map not existing in the second map.
pdifference :: PIsData k => Term s (PMap g k a #-> PMap any k b #-> PMap g k a)
pdifference = phoistAcyclic $
  plam $ \left right ->
    pcon . PMap $
      precList
        ( \self x xs ->
            plet (self # xs) $ \xs' ->
              pfoldAt
                # pfromData (pfstBuiltin # x)
                # (pcons # x # xs')
                # plam (const xs')
                # right
        )
        (const pnil)
        # pto left

-- | Tests if all values in the map satisfy the given predicate.
pall :: PIsData v => Term s ((v #-> PBool) #-> PMap any k v #-> PBool)
pall = phoistAcyclic $
  plam $ \pred map ->
    List.pall # plam (\pair -> pred #$ pfromData $ psndBuiltin # pair) # pto map

-- | Tests if anu value in the map satisfies the given predicate.
pany :: PIsData v => Term s ((v #-> PBool) #-> PMap any k v #-> PBool)
pany = phoistAcyclic $
  plam $ \pred map ->
    List.pany # plam (\pair -> pred #$ pfromData $ psndBuiltin # pair) # pto map

-- | Filters the map so it contains only the values that satisfy the given predicate.
pfilter :: PIsData v => Term s ((v #-> PBool) #-> PMap g k v #-> PMap g k v)
pfilter = phoistAcyclic $
  plam $ \pred ->
    pmapMaybe #$ plam $ \v -> pif (pred # v) (pcon $ PJust v) (pcon PNothing)

-- | Maps and filters the map, much like 'Data.List.mapMaybe'.
pmapMaybe ::
  (PIsData a, PIsData b) =>
  Term s ((a #-> PMaybe b) #-> PMap g k a #-> PMap g k b)
pmapMaybe = phoistAcyclic $
  plam $ \f -> pmapMaybeData #$ plam $ \v -> pmatch (f # pfromData v) $ \case
    PNothing -> pcon PNothing
    PJust v' -> pcon $ PJust (pdata v')

pmapMaybeData ::
  Term s ((PAsData a #-> PMaybe (PAsData b)) #-> PMap g k a #-> PMap g k b)
pmapMaybeData = phoistAcyclic $
  plam $ \f map ->
    pcon . PMap $
      precList
        ( \self x xs ->
            plet (self # xs) $ \xs' ->
              pmatch (f #$ psndBuiltin # x) $ \case
                PNothing -> xs'
                PJust v -> pcons # (ppairDataBuiltin # (pfstBuiltin # x) # v) # xs'
        )
        (const pnil)
        # pto map

-- | Applies a function to every value in the map, much like 'Data.List.map'.
pmap ::
  (PIsData a, PIsData b) =>
  Term s ((a #-> b) #-> PMap g k a #-> PMap g k b)
pmap = phoistAcyclic $
  plam $ \f -> pmapData #$ plam $ \v -> pdata (f # pfromData v)

pmapData ::
  Term s ((PAsData a #-> PAsData b) #-> PMap g k a #-> PMap g k b)
pmapData = phoistAcyclic $
  plam $ \f map ->
    pcon . PMap $
      precList
        ( \self x xs ->
            pcons
              # (ppairDataBuiltin # (pfstBuiltin # x) # (f #$ psndBuiltin # x))
              # (self # xs)
        )
        (const pnil)
        # pto map

{- | Given a comparison function and a "zero" value, check whether a binary relation holds over
2 sorted 'PMap's.

This is primarily intended to be used with 'PValue'.
-}
pcheckBinRel ::
  forall k v s.
  (POrd k, PIsData k, PIsData v) =>
  Term
    s
    ( (v #-> v #-> PBool)
        #-> v
        #-> PMap 'Sorted k v
        #-> PMap 'Sorted k v
        #-> PBool
    )
pcheckBinRel = phoistAcyclic $
  plam $ \f z m1 m2 ->
    let inner = pfix #$ plam $ \self l1 l2 ->
          pelimList
            ( \x xs ->
                plet (pfromData $ psndBuiltin # x) $ \v1 ->
                  pelimList
                    ( \y ys -> unTermCont $ do
                        v2 <- tcont . plet . pfromData $ psndBuiltin # y
                        k1 <- tcont . plet . pfromData $ pfstBuiltin # x
                        k2 <- tcont . plet . pfromData $ pfstBuiltin # y
                        pure $
                          pif
                            (k1 #== k2)
                            ( f # v1 # v2 #&& self
                                # xs
                                # ys
                            )
                            $ pif
                              (k1 #< k2)
                              (f # v1 # z #&& self # xs # l2)
                              $ f # z # v2 #&& self
                                # l1
                                # ys
                    )
                    ( f # v1 # z
                        #&& PPrelude.pall
                          # plam (\p -> f # pfromData (psndBuiltin # p) # z)
                          # xs
                    )
                    l2
            )
            (PPrelude.pall # plam (\p -> f # z #$ pfromData $ psndBuiltin # p) # l2)
            l1
     in inner # pto m1 # pto m2
