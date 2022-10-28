{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}

module Plutarch.Extra.Interval (
  pmember,
  pinterval,
  pfrom,
  pto,
  palways,
  pnever,
  psingleton,
  phull,
  pintersection,
  pcontains,
  pbefore,
  pafter,
) where

import Plutarch.Api.V1.Interval (
  PClosure,
  PExtended (PFinite, PNegInf, PPosInf),
  PInterval (PInterval),
  PLowerBound (PLowerBound),
  PUpperBound (PUpperBound),
 )
import Plutarch.Bool (pif')
import qualified Plutarch.Monadic as P
import Plutarch.Prelude hiding (psingleton, pto)
import qualified PlutusLedgerApi.V1.Interval as Plutus

-- check if `a` belongs to interval `i`
pmember ::
  forall a (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term
    s
    ( PAsData a
        #-> PInterval a
        #-> PBool
    )
pmember = phoistAcyclic $ plam $ \a i -> pcontains # i # (psingleton # a)

{- | create an interval that includes all values that are greater than or equal
 - to a and smaller than or equal to b
-}
pinterval ::
  forall a (s :: S).
  PIsData a =>
  Term
    s
    ( PAsData a
        #-> PAsData a
        #-> PInterval a
    )
pinterval = phoistAcyclic $
  plam $ \a b ->
    let start :: Term _ (PExtended a)
        start = pcon $ PFinite $ pdcons @"_0" # a # pdnil

        end :: Term _ (PExtended a)
        end = pcon $ PFinite $ pdcons @"_0" # b # pdnil
     in pclosedInterval # start # end

{- | create an interval that includes all values that are greater than or equal
 - to a
-}
pfrom :: forall a s. PIsData a => Term s (PAsData a #-> PInterval a)
pfrom = phoistAcyclic $
  plam $ \a ->
    let start :: Term _ (PExtended a)
        start = pcon $ PFinite $ pdcons @"_0" # a # pdnil
        end :: Term _ (PExtended a)
        end = pcon $ PPosInf pdnil
     in pclosedInterval # start # end

{- | create an interval that includes all values that are smaller than or equal
 - to a
-}
pto :: forall a (s :: S). PIsData a => Term s (PAsData a #-> PInterval a)
pto = phoistAcyclic $
  plam $ \a ->
    let start :: Term _ (PExtended a)
        start = pcon $ PNegInf pdnil

        end :: Term _ (PExtended a)
        end = pcon $ PFinite $ pdcons @"_0" # a # pdnil
     in pclosedInterval # start # end

-- | create an interval that covers every slot
palways :: forall a (s :: S). (PIsData a, PLiftData a) => Term s (PInterval a)
palways = pconstant Plutus.always

-- | create an interval that is empty
pnever :: forall a (s :: S). (PIsData a, PLiftData a) => Term s (PInterval a)
pnever = pconstant Plutus.never

-- | create and interval [a, a]
psingleton :: forall a (s :: S). PIsData a => Term s (PAsData a #-> PInterval a)
psingleton = phoistAcyclic $
  plam $ \a ->
    plet (pcon $ PFinite $ pdcons @"_0" # a # pdnil) $ \start ->
      pclosedInterval # start # start

-- | `hull i1 i2` is the smallest interval containing `i1` and `i2`
phull ::
  forall a (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term
    s
    ( PInterval a
        #-> PInterval a
        #-> PInterval a
    )
phull = phoistAcyclic $
  plam $ \x' y' -> P.do
    x <- pletFields @'["from", "to"] x'
    y <- pletFields @'["from", "to"] y'

    let lowerX = x.from
        upperX = x.to
        lowerY = y.from
        upperY = y.to

        lower = pcon $ PLowerBound $ minP # (lToE # lowerX) # (lToE # lowerY)
        upper = pcon $ PUpperBound $ maxP # (uToE # upperX) # (uToE # upperY)

    pinterval' # pdata lower # pdata upper

-- | `intersecion i1 i2` is the largest interval contained in `i1` and `i2`
pintersection ::
  forall a (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term
    s
    ( PInterval a
        #-> PInterval a
        #-> PInterval a
    )
pintersection = phoistAcyclic $
  plam $ \x' y' -> P.do
    x <- pletFields @'["from", "to"] x'
    y <- pletFields @'["from", "to"] y'

    let lowerX = x.from
        upperX = x.to

        lowerY = y.from
        upperY = y.to

        lower = pcon $ PLowerBound $ maxP # (lToE # lowerX) # (lToE # lowerY)
        upper = pcon $ PUpperBound $ minP # (uToE # upperX) # (uToE # upperY)

    pinterval' # pdata lower # pdata upper

-- | pcontains # a # b is true if the interval `b` is entirely contained in `a`
pcontains ::
  forall a (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term
    s
    ( PInterval a
        #-> PInterval a
        #-> PBool
    )
pcontains = phoistAcyclic $
  plam $ \x' y' -> P.do
    x <- pletFields @'["from", "to"] x'
    y <- pletFields @'["from", "to"] y'
    let lowerX = x.from
        upperX = x.to

        lowerY = y.from
        upperY = y.to

    leqP # (lToE # lowerX) # (lToE # lowerY) #&& leqP # (uToE # upperY) # (uToE # upperX)

-- | `a` before interval `i` is true if `a` is earlier than the start of `i`
pbefore ::
  forall a (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term
    s
    ( a
        #-> PInterval a
        #-> PBool
    )
pbefore = phoistAcyclic $
  plam $ \a y ->
    let lower = pfield @"from" # y
     in pbefore' # a # (lToE # lower)

-- | `a` after interval `i` is true if `a` is later than the end of `i`
pafter ::
  forall a s.
  (PEq a, POrd a, PIsData a) =>
  Term
    s
    ( a
        #-> PInterval a
        #-> PBool
    )
pafter = phoistAcyclic $
  plam $ \a y ->
    let upper = pfield @"to" # y
     in pafter' # a # (uToE # upper)

-- | interval from upper and lower
pinterval' ::
  forall a (s :: S).
  PIsData a =>
  Term
    s
    ( PAsData (PLowerBound a)
        #-> PAsData (PUpperBound a)
        #-> PInterval a
    )
pinterval' = phoistAcyclic $
  plam $ \lower upper ->
    pcon $
      PInterval $
        pdcons @"from" # lower
          #$ pdcons @"to" # upper # pdnil

-- | closed interval from PExtended
pclosedInterval ::
  forall a (s :: S).
  PIsData a =>
  Term
    s
    ( PExtended a
        #-> PExtended a
        #-> PInterval a
    )
pclosedInterval = phoistAcyclic $
  plam $ \start end ->
    let closure :: Term _ (PAsData PClosure)
        closure = pconstantData True

        upper :: Term _ (PUpperBound a)
        upper =
          pcon $
            PUpperBound $
              pdcons @"_0" # pdata end #$ pdcons @"_1"
                # closure
                # pdnil

        lower :: Term _ (PLowerBound a)
        lower =
          pcon $
            PLowerBound $
              pdcons @"_0" # pdata start #$ pdcons @"_1"
                # closure
                # pdnil
     in pinterval' # pdata lower # pdata upper

-- | value < endpoint
pbefore' ::
  forall a (s :: S).
  (PIsData a, POrd a, PEq a) =>
  Term
    s
    ( a
        #-> EndPoint a
        #-> PBool
    )
pbefore' = phoistAcyclic $
  plam $ \a y' -> P.do
    y <- pletFields @'["_0", "_1"] y'
    yt <- plet $ y._0
    let yc = y._1

    pif
      yc
      (pmatch yt (ltE' a))
      (pmatch yt (leqE' a))

-- | value > endpoint
pafter' ::
  forall a (s :: S).
  (PIsData a, POrd a, PEq a) =>
  Term
    s
    ( a
        #-> EndPoint a
        #-> PBool
    )
pafter' = phoistAcyclic $
  plam $ \a y' -> P.do
    y <- pletFields @'["_0", "_1"] y'
    yt <- plet $ y._0
    let yc = y._1

    pif
      yc
      (pmatch yt (gtE' a))
      (pmatch yt (geqE' a))

-- | (xPPlutus' s => Term s (EndPoint a)) <= (yPPlutus' s => Term s (EndPoint a))
leqP ::
  forall a (s :: S).
  (PIsData a, POrd a, PEq a) =>
  Term
    s
    ( EndPoint a
        #-> EndPoint a
        #-> PBool
    )
leqP = phoistAcyclic $
  plam $ \x' y' -> P.do
    x <- pletFields @'["_0", "_1"] x'
    y <- pletFields @'["_0", "_1"] y'

    xt <- plet $ x._0
    yt <- plet $ y._0

    xc <- plet $ x._1
    yc <- plet $ y._1

    pif
      (xc #&& yc #|| (pnot # xc) #&& (pnot # yc))
      (leqE # xt # yt)
      (ltE # xt # yt)

minP ::
  forall a (s :: S).
  (PIsData a, POrd a, PEq a) =>
  Term
    s
    ( EndPoint a
        #-> EndPoint a
        #-> EndPoint a
    )
minP = phoistAcyclic $ plam $ \x y -> pif' # (leqP # x # y) # x # y

maxP ::
  forall a (s :: S).
  (PIsData a, POrd a, PEq a) =>
  Term
    s
    ( EndPoint a
        #-> EndPoint a
        #-> EndPoint a
    )
maxP = phoistAcyclic $ plam $ \x y -> pif' # (leqP # x # y) # y # x

-- | (xPPlutus' s => Term s (PExtended a)) < (yPPlutus' s => Term s (PExtended b))
ltE ::
  forall a (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( PExtended a
        #-> PExtended a
        #-> PBool
    )
ltE = phoistAcyclic $ plam $ \x y -> pmatch x (cont y)
  where
    cont :: Term _ (PExtended a) -> PExtended a _ -> Term _ PBool
    cont y' x' = case x' of
      PNegInf _ -> pconstant True
      PPosInf _ -> pmatch y' isPosInf
      PFinite l -> pmatch y' (ltE' $ pfield @"_0" # l)

-- | (xPPlutus' s => Term s (PExtended a)) = (yPPlutus' s => Term s (PExtended b))
eqE ::
  forall a (s :: S).
  (PEq a, PIsData a) =>
  Term
    s
    ( PExtended a
        #-> PExtended a
        #-> PBool
    )
eqE = phoistAcyclic $
  plam $ \x y ->
    let cont x' = case x' of
          PNegInf _ -> pmatch y isNegInf
          PPosInf _ -> pmatch y isPosInf
          PFinite l -> pmatch y (eqE' (pfield @"_0" # l))
     in pmatch x cont

-- | value < PExtended
ltE' ::
  forall a (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
ltE' a y' = case y' of
  PNegInf _ -> pconstant False
  PPosInf _ -> pconstant True
  PFinite r -> a #< pfield @"_0" # r

-- | value > PExtended
gtE' ::
  forall a (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
gtE' a y' = case y' of
  PNegInf _ -> pconstant False
  PPosInf _ -> pconstant True
  PFinite r -> P.do
    b <- plet $ pfield @"_0" # r
    b #< a

-- | value = PExtended
eqE' ::
  forall a (s :: S).
  (PEq a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
eqE' a y' = case y' of
  PFinite r -> P.do
    b <- plet $ pfield @"_0" # r
    a #== b
  _ -> pconstant False

-- | value <= PExtended
leqE' ::
  forall a (s :: S).
  (POrd a, PEq a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
leqE' a y = ltE' a y #|| eqE' a y

-- | value >= PExtended
geqE' ::
  forall a (s :: S).
  (POrd a, PEq a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
geqE' a y = gtE' a y #|| eqE' a y

-- | (xPPlutus' s => Term s (PExtended a)) <= (yPPlutus' s => Term s (PExtended b))
leqE ::
  forall a (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term
    s
    ( PExtended a
        #-> PExtended a
        #-> PBool
    )
leqE = phoistAcyclic $ plam $ \x y -> ltE # x # y #|| eqE # x # y

isNegInf :: PExtended a s -> Term s PBool
isNegInf x = case x of
  PNegInf _ -> pconstant True
  _ -> pconstant False

isPosInf :: PExtended a s -> Term s PBool
isPosInf x = case x of
  PPosInf _ -> pconstant True
  _ -> pconstant False

type EndPoint a =
  PDataRecord
    '[ "_0" ':= PExtended a
     , "_1" ':= PClosure
     ]

uToEPPlutus' s => Term s (PUpperBound a #-> EndPoint a)
uToE = phoistAcyclic $ plam $ \x -> pmatch x (\(PUpperBound a) -> a)

lToEPPlutus' s => Term s (PLowerBound a #-> EndPoint a)
lToE = phoistAcyclic $ plam $ \x -> pmatch x (\(PLowerBound a) -> a)
