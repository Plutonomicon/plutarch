{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Rational (
  PRational (PRational),
  preduce,
  pnumerator,
  pdenominator,
  Plutarch.Rational.pfromInteger,
  pround,
  ptruncate,
  pproperFraction,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Bool (PBool, pcond, pif)
import Plutarch.Builtin.Data (PAsData, PBuiltinList, PData)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Fix (pfix)
import Plutarch.Internal.Lift (
  PLiftable (
    AsHaskell,
    PlutusRepr,
    haskToRepr,
    plutToRepr,
    reprToHask,
    reprToPlut
  ),
  PLiftedClosed,
  getPLiftedClosed,
  mkPLifted,
  mkPLiftedClosed,
  pconstant,
  pliftedFromClosed,
  pliftedToClosed,
 )
import Plutarch.Internal.ListLike (phead, pnil, ptail)
import Plutarch.Internal.Numeric (
  PAdditiveGroup (pnegate, pscaleInteger, (#-)),
  PAdditiveMonoid (pscaleNatural, pzero),
  PAdditiveSemigroup (pscalePositive, (#+)),
  PIntegralDomain (pabs, psignum),
  PMultiplicativeMonoid (pone, ppowNatural),
  PMultiplicativeSemigroup (ppowPositive, (#*)),
  PPositive,
  PRing (pfromInteger),
  pdiv,
  pmod,
  positiveToInteger,
  pquot,
  ptryPositive,
  toPositiveAbs,
 )
import Plutarch.Internal.Ord (
  POrd ((#<), (#<=)),
 )
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PlutusType, pcon, pmatch)
import Plutarch.Internal.Show (PShow, pshow, pshow')
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  plet,
  punsafeBuiltin,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Internal.TermCont (
  runTermCont,
  tcont,
  unTermCont,
 )
import Plutarch.Internal.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'), ptryFrom)
import Plutarch.Pair (PPair (PPair))
import Plutarch.Repr.SOP (DeriveAsSOPRec (DeriveAsSOPRec))
import Plutarch.Trace (ptraceInfoError)
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import PlutusCore qualified as PLC
import PlutusTx.Ratio qualified as PlutusTx

{- | A Scott-encoded rational number, with a guaranteed positive denominator
(and thus, a canonical form).

= Note

This is not the Plutarch equivalent of a Plutus @Rational@; for this, you
want @PRationalData@ from @plutarch-ledger-api@. 'PRational' is designed to
optimize for computation: if you want to do any serious work with rational
numbers that isn't just passing them around, you want to use (or convert to)
'PRational'.
-}
data PRational s
  = PRational (Term s PInteger) (Term s PPositive)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)

deriving via (DeriveAsSOPRec PRational) instance PlutusType PRational

-- | @since WIP
instance PLiftable PRational where
  type AsHaskell PRational = PlutusTx.Rational
  type PlutusRepr PRational = PLiftedClosed PRational
  {-# INLINEABLE haskToRepr #-}
  haskToRepr r =
    let n = PlutusTx.numerator r
        d = PlutusTx.denominator r
     in case signum n of
          0 -> mkPLiftedClosed $ pcon $ PRational 0 pone
          _ ->
            let dabs = toPositiveAbs d
             in case signum d of
                  (-1) ->
                    mkPLiftedClosed $ pcon . PRational (pconstant . negate $ n) . pconstant $ dabs
                  _ ->
                    mkPLiftedClosed $ pcon . PRational (pconstant n) . pconstant $ dabs
  {-# INLINEABLE reprToHask #-}
  reprToHask x = do
    n <- plutToRepr $ mkPLifted (pnumerator # getPLiftedClosed x)
    dr :: PlutusRepr PPositive <- plutToRepr $ mkPLifted (pdenominator # getPLiftedClosed x)
    d <- reprToHask @PPositive dr
    pure . PlutusTx.unsafeRatio n . positiveToInteger $ d

  {-# INLINEABLE plutToRepr #-}
  plutToRepr = Right . pliftedToClosed
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = pliftedFromClosed

instance PEq PRational where
  {-# INLINEABLE (#==) #-}
  l' #== r' = inner # l' # r'
    where
      inner :: forall (s :: S). Term s (PRational :--> PRational :--> PBool)
      inner = phoistAcyclic $ plam $ \l r ->
        cmpHelper # punsafeBuiltin PLC.EqualsInteger # l # r

instance POrd PRational where
  {-# INLINEABLE (#<=) #-}
  l' #<= r' = inner # l' # r'
    where
      inner :: forall (s :: S). Term s (PRational :--> PRational :--> PBool)
      inner = phoistAcyclic $ plam $ \l r ->
        cmpHelper # punsafeBuiltin PLC.LessThanEqualsInteger # l # r
  {-# INLINEABLE (#<) #-}
  l' #< r' = inner # l' # r'
    where
      inner :: forall (s :: S). Term s (PRational :--> PRational :--> PBool)
      inner = phoistAcyclic $ plam $ \l r ->
        cmpHelper # punsafeBuiltin PLC.LessThanInteger # l # r

-- | @since WIP
instance PAdditiveSemigroup PRational where
  {-# INLINEABLE (#+) #-}
  x' #+ y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd' <- tcont $ pmatch x
          PRational yn yd' <- tcont $ pmatch y
          xd <- tcont $ plet xd'
          yd <- tcont $ plet yd'
          pure $ preduce' # (xn * pto yd + yn * pto xd) # pto (xd #* yd)
      )
      # x'
      # y'
  {-# INLINEABLE pscalePositive #-}
  pscalePositive x p = pmatch x $ \(PRational xn xd) ->
    preduce' # (xn #* pto p) # pto xd

-- | @since WIP
instance PAdditiveMonoid PRational where
  {-# INLINEABLE pzero #-}
  pzero = pcon . PRational pzero $ pone
  {-# INLINEABLE pscaleNatural #-}
  pscaleNatural x n = pmatch x $ \(PRational xn xd) ->
    preduce' # (xn #* pto n) # pto xd

-- | @since WIP
instance PAdditiveGroup PRational where
  {-# INLINEABLE pnegate #-}
  pnegate =
    phoistAcyclic $
      plam $ \x ->
        pmatch x $ \(PRational xn xd) ->
          pcon $ PRational (pnegate # xn) xd
  {-# INLINEABLE (#-) #-}
  x' #- y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd' <- tcont $ pmatch x
          PRational yn yd' <- tcont $ pmatch y
          xd <- tcont $ plet xd'
          yd <- tcont $ plet yd'
          pure $ preduce' # (xn * pto yd - yn * pto xd) # pto (xd #* yd)
      )
      # x'
      # y'
  {-# INLINEABLE pscaleInteger #-}
  pscaleInteger x e = pmatch x $ \(PRational xn xd) ->
    preduce' # (xn #* e) # pto xd

-- | @since WIP
instance PMultiplicativeSemigroup PRational where
  {-# INLINEABLE (#*) #-}
  x' #* y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd <- tcont $ pmatch x
          PRational yn yd <- tcont $ pmatch y
          pure $ preduce' # (xn * yn) # pto (xd #* yd)
      )
      # x'
      # y'
  {-# INLINEABLE ppowPositive #-}
  ppowPositive x p =
    plet p $ \p' ->
      pmatch x $ \(PRational xn xd) ->
        pcon . PRational (ppowPositive xn p') $ ppowPositive xd p'

-- | @since WIP
instance PMultiplicativeMonoid PRational where
  {-# INLINEABLE pone #-}
  pone = pcon . PRational pone $ pone
  {-# INLINEABLE ppowNatural #-}
  ppowNatural x n = plet n $ \n' ->
    pmatch x $ \(PRational xn xd) ->
      pcon . PRational (ppowNatural xn n') $ ppowNatural xd n'

-- | @since WIP
instance PRing PRational where
  {-# INLINEABLE pfromInteger #-}
  pfromInteger n = pcon $ PRational (fromInteger n) pone

-- | @since WIP
instance PIntegralDomain PRational where
  {-# INLINEABLE pabs #-}
  pabs =
    phoistAcyclic $
      plam $ \x ->
        pmatch x $ \(PRational xn xd) ->
          pcon $ PRational (pabs # xn) xd
  {-# INLINEABLE psignum #-}
  psignum = phoistAcyclic $ plam $ \x ->
    pmatch x $ \(PRational n _) ->
      pcond
        [ (n #== 0, pzero)
        , (n #<= 0, pcon . PRational (pconstant (-1)) $ pone)
        ]
        pone

-- | @since WIP
instance Fractional (Term s PRational) where
  {-# INLINEABLE (/) #-}
  x / y = inner # x # y
    where
      inner :: forall (s :: S). Term s (PRational :--> PRational :--> PRational)
      inner = phoistAcyclic $ plam $ \x y -> pmatch x $ \(PRational xn xd) ->
        pmatch y $ \(PRational yn yd) ->
          plet (pto xd * yn) $ \denm ->
            pif
              (denm #== 0)
              (ptraceInfoError "Cannot divide by zero")
              (preduce' # (xn * pto yd) # denm)
  {-# INLINEABLE recip #-}
  recip x = inner # x
    where
      inner :: forall (s :: S). Term s (PRational :--> PRational)
      inner = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational xn xd) ->
        pcond
          [ (xn #== 0, ptraceInfoError "attempted to construct the reciprocal of zero")
          , (xn #<= 0, pcon $ PRational (pnegate #$ pto xd) (punsafeCoerce $ pnegate # xn))
          ]
          (pcon $ PRational (pto xd) (punsafeCoerce xn))
  {-# INLINEABLE fromRational #-}
  fromRational = pconstant . PlutusTx.fromGHC

instance PShow PRational where
  pshow' _ x =
    pshowRat # x
    where
      pshowRat = phoistAcyclic $
        plam $ \n -> pmatch n $ \(PRational x y) ->
          pshow x <> "/" <> pshow (pto y)

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

-- | NOTE: This instance produces a verified 'PPositive' as the excess output.
instance PTryFrom PData (PAsData PRational) where
  type PTryFromExcess PData (PAsData PRational) = Flip Term PPositive
  ptryFrom' opq = runTermCont $ do
    (_, ld) <- tcont $ ptryFrom @(PAsData (PBuiltinList PData)) opq
    ratTail <- tcont . plet $ ptail # ld
    tcont $ \f -> pif (ptail # ratTail #== pnil) (f ()) $ ptraceInfoError "ptryFrom(PRational): data list length should be 2"
    (_, denm) <- tcont $ ptryFrom @(PAsData PInteger) $ phead # ratTail
    res <- tcont . plet $ ptryPositive # denm
    pure (punsafeCoerce opq, res)

preduce :: Term s (PRational :--> PRational)
preduce = phoistAcyclic $ plam $ \x ->
  pmatch x $ \(PRational n d) -> preduce' # n # pto d

pgcd :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont . plet $ pabs # x'
    y <- tcont . plet $ pabs # y'
    pure $
      pif
        (x #<= y)
        (pgcd' # y # x)
        (pgcd' # x # y)

-- assumes inputs are non negative and a >= b
pgcd' :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd' = phoistAcyclic $ pfix #$ plam f
  where
    f self a b =
      pif
        (b #== 0)
        a
        $ self # b #$ pmod # a # b

pnumerator :: Term s (PRational :--> PInteger)
pnumerator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational n _) -> n

pdenominator :: Term s (PRational :--> PPositive)
pdenominator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational _ d) -> d

pfromInteger :: Term s (PInteger :--> PRational)
pfromInteger = phoistAcyclic $ plam $ \n -> pcon $ PRational n pone

pround :: Term s (PRational :--> PInteger)
pround = phoistAcyclic $
  plam $ \x -> unTermCont $ do
    PRational a' b' <- tcont $ pmatch x
    a <- tcont $ plet a'
    b <- tcont $ plet b'
    base <- tcont . plet $ pdiv # a # pto b
    rem <- tcont . plet $ pmod # a # pto b
    let result =
          pcond
            [ (pmod # pto b # 2 #== 1, pif (pdiv # pto b # 2 #< rem) 1 0)
            , (pdiv # pto b # 2 #== rem, pmod # base # 2)
            , (rem #< pdiv # pto b # 2, 0)
            ]
            1
    pure $ base + result

ptruncate :: Term s (PRational :--> PInteger)
ptruncate = phoistAcyclic $
  plam $ \x ->
    pmatch x $ \(PRational a b) ->
      pquot # a # pto b

pproperFraction :: Term s (PRational :--> PPair PInteger PRational)
pproperFraction = phoistAcyclic $
  plam $ \x ->
    plet (ptruncate # x) $ \q ->
      pcon $ PPair q (x - Plutarch.Rational.pfromInteger # q)

-- Helpers

cmpHelper ::
  forall (s :: S).
  Term s ((PInteger :--> PInteger :--> PBool) :--> PRational :--> PRational :--> PBool)
cmpHelper = phoistAcyclic $ plam $ \f l r ->
  pmatch l $ \(PRational ln ld) ->
    pmatch r $ \(PRational rn rd) ->
      f # (pto rd * ln) # (rn * pto ld)

-- Assumes d is not zero
preduce' :: forall (s :: S). Term s (PInteger :--> PInteger :--> PRational)
preduce' = phoistAcyclic $ plam $ \n d' ->
  plet d' $ \d ->
    plet (pgcd # n # d) $ \r ->
      pif
        (d #<= 0)
        (pcon $ PRational (pnegate # (pdiv # n # r)) $ punsafeDowncast (pnegate # (pdiv # d # r)))
        (pcon $ PRational (pdiv # n # r) $ punsafeDowncast (pdiv # d # r))
