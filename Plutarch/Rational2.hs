{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Rational2 where
--   PRational (PRational),
--   preduce,
--   pnumerator,
--   pdenominator,
--   Plutarch.Rational.pfromInteger,
--   pround,
--   ptruncate,
--   pproperFraction,
--   PFractional (..),
-- ) where

import Data.Kind (Type, Constraint)
import Plutarch.Core
import Plutarch.Generics
import Plutarch.Integer2
import Plutarch.Positive2

-- import Data.Ratio (denominator, numerator)
import GHC.Generics (Generic)
-- import Plutarch (
--   DPTStrat,
--   DerivePlutusType,
--   PType,
--   PlutusType,
--   PlutusTypeScott,
--   Term,
--   pcon,
--   pfix,
--   phoistAcyclic,
--   plam,
--   plet,
--   pmatch,
--   pto,
--   runTermCont,
--   (#),
--   (#$),
--   type (#->),
--  )
-- import Plutarch.Bool (PEq, POrd, PPartialOrd, pif, (#<), (#<=), (#==))
-- import Plutarch.Builtin (
--   PAsData,
--   PBuiltinList,
--   PData,
--   PIsData,
--   pasInt,
--   pasList,
--   pdata,
--   pdataImpl,
--   pforgetData,
--   pfromDataImpl,
--  )
-- import Plutarch.Integer (PInteger, pdiv, pmod)
-- import Plutarch.Lift (pconstant)
-- import Plutarch.List (pcons, phead, pnil, ptail)
-- import Plutarch.Num (PNum, pabs, pfromInteger, pnegate, psignum, (#*), (#+), (#-))
-- import Plutarch.Pair (PPair (PPair))
-- import Plutarch.Positive (PPositive, ptryPositive)
-- import Plutarch.Show (PShow, pshow, pshow')
-- import Plutarch.TermCont (tcont, unTermCont)
-- import Plutarch.Trace (ptraceError)
-- import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'), ptryFrom)
-- import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)

type  PFractional :: PDSLKind -> PType -> Constraint
class PFractional edsl a where
  (#/) :: Term edsl a -> Term edsl a -> Term edsl a
  precip :: Term edsl (a #-> a)
--   pfromRational :: Term edsl (PRational #-> a)

-- | Note: This type is _not_ the synonym of 'PlutusTx.Rational'.
data PRational edsl
  = PRational (PInteger edsl) (PPositive edsl)
  deriving stock (Generic)
  -- deriving PHasRepr
  -- via HasSOPRepr (PRational edsl)

  -- deriving anyclass (PlutusType)

-- instance DerivePlutusType PRational where type DPTStrat _ = PlutusTypeScott

-- instance PEq PRational where
--   l' #== r' =
--     phoistAcyclic
--       ( plam $ \l r ->
--           pmatch l $ \(PRational ln ld) ->
--             pmatch r $ \(PRational rn rd) ->
--               pto rd * ln #== rn * pto ld
--       )
--       # l'
--       # r'

-- instance (PNum a, PFractional a) => Fractional (Term s a) where
--   (/) = (#/)
--   recip x = precip # x
--   fromRational x =
--     pfromRational #$ pcon $
--       PRational
--         (pconstant $ numerator x)
--         (punsafeDowncast . pconstant $ denominator x)

-- instance PShow PRational where
--   pshow' _ x =
--     pshowRat # x
--     where
--       pshowRat = phoistAcyclic $
--         plam $ \n -> pmatch n $ \(PRational x y) ->
--           pshow x <> "/" <> pshow (pto y)

-- -- | This instance _does not_ correspond to 'PlutusTx.Rational's data encoding.
-- instance PIsData PRational where
--   pfromDataImpl x' = phoistAcyclic (plam $ \x -> plistToRat #$ pasList # pforgetData x) # x'
--     where
--       plistToRatPPlutus' s => Term s (PBuiltinList PData #-> PRational)
--       plistToRat = plam $ \x ->
--         pcon $
--           PRational (pasInt #$ phead # x)
--             . punsafeDowncast
--             $ pasInt #$ phead #$ ptail # x
--   pdataImpl x' =
--     phoistAcyclic
--       ( plam $ \x -> unTermCont $ do
--           PRational a b <- tcont $ pmatch x
--           let res :: Term _ (PBuiltinList (PAsData PInteger))
--               res = pcons # pdata a #$ pcons # pdata (pto b) #$ pnil
--           pure $ pdataImpl res
--       )
--       # x'

-- newtype Flip f a b = Flip (f b a) deriving stock (Generic)

-- -- | NOTE: This instance produces a verified 'PPositive' as the excess output.
-- instance PTryFrom PData (PAsData PRational) where
--   type PTryFromExcess PData (PAsData PRational) = Flip Term PPositive
--   ptryFrom' opq = runTermCont $ do
--     (_, ld) <- tcont $ ptryFrom @(PAsData (PBuiltinList PData)) opq
--     ratTail <- tcont . plet $ ptail # ld
--     tcont $ \f -> pif (ptail # ratTail #== pnil) (f ()) $ ptraceError "ptryFrom(PRational): data list length should be 2"
--     (_, denm) <- tcont $ ptryFrom @(PAsData PInteger) $ phead # ratTail
--     res <- tcont . plet $ ptryPositive # denm
--     pure (punsafeCoerce opq, res)

-- instance PPartialOrd PRational where
--   l' #<= r' =
--     phoistAcyclic
--       ( plam $ \l r -> unTermCont $ do
--           PRational ln ld <- tcont $ pmatch l
--           PRational rn rd <- tcont $ pmatch r
--           pure $ pto rd * ln #<= rn * pto ld
--       )
--       # l'
--       # r'

--   l' #< r' =
--     phoistAcyclic
--       ( plam $ \l r -> unTermCont $ do
--           PRational ln ld <- tcont $ pmatch l
--           PRational rn rd <- tcont $ pmatch r
--           pure $ pto rd * ln #< rn * pto ld
--       )
--       # l'
--       # r'

-- instance POrd PRational

-- instance PNum PRational where
--   x' #+ y' =
--     phoistAcyclic
--       ( plam $ \x y -> unTermCont $ do
--           PRational xn xd' <- tcont $ pmatch x
--           PRational yn yd' <- tcont $ pmatch y
--           xd <- tcont $ plet xd'
--           yd <- tcont $ plet yd'
--           pure $
--             preduce
--               #$ pcon
--               $ PRational (xn * pto yd + yn * pto xd) $
--                 punsafeDowncast $ pto xd * pto yd
--       )
--       # x'
--       # y'

--   -- TODO (Optimize): Could this be optimized with an impl in terms of `#+`.
--   x' #- y' =
--     phoistAcyclic
--       ( plam $ \x y -> unTermCont $ do
--           PRational xn xd' <- tcont $ pmatch x
--           PRational yn yd' <- tcont $ pmatch y
--           xd <- tcont $ plet xd'
--           yd <- tcont $ plet yd'
--           pure $
--             preduce
--               #$ pcon
--               $ PRational (xn * pto yd - yn * pto xd) $
--                 punsafeDowncast $ pto xd * pto yd
--       )
--       # x'
--       # y'

--   x' #* y' =
--     phoistAcyclic
--       ( plam $ \x y -> unTermCont $ do
--           PRational xn xd <- tcont $ pmatch x
--           PRational yn yd <- tcont $ pmatch y
--           pure $
--             preduce
--               #$ pcon
--               $ PRational (xn * yn) $
--                 punsafeDowncast $ pto xd * pto yd
--       )
--       # x'
--       # y'

--   pnegate =
--     phoistAcyclic $
--       plam $ \x ->
--         pmatch x $ \(PRational xn xd) ->
--           pcon $ PRational (negate xn) xd

--   pabs =
--     phoistAcyclic $
--       plam $ \x ->
--         pmatch x $ \(PRational xn xd) ->
--           pcon $ PRational (abs xn) (abs xd)

--   psignum =
--     phoistAcyclic $
--       plam $ \x' -> plet x' $ \x ->
--         pif
--           (x #== 0)
--           0
--           $ pif
--             (x #< 0)
--             (-1)
--             1

--   pfromInteger n = pcon $ PRational (fromInteger n) 1

-- instance PFractional PRational where
--   precip =
--     phoistAcyclic $
--       plam $ \x ->
--         pmatch x $ \(PRational xn xd) ->
--           pcon $ PRational (pto xd) $ ptryPositive # xn

--   -- TODO (Optimize): Could this be optimized with an impl in terms of `#*`.
--   x' #/ y' =
--     phoistAcyclic
--       ( plam $ \x y -> unTermCont $ do
--           PRational xn xd <- tcont $ pmatch x
--           PRational yn yd <- tcont $ pmatch y
--           denm <- tcont . plet $ ptryPositive #$ pto xd * yn
--           pure $ preduce #$ pcon $ PRational (xn * pto yd) denm
--       )
--       # x'
--       # y'

--   pfromRational = phoistAcyclic $ plam id

-- preducePPlutus' s => Term s (PRational #-> PRational)
-- preduce = phoistAcyclic $
--   plam $ \x -> unTermCont $ do
--     PRational xn xd' <- tcont $ pmatch x
--     xd <- tcont . plet $ pto xd'
--     r <- tcont . plet $ pgcd # xn # xd
--     s <- tcont . plet $ psignum # xd
--     pure . pcon $ PRational (s * pdiv # xn # r) $ punsafeDowncast $ s * pdiv # xd # r

-- pgcdPPlutus' s => Term s (PInteger #-> PInteger #-> PInteger)
-- pgcd = phoistAcyclic $
--   plam $ \x' y' -> unTermCont $ do
--     x <- tcont . plet $ pabs # x'
--     y <- tcont . plet $ pabs # y'
--     pure $ pgcd' # (pmax # x # y) #$ pmin # x # y

-- -- assumes inputs are non negative and a >= b
-- pgcd'PPlutus' s => Term s (PInteger #-> PInteger #-> PInteger)
-- pgcd' = phoistAcyclic $ pfix #$ plam $ f
--   where
--     f self a b =
--       pif
--         (b #== 0)
--         a
--         $ self # b #$ pmod # a # b

-- pnumeratorPPlutus' s => Term s (PRational #-> PInteger)
-- pnumerator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational n _) -> n

-- pdenominatorPPlutus' s => Term s (PRational #-> PPositive)
-- pdenominator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational _ d) -> d

-- pfromIntegerPPlutus' s => Term s (PInteger #-> PRational)
-- pfromInteger = phoistAcyclic $ plam $ \n -> pcon $ PRational n 1

-- proundPPlutus' s => Term s (PRational #-> PInteger)
-- pround = phoistAcyclic $
--   plam $ \x -> unTermCont $ do
--     PRational a' b' <- tcont $ pmatch x
--     a <- tcont $ plet a'
--     b <- tcont $ plet b'
--     base <- tcont . plet $ pdiv # a # pto b
--     rem <- tcont . plet $ pmod # a # pto b
--     let result =
--           pif
--             (pmod # pto b # 2 #== 1)
--             (pif (pdiv # pto b # 2 #< rem) 1 0)
--             $ pif
--               (pdiv # pto b # 2 #== rem)
--               (pmod # base # 2)
--               (pif (rem #< pdiv # pto b # 2) 0 1)
--     pure $ base + result

-- ptruncatePPlutus' s => Term s (PRational #-> PInteger)
-- ptruncate = phoistAcyclic $
--   plam $ \x -> unTermCont $ do
--     PRational a' b' <- tcont $ pmatch x
--     a <- tcont $ plet a'
--     b <- tcont $ plet b'
--     q <- tcont . plet $ pdiv # a # pto b
--     pure $
--       pif
--         (0 #<= a)
--         q
--         (q + pif (pmod # a # pto b #== 0) 0 1)

-- pproperFractionPPlutus' s => Term s (PRational #-> PPair PInteger PRational)
-- pproperFraction = phoistAcyclic $
--   plam $ \x ->
--     plet (ptruncate # x) $ \q ->
--       pcon $ PPair q (x - Plutarch.Rational.pfromInteger # q)
