{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Plutarch.DataRepr (PDataRepr, SNat (..), punDataRepr, pindexDataRepr, pmatchDataRepr) where

import Plutarch (punsafeBuiltin, punsafeCoerce)
import Plutarch.Bool (pif, (£==))
import Plutarch.Builtin (PData, PList)
import qualified Plutarch.Builtin.Pair as BP
import Plutarch.BuiltinHList (PBuiltinHList)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import qualified PlutusCore as PLC

type PDataRepr :: [[k -> Type]] -> k -> Type
data PDataRepr (defs :: [[k -> Type]]) (s :: k)

pasData :: Term s (PDataRepr _) -> Term s PData
pasData = punsafeCoerce

data Nat = N | S Nat

data SNat :: Nat -> Type where
  SN :: SNat 'N
  SS :: SNat n -> SNat ( 'S n)

unSingleton :: SNat n -> Nat
unSingleton SN = N
unSingleton (SS n) = S $ unSingleton n

natToInteger :: Nat -> Integer
natToInteger N = 0
natToInteger (S n) = 1 + natToInteger n

type family IndexList (n :: Nat) (l :: [k]) :: k
type instance IndexList 'N '[x] = x
type instance IndexList ( 'S n) (x : xs) = IndexList n xs

punDataRepr :: Term s (PDataRepr '[def] :--> PBuiltinHList def)
punDataRepr = phoistAcyclic $
  plam $ \t ->
    plet (pasConstr £$ pasData t) $ \d ->
      (punsafeCoerce $ BP.sndPair d :: Term _ (PBuiltinHList def))

pindexDataRepr :: SNat n -> Term s (PDataRepr (def : defs) :--> PBuiltinHList (IndexList n (def : defs)))
pindexDataRepr n = phoistAcyclic $
  plam $ \t ->
    plet (pasConstr £$ pasData t) $ \d ->
      let i :: Term _ PInteger = BP.fstPair d
       in pif
            (i £== (fromInteger . natToInteger . unSingleton $ n))
            (punsafeCoerce $ BP.sndPair d :: Term _ (PBuiltinHList _))
            perror

type family LengthList (l :: [k]) :: Nat
type instance LengthList '[] = 'N
type instance LengthList (x : xs) = 'S (LengthList xs)

data DataReprHandlers (out :: k -> Type) (def :: [[k -> Type]]) (s :: k) where
  DRHNil :: DataReprHandlers out '[] s
  DRHCons :: (Term s (PBuiltinHList def) -> Term s out) -> DataReprHandlers out defs s -> DataReprHandlers out (def : defs) s

-- FIXME: remove unnecessary final perror if all cases are matched
punsafeMatchDataRepr' :: Integer -> DataReprHandlers out defs s -> Term s PInteger -> Term s (PList PData) -> Term s out
punsafeMatchDataRepr' _ DRHNil _ _ = perror
punsafeMatchDataRepr' idx (DRHCons handler rest) constr args =
  pif
    (fromInteger idx £== constr)
    (handler $ punsafeCoerce args)
    $ punsafeMatchDataRepr' (idx + 1) rest constr args

pmatchDataRepr :: DataReprHandlers out defs s -> Term s (PDataRepr defs) -> Term s out
pmatchDataRepr handlers d =
  let d' = pasConstr £$ pasData d
   in punsafeMatchDataRepr'
        0
        handlers
        (BP.fstPair d')
        (BP.sndPair d')

-- TODO: Rewrite this to use Plutarch.Builtin instead of `punsafeBuiltin`. This
-- may first require having Plutarch.Builtin use `PData` (in lieu of
-- `POpaque``).
pasConstr :: Term s (PData :--> BP.PPair PInteger (PList PData))
pasConstr = punsafeBuiltin PLC.UnConstrData
