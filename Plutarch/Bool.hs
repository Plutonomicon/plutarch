{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Bool (
  PBool (..),
  PEq (..),
  PPartialOrd (..),
  POrd,
  pif,
  pif',
  pnot,
  (#&&),
  (#||),
  por,
  pand,
  pand',
  por',
) where

import Data.List.NonEmpty (nonEmpty)
import Generics.SOP (
  All,
  All2,
  HCollapse (hcollapse),
  K (K),
  NP,
  Proxy (Proxy),
  SOP (SOP),
  ccompare_NS,
  hcliftA2,
 )
import Plutarch.Internal (
  PDelayed,
  S,
  Term,
  pdelay,
  pforce,
  phoistAcyclic,
  plet,
  (#),
  (#$),
  (#->),
 )
import Plutarch.Internal.Generic (PCode, PGeneric, gpfrom)
import Plutarch.Internal.Other (
  pto,
 )
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon, pcon', pmatch, pmatch')
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Unsafe (punsafeBuiltin)
import qualified PlutusCore as PLC

-- | Plutus 'BuiltinBool'
data PBool (s :: S) = PTrue | PFalse
  deriving stock (Show)

instance PUnsafeLiftDecl PBool where type PLifted PBool = Bool
deriving via DerivePConstantDirect Bool PBool instance PConstantDecl Bool

instance PlutusType PBool where
  type PInner PBool = PBool
  pcon' PTrue = pconstant True
  pcon' PFalse = pconstant False
  pmatch' b f = pforce $ pif' # b # pdelay (f PTrue) # pdelay (f PFalse)

class PEq t where
  (#==)PPlutus' s => Term s t -> Term s t -> Term s PBool
  default (#==) ::
    (PGeneric t, PlutusType t, All2 PEq (PCode t)) =>
    Term s t ->
    Term s t ->
    Term s PBool
  a #== b = gpeq # a # b

infix 4 #==

-- | Partial ordering relation.
class PEq t => PPartialOrd t where
  (#<=)PPlutus' s => Term s t -> Term s t -> Term s PBool
  default (#<=) :: (POrd (PInner t)) => Term s t -> Term s t -> Term s PBool
  x #<= y = pto x #<= pto y
  (#<)PPlutus' s => Term s t -> Term s t -> Term s PBool
  default (#<) :: (POrd (PInner t)) => Term s t -> Term s t -> Term s PBool
  x #< y = pto x #< pto y

infix 4 #<=
infix 4 #<

-- | Total ordering relation.
class PPartialOrd t => POrd t

instance PEq PBool where
  x #== y' = plet y' $ \y -> pif' # x # y #$ pnot # y

instance PPartialOrd PBool where
  x #< y = pif' # x # pconstant False # y
  x #<= y = pif' # x # y # pconstant True

instance POrd PBool

{- | Strict version of 'pif'.
 Emits slightly less code.
-}
pif'PPlutus' s => Term s (PBool #-> a #-> a #-> a)
pif' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.IfThenElse

-- | Lazy if-then-else.
pifPPlutus' s => Term s PBool -> Term s a -> Term s a -> Term s a
pif b case_true case_false = pmatch b $ \case
  PTrue -> case_true
  PFalse -> case_false

-- | Boolean negation for 'PBool' terms.
pnotPPlutus' s => Term s (PBool #-> PBool)
pnot = phoistAcyclic $ plam $ \x -> pif' # x # pcon PFalse # pcon PTrue

-- | Lazily evaluated boolean and for 'PBool' terms.
infixr 3 #&&

(#&&)PPlutus' s => Term s PBool -> Term s PBool -> Term s PBool
x #&& y = pforce $ pand # x # pdelay y

-- | Lazily evaluated boolean or for 'PBool' terms.
infixr 2 #||

(#||)PPlutus' s => Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pforce $ por # x # pdelay y

-- | Hoisted, Plutarch level, lazily evaluated boolean and function.
pandPPlutus' s => Term s (PBool #-> PDelayed PBool #-> PDelayed PBool)
pand = phoistAcyclic $ plam $ \x y -> pif' # x # y # (phoistAcyclic $ pdelay $ pcon PFalse)

-- | Hoisted, Plutarch level, strictly evaluated boolean and function.
pand'PPlutus' s => Term s (PBool #-> PBool #-> PBool)
pand' = phoistAcyclic $ plam $ \x y -> pif' # x # y # (pcon PFalse)

-- | Hoisted, Plutarch level, lazily evaluated boolean or function.
porPPlutus' s => Term s (PBool #-> PDelayed PBool #-> PDelayed PBool)
por = phoistAcyclic $ plam $ \x -> pif' # x # (phoistAcyclic $ pdelay $ pcon PTrue)

-- | Hoisted, Plutarch level, strictly evaluated boolean or function.
por'PPlutus' s => Term s (PBool #-> PBool #-> PBool)
por' = phoistAcyclic $ plam $ \x -> pif' # x # (pcon PTrue)

-- | Like Haskell's `and` but for Plutarch terms
pands :: [Term s PBool] -> Term s PBool
pands ts' =
  case nonEmpty ts' of
    Nothing -> pcon PTrue
    Just ts -> foldl1 (#&&) ts

-- | Generic version of (#==)
gpeq ::
  forall t s.
  ( PGeneric t
  , PlutusType t
  , All2 PEq (PCode t)
  ) =>
  Term s (t #-> t #-> PBool)
gpeq =
  phoistAcyclic $
    plam $ \x y ->
      pmatch x $ \x' ->
        pmatch y $ \y' ->
          gpeq' (gpfrom x') (gpfrom y')

gpeq' :: All2 PEq xss => SOP (Term s) xss -> SOP (Term s) xss -> Term s PBool
gpeq' (SOP c1) (SOP c2) =
  ccompare_NS (Proxy @(All PEq)) (pcon PFalse) eqProd (pcon PFalse) c1 c2

eqProd :: All PEq xs => NP (Term s) xs -> NP (Term s) xs -> Term s PBool
eqProd p1 p2 =
  pands $ hcollapse $ hcliftA2 (Proxy :: Proxy PEq) eqTerm p1 p2
  where
    eqTerm :: forall s a. PEq a => Term s a -> Term s a -> K (Term s PBool) a
    eqTerm a b =
      K $ a #== b
