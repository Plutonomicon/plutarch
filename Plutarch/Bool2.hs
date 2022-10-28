{-# Language UndecidableSuperClasses #-}
{-# Language UndecidableInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -w -Wno-orphans #-}

module Plutarch.Bool2 where

import Data.List.NonEmpty (nonEmpty)
import Data.Kind (Type, Constraint)
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
import Plutarch.Core (Term)
import Plutarch.Generics
import Data.Functor.Compose
import Plutarch.Core hiding (PBool(..))
import Plutarch.PType
import GHC.Generics
import Data.SOP.Constraint qualified as SOP
import qualified Generics.SOP as SOP
import qualified Data.SOP as SOP

import Plutarch.Lam
import Plutarch.Internal2
import Plutarch.Internal.PlutusType2
import Plutarch.Plutus (PHoist(..), PForce(..), PDelayed(..))

type PBool :: PType
data PBool ef = PFalse | PTrue
  deriving 
  stock (Show, Generic)

  deriving PHasRepr
  via HasPrimitiveRepr PBool

instance ( PLC edsl
         , PHoist edsl 
         , PConstructable edsl PBool
         , PConstructable edsl (PLet PBool)
         , PIfThenElse edsl
         )
      => PEq edsl PBool where
  x #== y' = plet y' $ \y -> pif' # x # y #$ pnot # y

type  PIfThenElse :: PDSLKind -> Constraint
class PIfThenElse edsl where
  pif' :: Term edsl (PBool #-> a #-> a #-> a)

infix 4 #==

type  PEq :: PDSLKind -> PType -> Constraint
class PEq edsl a where
  (#==) :: Term edsl a -> Term edsl a -> Term edsl PBool
  default (#==) 
    :: PGeneric a
    => PlutusType a
    => All2 (PEq edsl) (PCode a)
    => PHoist edsl
    => PLC edsl
    => PConstructable edsl PBool
    => PIfThenElse edsl
    => PConstructable edsl a
    => PForce edsl
    => IsPType edsl (PDelayed PBool)
    => SOP.AllZip2 (SOP.LiftedCoercible (Pf' (Helper edsl)) (Term edsl)) (PCode a) (PCode a) 
    => Term edsl a -> Term edsl a -> Term edsl PBool
  a #== b = gpeq # a # b

infix 4 #<=
infix 4 #<

type  PPartialOrd :: PDSLKind -> PType -> Constraint
class PEq edsl a => PPartialOrd edsl a where
  (#<=) :: Term edsl a -> Term edsl a -> Term edsl PBool
  (#<)  :: Term edsl a -> Term edsl a -> Term edsl PBool
  default (#<=) 
    :: POrd edsl (PInner a) 
    => PUntyped edsl
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => Term edsl a -> Term edsl a -> Term edsl PBool
  x #<= y = pto x #<= pto y
  default (#<) 
    :: POrd edsl (PInner a) 
    => PUntyped edsl
    => IsPType edsl a
    => IsPType edsl (PInner a)
    => Term edsl a -> Term edsl a -> Term edsl PBool
  x #< y = pto x #< pto y

type  POrd :: PDSLKind -> PType -> Constraint
class PPartialOrd edsl a => POrd edsl a

pmin 
  :: POrd edsl a 
  => PConstructable edsl PBool
  => IsPType edsl a
  => PLC edsl
  => PHoist edsl
  => Term edsl (a #-> a #-> a)
pmin = phoistAcyclic $ plam \a b -> pif (a #<= b) a b

pmax 
  :: POrd edsl a 
  => PConstructable edsl PBool
  => IsPType edsl a
  => PLC edsl
  => PHoist edsl
  => Term edsl (a #-> a #-> a)
pmax = phoistAcyclic $ plam \a b -> pif (a #<= b) b a

--

-- | Lazy if-then-else.
pif :: PConstructable edsl PBool 
    => IsPType edsl a
    => Term edsl PBool 
    -> Term edsl a 
    -> Term edsl a 
    -> Term edsl a
pif b case_true case_false = pmatch b \case
  PTrue  -> case_true
  PFalse -> case_false

-- | Boolean negation for 'PBool' terms.
pnot :: PIfThenElse edsl
     => PLC edsl 
     => PHoist edsl 
     => PConstructable edsl PBool
     => Term edsl (PBool #-> PBool)
pnot = phoistAcyclic $ plam \x -> 
  pif' # x # pcon PFalse # pcon PTrue

-- | Lazily evaluated boolean and for 'PBool' terms.
infixr 3 #&&

(#&&) :: PConstructable edsl PBool
      => PLC edsl
      => PHoist edsl
      => PIfThenElse edsl
      => PForce edsl
      => IsPType edsl (PDelayed PBool)
      => Term edsl PBool -> Term edsl PBool -> Term edsl PBool
x #&& y = pforce $ pand # x # pdelay y

-- | Lazily evaluated boolean or for 'PBool' terms.
infixr 2 #||

(#||) 
  :: PConstructable edsl PBool
  => PLC edsl 
  => PForce edsl 
  => PHoist edsl 
  => PIfThenElse edsl 
  => IsPType edsl (PDelayed PBool)
  => Term edsl PBool -> Term edsl PBool -> Term edsl PBool
x #|| y = pforce $ por # x # pdelay y

-- | Hoisted, Plutarch level, lazily evaluated boolean and function.
pand :: PConstructable edsl PBool
     => PForce edsl
     => PLC edsl 
     => PHoist edsl 
     => PIfThenElse edsl 
     => IsPType edsl (PDelayed PBool)
     => Term edsl (PBool #-> PDelayed PBool #-> PDelayed PBool)
pand = phoistAcyclic $ plam $ \x y -> pif' # x # y # (phoistAcyclic $ pdelay $ pcon PFalse)

-- | Hoisted, Plutarch level, strictly evaluated boolean and function.
pand' :: PConstructable edsl PBool
      => PLC edsl 
      => PHoist edsl 
      => PIfThenElse edsl 
      => Term edsl (PBool #-> PBool #-> PBool)
pand' = phoistAcyclic $ plam $ \x y -> pif' # x # y # (pcon PFalse)

-- | Hoisted, Plutarch level, lazily evaluated boolean or function.
por :: PConstructable edsl PBool
    => PLC edsl 
    => PForce edsl 
    => PHoist edsl 
    => PIfThenElse edsl 
    => IsPType edsl (PDelayed PBool)
    => Term edsl (PBool #-> PDelayed PBool #-> PDelayed PBool)
por = phoistAcyclic $ plam $ \x -> pif' # x # (phoistAcyclic $ pdelay $ pcon PTrue)

-- | Hoisted, Plutarch level, strictly evaluated boolean or function.
por' :: PConstructable edsl PBool
     => PLC edsl 
     => PHoist edsl 
     => PIfThenElse edsl 
     => Term edsl (PBool #-> PBool #-> PBool)
por' = phoistAcyclic $ plam $ \x -> pif' # x # (pcon PTrue)

-- | Like Haskell's `and` but for Plutarch terms
pands :: PConstructable edsl PBool
      => PLC edsl
      => PHoist edsl
      => PIfThenElse edsl
      => PForce edsl
      => IsPType edsl (PDelayed PBool)
      => [Term edsl PBool] -> Term edsl PBool
pands ts' =
  case nonEmpty ts' of
    Nothing -> pcon PTrue
    Just ts -> foldl1 (#&&) ts

-- | Generic version of (#==)
gpeq 
  :: forall edsl a. ()
  => PGeneric a
  => PlutusType a
  => All2 (PEq edsl) (PCode a)
  => PHoist edsl
  => PLC edsl
  => PConstructable edsl PBool
  => PIfThenElse edsl
  => PConstructable edsl a
  => PForce edsl
  => IsPType edsl (PDelayed PBool)
  => SOP.AllZip2 (SOP.LiftedCoercible (Pf' (Helper edsl)) (Term edsl)) (PCode a) (PCode a) 
  => Term edsl (a #-> a #-> PBool)
gpeq = phoistAcyclic (plam body) where

  body :: Term edsl a -> Term edsl a -> Term edsl PBool 
  body x y = 
    pmatch x \x' -> 
    pmatch y \y' -> 
      gpeq' (gfromConcrete x') (gfromConcrete y')

gpeq' 
  :: forall edsl xss. ()
  => PConstructable edsl PBool
  => PLC edsl
  => PHoist edsl
  => PIfThenElse edsl
  => PForce edsl
  => IsPType edsl (PDelayed PBool)
  => All2 (PEq edsl) xss 
  => SOP (Term edsl) xss -> SOP (Term edsl) xss -> Term edsl PBool
gpeq' (SOP c1) (SOP c2) =
  ccompare_NS (Proxy @(All (PEq edsl))) (pcon PFalse) eqProd (pcon PFalse) c1 c2

eqProd 
  :: forall edsl xs. ()
  => PConstructable edsl PBool
  => PLC edsl
  => PHoist edsl
  => PIfThenElse edsl
  => PForce edsl
  => IsPType edsl (PDelayed PBool)
  => All (PEq edsl) xs 
  => NP (Term edsl) xs -> NP (Term edsl) xs -> Term edsl PBool
eqProd p1 p2 =
  pands $ hcollapse $ hcliftA2 (Proxy @(PEq edsl)) eqTerm p1 p2 
  where
    eqTerm :: PEq edsl a => Term edsl a -> Term edsl a -> K (Term edsl PBool) u
    eqTerm a b = K (a #== b)

type
  GetPNewtype' :: [[PType]] -> PType
type family 
  GetPNewtype' a where
  GetPNewtype' '[ '[a]] = a

type GetPNewtype :: PType -> PType
type GetPNewtype a = GetPNewtype' (PCode a)

derivedPCon' 
  :: PGeneric a 
  => PCode a ~ '[ '[GetPNewtype a]]
  => SOP.AllZip2 (SOP.LiftedCoercible (Pf' (Helper edsl)) (Term edsl)) (PCode a) (PCode a)
  => PConcrete edsl a 
  -> Term edsl (GetPNewtype a)
derivedPCon' a = case gfromConcrete a of
    SOP.SOP (SOP.Z (x SOP.:* SOP.Nil)) -> x
    SOP.SOP (SOP.S x) -> case x of {}
