{-# Language UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

{-# Language PackageImports #-}

module Plutarch.Plutus where

import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Data.Kind (Type, Constraint)
import Plutarch.Core
import "plutarch-core" Plutarch.Prelude
import Plutarch.Internal2

type (&) :: (k -> Constraint) -> (k -> Constraint) -> (k -> Constraint)
class    (cls a, cls1 a) => (cls & cls1) a
instance (cls a, cls1 a) => (cls & cls1) a

-- data PUnit s = PUnit
--   deriving 
--   stock (Show, Generic)

--   deriving PHasRepr
--   via PIsPrimitive PBool

-- instance PLink edsl ()   PUnit
-- instance PLink edsl Bool PBool

--   default (#==) ::
--     (PGeneric t, PlutusType t, All2 PEq (PCode t)) =>
--     Term s t ->
--     Term s t ->
--     Term s PBool
--   a #== b = gpeq # a # b

-- instance PEq edsl PBool where
--   (#==) :: Term edsl PBool -> Term edsl PBool -> Term edsl PBool
--   (#==) = undefined 

-- infix 4 #==

-- -- | Partial ordering relation.
-- type  PPartialOrd :: PDSLKind -> PType -> Constraint
-- class PEq edsl t => PPartialOrd edsl t where
--   (#<=) :: Term edsl t -> Term edsl t -> Term edsl PBool
-- -- --   default (#<=) :: (POrd (PInner t)) => Term s t -> Term s t -> Term s PBool
-- -- --   x #<= y = pto x #<= pto y
--   (#<) :: Term edsl t -> Term edsl t -> Term edsl PBool
-- -- --   default (#<) :: (POrd (PInner t)) => Term s t -> Term s t -> Term s PBool
-- -- --   x #< y = pto x #< pto y

-- infix 4 #<=
-- infix 4 #<

-- -- | Total ordering relation.
-- class PPartialOrd edsl t => POrd edsl t

-- -- instance PPartialOrd PBool where
-- --   x #< y = pif' # x # pconstant False # y
-- --   x #<= y = pif' # x # y # pconstant True

-- -- instance POrd PBool

-- {- | Strict version of 'pif'.
--  Emits slightly less code.
-- -}

-- instance PIfThenElse Impl' where
--   pif' :: Term Impl' (PBool #-> a #-> a #-> a)
--   pif' = phoistAcyclic do pforce do punsafeBuiltin PLC.IfThenElse

{- |
  Plutus \'force\',
  used to force evaluation of 'PDelayed' terms.
-}
type PDelayed :: PType -> PType
data PDelayed a p
  deriving PHasRepr
  via HasPrimitiveRepr (PDelayed a)

type  PForce :: PDSLKind -> Constraint
class PForce edsl where
  pforce :: Term edsl (PDelayed a) -> Term edsl a

  pdelay :: Term edsl a -> Term edsl  (PDelayed a)

type  PHoist :: PDSLKind -> Constraint
class PHoist edsl where
  -- Use ClosedTerm?
  phoistAcyclic :: HasCallStack => Term edsl a -> Term edsl a

-- type PPlutus' :: PDSLKind -> Constraint
-- type PPlutus' edsl = 
--   ( PLC edsl
--   , PSOP edsl
--   , PHoist edsl
--   , forall a. IsPType edsl a => IsPType edsl (PDelayed a)
--   , PForce edsl
--   )

class PLink edsl a p => PEmbed edsl a p where
  pembed :: a -> Term edsl p

class PLink edsl a p => PExtract edsl a p where
  pextract :: Term edsl p -> Maybe a 

class PLink edsl a p | a -> p, p -> a

-- -- class (PConstantDecl (PLifted p), PConstanted (PLifted p) ~ p) => PUnsafeLiftDecl (p :: PType) where
-- --   type PLifted p = (r :: Type) | r -> p

-- -- class
-- --   ( PUnsafeLiftDecl (PConstanted h)
-- --   , PLC.DefaultUni `PLC.Includes` PConstantRepr h
-- --   ) =>
-- --   PConstantDecl (h :: Type)
-- --   where
-- --   type PConstantRepr h :: Type
-- --   type PConstanted h :: PType
-- --   pconstantToRepr :: h -> PConstantRepr h
-- --   pconstantFromRepr :: PConstantRepr h -> Maybe h

-- -- type PLift :: PType -> Constraint
-- -- type PLift = PUnsafeLiftDecl



