{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Other2 where

import Plutarch.Core
import Plutarch.Internal2
import Plutarch.Lam
import Plutarch.Plutus

import Plutarch.Bool2

-- import qualified Data.Text as T
-- import GHC.Stack (HasCallStack)
-- import Plutarch.Internal (ClosedTerm, Config, Term, compile, phoistAcyclic, plam', punsafeCoerce, (#), (#->))
-- import Plutarch.Internal.PlutusType (
--   PContravariant',
--   PCovariant',
--   PInner,
--   PVariant',
--   PlutusType,
--   pcon',
--   pmatch',
--  )
-- import PlutusCore.Pretty (prettyPlcReadableDebug)
-- import PlutusLedgerApi.V1.Scripts (Script (Script))

-- -- | Prettyprint a compiled Script via the PLC pretty printer
-- printScript :: Script -> String
-- printScript = show . prettyPlcReadableDebug . (\(Script s) -> s)

-- {- | Prettyprint a Term via the PLC pretty printer

--   TODO: Heavily improve. It's unreadable right now.

--   We could convert the de Bruijn indices into names with:

--   > show . prettyPlcReadableDef . (\(Right p) -> p) . Scripts.mkTermToEvaluate . compile $ term
-- -}
-- printTerm :: HasCallStack => Config -> ClosedTerm a -> String
-- printTerm config term = printScript $ either (error . T.unpack) id $ compile config term


-- | An Arbitrary Term with an unknown type
type    POpaque :: PDSLKind -> PType
newtype POpaque edsl el = POpaque (Term edsl (POpaque edsl))
  deriving PHasRepr
  via HasPrimitiveRepr (POpaque edsl)

  deriving PlutusType
  via HasSameInner (POpaque edsl)

-- | Erase the type of a Term
popaque :: PUntyped edsl 
        => PConstructable edsl (POpaque edsl) 
        => IsPType edsl a 
        => Term edsl a -> Term edsl (POpaque edsl)
popaque = punsafeCoerce

{- |
  Fixpoint recursion. Used to encode recursive functions.

  Example:

  > iterateN' ::
  >  Term edsl (PInteger #-> (a #-> a) #-> a #-> a) ->
  >  Term edsl PInteger ->
  >  Term edsl (a #-> a) ->
  >  Term edsl a
  > iterateN' self n f x =
  >   pif (n #== 0) x (self # n - 1 #$ f x)
  >
  > iterateN :: Term s (PInteger #-> (a #-> a) #-> a #-> a)
  > iterateN = pfix #$ plam iterateN'
  >

  Further examples can be found in examples/Recursion.hs
-}
pfix :: forall edsl a b. ()
     => PUntyped edsl 
     => PLC edsl 
     => PHoist edsl 
     => PConstructable edsl (POpaque edsl) 
     => IsPType edsl a 
     => IsPType edsl b 
     => Term edsl (((a #-> b) #-> (a #-> b)) #-> (a #-> b))
pfix = phoistAcyclic $ punsafeCoerce $ plam body where

  body :: Term edsl ((POpaque edsl #-> a) #-> b) -> Term edsl b
  body f = w # punsafeCoerce w where

    w :: Term edsl (POpaque edsl #-> b)
    w = plam \x -> f # plam \v -> punsafeCoerce x # x # v

-- data PInteger a

-- instance p ~ PInteger => Num (Term edsl p)

-- iterateN' 
--   :: forall edsl a. ()
--   => PEq edsl a
--   => PLC edsl 
--   => PEq edsl PInteger
--   => PConstructable edsl PBool
--   => IsPType edsl a
--   => Term edsl (PInteger #-> (a #-> a) #-> a #-> a) 
--   -> Term edsl PInteger 
--   -> Term edsl (a #-> a) 
--   -> Term edsl a 
--   -> Term edsl a
-- iterateN' self n f x = 
--    pif (n #== 0) x undefined  where

--  s :: Term edsl ((a #-> a) #-> (a #-> a))
--  s = self # neg n 1

--  neg = undefined 
