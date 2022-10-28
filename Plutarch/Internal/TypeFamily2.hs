{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.TypeFamily2 (ToPType, ToPType2, UnTerm, Snd) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (Text), TypeError)
import Plutarch.PType (PType)
import Plutarch.Core (Term)

-- | Convert a list of `Term s a` to a list of `a`.
type ToPType :: [Type] -> [PType]
type family ToPType as where
  ToPType '[] = '[]
  ToPType (a ': as) = UnTerm a ': ToPType as

type ToPType2 :: [[Type]] -> [[PType]]
type family ToPType2 as where
  ToPType2 '[] = '[]
  ToPType2 (a ': as) = ToPType a ': ToPType2 as

type UnTerm :: Type -> PType
type family UnTerm x where
  UnTerm (Term _ a) = a
  UnTerm _ = TypeError ( 'Text "Non-term in Plutarch data type not allowed")

type family Snd ab where
  Snd '(_, b) = b
