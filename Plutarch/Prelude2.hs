{-# Language UndecidableSuperClasses #-}
{-# Language UndecidableInstances #-}
{-# Language FlexibleInstances #-}
module Plutarch.Prelude2 (
  -- * eDSL types and functions.
  (:-->),
  PDelayed,
  Term,
  ClosedTerm,
  plam,
  papp,
  pdelay,
  pforce,
  phoistAcyclic,
  perror,
  (#$),
  (#),
  plet,
  pinl,
  pto,
  pfix,
  pthrow,
  Type,
  S,
  PType,
  PlutusType (PInner),
  DerivePlutusType,
  DPTStrat,
  PlutusTypeScott,
  PlutusTypeNewtype,
  PlutusTypeData,
  PCon,
  PMatch,
  pcon,
  pmatch,
  PForall (PForall),

  -- * Integers and integer utilities
  PInteger,
  PIntegral (pdiv, pmod, pquot, prem),

  -- * Rational numbers and utilities
  PRational (PRational),
  pnumerator,
  pdenominator,
  pround,

  -- * Booleans and boolean functions
  PBool (..),
  PEq ((#==)),
  PPartialOrd ((#<=), (#<)),
  POrd,
  pif,
  pnot,
  (#&&),
  (#||),

  -- * Bytestrings and bytestring utilities
  PByteString,
  phexByteStr,
  pconsBS,
  psliceBS,
  plengthBS,
  pindexBS,

  -- * String and string utilities
  PString,
  pencodeUtf8,
  pdecodeUtf8,

  -- * Unit type and utilities
  PUnit (..),

  -- * Common list typeclass and utilities
  PListLike (PElemConstraint, pelimList, pcons, pnil, phead, ptail, pnull),
  PIsListLike,
  plistEquals,
  pelem,
  pelemAt,
  plength,
  ptryIndex,
  pdrop,
  psingleton,
  pconcat,
  pzipWith,
  pzipWith',
  pzip,
  pmap,
  pfilter,
  pfind,
  precList,
  pfoldr,
  pfoldrLazy,
  pfoldl,
  pall,
  pany,
  (#!!),

  -- * Scott encoded list type
  PList (..),

  -- * Scott encoded maybe type and utilities
  PMaybe (..),

  -- * Scott encoded either type and utilities
  PEither (..),

  -- * Scott encoded pair type and utilities
  PPair (..),

  -- * Opaque type
  POpaque (POpaque),
  popaque,

  -- * Builtin types and utilities
  PData,
  pfstBuiltin,
  psndBuiltin,
  PBuiltinPair,
  PBuiltinList (..),
  PIsData,
  pfromData,
  pdata,
  PAsData,

  -- * DataRepr and related functions
  PDataRecord,
  PDataSum,
  PLabeledType ((:=)),
  pdcons,
  pdnil,
  pfield,
  getField,
  pletFields,

  -- * Tracing
  PShow,
  pshow,
  ptrace,
  ptraceShowId,
  ptraceIfFalse,
  ptraceIfTrue,
  ptraceError,

  -- * Cryptographic hashes and signatures
  psha2_256,
  psha3_256,
  pverifySignature,

  -- * Converstion between Plutarch terms and Haskell types
  pconstant,
  pconstantData,
  plift,
  PConstant,
  PLift,
  PConstantData,
  PLiftData,

  -- * Continuation monad
  TermCont (TermCont, runTermCont),
  unTermCont,
  tcont,
  pupcast,
  ptryFrom,
  PTryFrom,
  PSubtype,
  Generic,
) where

import Prelude ()

import Plutarch.Core 

import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.Records (getField)
-- import Plutarch
-- import Plutarch.Bool
-- import Plutarch.Builtin
-- import Plutarch.ByteString
-- import Plutarch.Crypto
-- import Plutarch.DataRepr
-- import Plutarch.Either
-- import Plutarch.Integer
import Plutarch.Internal2
-- import Plutarch.Lift
-- import Plutarch.List
-- import Plutarch.Maybe
-- import Plutarch.Pair
-- import Plutarch.Rational
-- import Plutarch.Show
-- import Plutarch.String
-- import Plutarch.TermCont
-- import Plutarch.Trace
-- import Plutarch.TryFrom
-- import Plutarch.Unit

class    PLC a => PLC' a
instance PLC a => PLC' a

class    PSOP a => PSOP' a
instance PSOP a => PSOP' a

type PPlutus = PLC' & PSOP' & PHoist & PForce


