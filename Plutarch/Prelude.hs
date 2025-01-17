module Plutarch.Prelude (
  PDataNewtype (..),

  -- * Boolean
  PBool (..),
  pand',
  pcond,
  pif,
  pif',
  pnot,
  por',
  (#&&),
  (#||),

  -- * Bytestring
  PByte,
  PByteString,
  PLogicOpSemantics,
  pandBS,
  pbyteToInteger,
  pcomplementBS,
  pconsBS,
  phexByteStr,
  pindexBS,
  pintegerToByte,
  plengthBS,
  porBS,
  ppadding,
  preplicateBS,
  psliceBS,
  ptruncation,
  pxorBS,

  -- * Cryptographic primitives
  psha2_256,
  psha3_256,
  pverifySignature,

  -- * Data encoding
  PAsData (..),
  PBuiltinList (..),
  PBuiltinPair (..),
  PData (..),
  pasByteStr,
  pasConstr,
  pasInt,
  pasList,
  pasMap,
  pchooseData,
  pchooseListBuiltin,
  pconsBuiltin,
  pconstrBuiltin,
  pfstBuiltin,
  pheadBuiltin,
  plistData,
  pnullBuiltin,
  ppairDataBuiltin,
  pserialiseData,
  psndBuiltin,
  ptailBuiltin,

  -- * Integer
  PInteger,

  -- * Opaque
  POpaque (..),
  popaque,

  -- * String
  PString,
  pdecodeUtf8,
  pencodeUtf8,

  -- * Unit
  PUnit (..),

  -- * DataRepr
  PDataFields,
  PDataRecord,
  PDataSum,
  PLabeledType ((:=)),
  PlutusTypeData,
  pdcons,
  pdnil,
  pfield,
  pletFields,

  -- * Either
  PEither (..),

  -- * Enumerable and Countable
  PCountable (..),
  PEnumerable (..),

  -- * Eq and Ord
  PEq (..),
  POrd (..),
  (#>),
  (#>=),

  -- * Fixed point
  pfix,

  -- * IsData
  PIsData (..),
  pdata,
  pforgetData,
  pfromData,

  -- * Lifting and lowering
  PLiftable (..),
  DeriveDataPLiftable,
  DeriveNewtypePLiftable,
  PLifted (..),
  reprToPlutUni,
  plutToReprUni,
  pconstant,
  plift,

  -- * Lists
  PElemConstraint,
  PIsListLike,
  PListLike,
  PList (..),
  pelem,
  pelemAt,
  pfind,
  plistEquals,
  puncons,
  pzip,
  (#!!),
  pall,
  pany,
  pconcat,
  pcons,
  pdrop,
  pelimList,
  pfilter,
  pfoldl,
  pfoldr,
  pfoldrLazy,
  phead,
  plength,
  pmap,
  pnil,
  pnull,
  precList,
  psingleton,
  ptail,
  ptryIndex,
  pzipWith,
  pzipWith',

  -- * PlutusType
  DerivePlutusType (DPTStrat),
  PCon,
  PMatch,
  PlutusType (PInner),
  pcon,
  pmatch,
  PlutusTypeNewtype,
  PlutusTypeScott,

  -- * Numeric
  Positive,
  PPositive,
  PNatural,
  PAdditiveSemigroup (..),
  PAdditiveMonoid (..),
  PAdditiveGroup (..),
  PMultiplicativeSemigroup (..),
  PMultiplicativeMonoid (..),
  PRing (..),
  PIntegralDomain (..),
  pquot,
  prem,
  pdiv,
  pmod,
  ppositive,
  ptryPositive,
  pnatural,
  ptryNatural,
  ppositiveToNatural,

  -- * Other
  pto,
  pinl,
  plam,
  PForall (..),

  -- * Show
  PShow,
  pshow,

  -- * Term and related functionality
  Term,
  S,
  ClosedTerm,
  PDelayed,
  papp,
  pdelay,
  pforce,
  perror,
  phoistAcyclic,
  plet,
  pthrow,
  (#),
  (#$),
  (:-->),

  -- * Conversion
  PSubtype,
  PTryFrom (..),
  ptryFrom,
  pupcast,

  -- * Maybe
  PMaybe (..),

  -- * Pair
  PPair (..),

  -- * Rational
  PRational (..),
  pdenominator,
  pnumerator,
  pround,

  -- * TermCont
  TermCont (..),
  pguardC,
  pguardC',
  pletC,
  pletFieldsC,
  pmatchC,
  ptraceC,
  ptryFromC,
  unTermCont,
  tcont,

  -- * Tracing
  ptrace,
  ptraceDebug,
  ptraceDebugError,
  ptraceDebugIfFalse,
  ptraceDebugIfTrue,
  ptraceDebugShowId,
  ptraceError,
  ptraceIfFalse,
  ptraceInfo,
  ptraceInfoError,
  ptraceInfoIfFalse,
  ptraceInfoIfTrue,
  ptraceInfoShowId,
  ptraceShowId,

  -- * Semigroup and monoid
  PSemigroup (..),
  PMonoid (..),
  PAnd (..),
  POr (..),
  PXor (..),
) where

import Plutarch.Builtin
import Plutarch.Builtin.Bool
import Plutarch.Builtin.ByteString
import Plutarch.Builtin.Crypto
import Plutarch.Builtin.Data
import Plutarch.Builtin.Integer
import Plutarch.Builtin.Opaque
import Plutarch.Builtin.String
import Plutarch.Builtin.Unit
import Plutarch.DataRepr
import Plutarch.Either
import Plutarch.Enum
import Plutarch.Internal.Eq
import Plutarch.Internal.Fix
import Plutarch.Internal.IsData
import Plutarch.Internal.Lift
import Plutarch.Internal.ListLike
import Plutarch.Internal.Newtype
import Plutarch.Internal.Numeric
import Plutarch.Internal.Ord
import Plutarch.Internal.Other
import Plutarch.Internal.PLam
import Plutarch.Internal.PlutusType
import Plutarch.Internal.Quantification
import Plutarch.Internal.ScottEncoding
import Plutarch.Internal.Semigroup
import Plutarch.Internal.Show
import Plutarch.Internal.Term
import Plutarch.Internal.TryFrom
import Plutarch.List
import Plutarch.Maybe
import Plutarch.Pair
import Plutarch.Rational
import Plutarch.TermCont
import Plutarch.Trace
import Prelude ()
