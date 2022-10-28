{-# Options_GHC -w #-}
{-# Language FlexibleInstances #-}
{-# Language LiberalTypeSynonyms #-}
{-# Language ImpredicativeTypes #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}

module Plutarch.Internal2 where

import Data.Coerce
import Data.Kind
import Data.String (fromString)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Except (ExceptT(..), Except)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT), asks)
import Control.Monad.Zip (MonadZip)
import Data.Default (Default (def))
import Data.Functor.Identity (Identity(..))
import Data.List (foldl', groupBy, sortOn)
import qualified Data.ByteString as BS
import Crypto.Hash.IO (HashAlgorithm)
import qualified Flat.Run as F
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Word (Word64)

import Crypto.Hash (Context, Digest, hashFinalize, hashInit, hashUpdate)
import Crypto.Hash.Algorithms (Blake2b_160)
import PlutusCore (Some (Some), ValueOf (ValueOf))
import PlutusCore qualified as PLC
import PlutusCore.DeBruijn (DeBruijn (DeBruijn), Index (Index))
import PlutusCore.Default (DefaultUni(..))
import PlutusLedgerApi.V1.Scripts (Script (Script))
import UntypedPlutusCore qualified as UPLC
import Data.Functor ((<&>))

-- import Control.Applicative (Alternative)
-- import Control.Monad (MonadPlus)
-- import Control.Monad.Except (ExceptT(..), Except)
-- import Control.Monad.Fix (MonadFix)
-- import Control.Monad.Reader (MonadReader, runReaderT, ReaderT (ReaderT), asks)
-- import Control.Monad.Zip (MonadZip)
-- import Data.Functor.Identity (Identity(..))
-- import Crypto.Hash (Context, Digest, hashFinalize, hashInit, hashUpdate)
-- import Crypto.Hash.Algorithms (Blake2b_160)
-- import Crypto.Hash.IO (HashAlgorithm)
-- import qualified Data.ByteString as BS
-- import Data.Default (Default (def))
-- import Data.Kind (Type)
-- import Data.List (foldl', groupBy, sortOn)
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
-- import Data.String (fromString)
-- import Data.Text (Text)
-- import qualified Flat.Run as F
-- import GHC.Stack (HasCallStack, callStack, prettyCallStack)
-- import GHC.Word (Word64)
import Plutarch.Internal.Evaluate (evalScript)
-- import PlutusCore (Some (Some), ValueOf (ValueOf))
-- import qualified PlutusCore as PLC
-- import PlutusCore.DeBruijn (DeBruijn (DeBruijn), Index (Index))
-- import PlutusLedgerApi.V1.Scripts (Script (Script))
-- import qualified UntypedPlutusCore as UPLC

import Plutarch.PType
import Plutarch.Core 
import Plutarch.Lam 
import Plutarch.Core qualified as Core

type Dig = Digest Blake2b_160

data HoistedTerm = HoistedTerm Dig RawTerm
  deriving stock (Show)

type UTerm :: Type
type UTerm = UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()

data RawTerm
  = RVar Word64
  | RLamAbs Word64 RawTerm
  | RApply RawTerm [RawTerm]
  | RForce RawTerm
  | RDelay RawTerm
  | RConstant (Some (ValueOf PLC.DefaultUni))
  | RBuiltin PLC.DefaultFun
  | RCompiled UTerm
  | RError
  | RHoisted HoistedTerm
  deriving stock (Show)

data TermResult = TermResult
  { getTerm :: RawTerm
  , getDeps :: [HoistedTerm]
  }

mapTerm :: (RawTerm -> RawTerm) -> TermResult -> TermResult
mapTerm f (TermResult t d) = TermResult (f t) d

mkTermRes :: RawTerm -> TermResult
mkTermRes r = TermResult r []

data Config = Config
  { tracingMode :: TracingMode
  }

data TracingMode = NoTracing | DoTracing | DetTracing

-- | Default is to be efficient
instance Default Config where
  def =
    Config
      { tracingMode = NoTracing
      }

newtype TermMonad m = TermMonad {runTermMonad :: Config -> Either Text m}
  deriving
    ( Functor, Applicative, Alternative
    , Monad, MonadPlus, MonadFix, MonadZip, MonadReader Config
    )
  via ReaderT Config (Except Text)

pgetConfig :: (Config -> ef /$ a) -> ef /$ a
pgetConfig f = undefined 
-- pgetConfig f = Term \lvl -> TermMonad $ do
--   config <- ask
--   runTermMonad $ asRawTerm (f config) lvl

deBruijnInitIndex :: Word64
deBruijnInitIndex = 0

succIndex :: Word64 -> Word64
succIndex = succ

type    Impl :: PType -> Type
newtype Impl a = Impl { runImpl :: Word64 -> TermMonad TermResult }

type Impl' :: PDSLKind
type Impl' = 'PDSLKind Impl

{-# Complete TheImpl #-}

type TheImpl :: PType -> Type
type TheImpl = Term Impl'

pattern TheImpl :: (Word64 -> TermMonad TermResult) -> Term Impl' a
pattern TheImpl { runTheImpl } = Term (Impl runTheImpl)

instance PDSL Impl'

instance Applicative f => PConstructable' Impl' PUnit where
  pconImpl :: PConcrete Impl' PUnit -> Impl PUnit
  pconImpl PUnit = Impl \_ -> pure unit
    where
    unit :: TermResult
    unit = TermResult 
      { getTerm = RConstant (Some (ValueOf DefaultUniUnit ()))
      , getDeps = []
      }

  pmatchImpl :: Impl PUnit -> (PConcrete Impl' PUnit -> Term Impl' b) -> Term Impl' b
  pmatchImpl _ next = next PUnit

instance PConstructable' Impl' (PLet a) where
  pconImpl :: PConcrete Impl' (PLet a) -> Impl (PLet a)
  pconImpl (PLet term) = coerce term

  pmatchImpl :: Impl (PLet a) -> (PConcrete Impl' (PLet a) -> Term Impl' b) -> Term Impl' b
  pmatchImpl term next = next do
    PLet (coerce term)

instance PConstructable' Impl' (a #-> b) where
  pconImpl :: PConcrete Impl' (a #-> b) -> Impl (a #-> b)
  pconImpl (PLam body) = Impl \lvl -> let

    f :: TermResult -> TermResult
    f t = case getTerm t of
      -- eta-reduce for arity 1
      RApply t'@(getArity -> Just _) [RVar 0] -> t {getTerm = t'}
      -- eta-reduce for arity 2 + n
      RLamAbs n (RApply t'@(getArity -> Just n') args)
        | (== Just [0 .. n + 1]) (traverse (\case RVar n -> Just n; _ -> Nothing) args)
            && n' >= n + 1 ->
            t {getTerm = t'}
      -- increment arity
      RLamAbs n t' -> t {getTerm = RLamAbs (n + 1) t'}
      -- new lambda
      _ -> mapTerm (RLamAbs 0) t

    x :: TermMonad TermResult
    x = runTheImpl (body v) (lvl + 1)

    v :: TheImpl a
    v = TheImpl \j -> pure $ mkTermRes $ RVar (j - (lvl + 1))

    in 
    f <$> x

  pmatchImpl :: IsPType Impl' x => Impl (a #-> b) -> (PConcrete Impl' (a #-> b) -> Term Impl' x) -> Term Impl' x
  pmatchImpl (Impl fun) next = next $ PLam \(TheImpl term) -> TheImpl \lvl -> 
    (,) <$> fun lvl <*> term lvl >>= \case
      -- Applying anything to an error is an error.
      (getTerm -> RError, _) -> pthrow' "application to an error"
      -- Applying an error to anything is an error.
      (_, getTerm -> RError) -> pthrow' "application with an error"
      -- Applying to `id` changes nothing.
      (getTerm -> RLamAbs 0 (RVar 0), y') -> pure y'
      (getTerm -> RHoisted (HoistedTerm _ (RLamAbs 0 (RVar 0))), y') -> pure y'
      -- append argument
      (x'@(getTerm -> RApply x'l x'r), y') -> pure $ TermResult (RApply x'l (getTerm y' : x'r)) (getDeps x' <> getDeps y')
      -- new RApply
      (x', y') -> pure $ TermResult (RApply (getTerm x') [getTerm y']) (getDeps x' <> getDeps y')

instance (PHasRepr a, PHasRepr b) => PConstructable' Impl' (PPair a b) where
  pconImpl :: PConcrete Impl' (PPair a b) -> Impl (PPair a b)
  pconImpl (PPair (TheImpl a) (TheImpl b)) = coerce (plam body) where

    body :: Term Impl' (a #-> b #-> PPair a b)
         -> Term Impl' (PPair a b)
    body pair = pair # TheImpl a # TheImpl b

  pmatchImpl
    :: forall x. ()
    => IsPType Impl' x
    => Impl (PPair a b)
    -> (PConcrete Impl' (PPair a b)
    -> Term Impl' x)
    -> Term Impl' x
  pmatchImpl (Impl term) next = undefined  where

    body :: Term Impl' x
    body = plet (TheImpl term) \(TheImpl thePair) -> let

      one :: Term Impl' a
      two :: Term Impl' b
      one = TheImpl thePair # plam \a b -> a
      two = TheImpl thePair # plam \a b -> b

      in next (PPair one two)

instance m ~ TermMonad => PAp m Impl' where
  papr :: TermMonad a -> Term Impl' b -> Term Impl' b
  papr as (TheImpl term) = TheImpl \lvl -> as *> term lvl

  papl :: Term Impl' a -> TermMonad b -> Term Impl' a
  papl (TheImpl term) bs = TheImpl \lvl -> term lvl <* bs
  
instance m ~ TermMonad => PEmbeds m Impl' where
  pembed :: TermMonad (Term Impl' a) -> Term Impl' a
  pembed terms = TheImpl \lvl -> do
    impl <- terms
    runTheImpl impl lvl 

-- asClosedRawTerm :: forall cls a edsl. ClosedTerm cls a -> cls edsl => UnEDSLKind edsl (PRepr a)
-- asClosedRawTerm closed
--   | Term term <- closed @edsl
--   = term

-- asClosedRawTerm' :: ClosedTerm cls a
--                  -> (cls Impl' => TermMonad TermResult)
-- asClosedRawTerm' (TheImpl term) = term deBruijnInitIndex

-- Couldn't find a definition for this in plutus-core
subst :: Word64 -> (Word64 -> UTerm) -> UTerm -> UTerm
subst idx x (UPLC.Apply () yx yy) = UPLC.Apply () (subst idx x yx) (subst idx x yy)
subst idx x (UPLC.LamAbs () name y) = UPLC.LamAbs () name (subst (idx + 1) x y)
subst idx x (UPLC.Delay () y) = UPLC.Delay () (subst idx x y)
subst idx x (UPLC.Force () y) = UPLC.Force () (subst idx x y)
subst idx x (UPLC.Var () (DeBruijn (Index idx'))) | idx == idx' = x idx
subst idx _ y@(UPLC.Var () (DeBruijn (Index idx'))) | idx > idx' = y
subst idx _ (UPLC.Var () (DeBruijn (Index idx'))) | idx < idx' = UPLC.Var () (DeBruijn . Index $ idx' - 1)
subst _ _ y = y

rawTermToUPLC ::
  (HoistedTerm -> Word64 -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()) ->
  Word64 ->
  RawTerm ->
  UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
rawTermToUPLC _ _ (RVar i) = UPLC.Var () (DeBruijn . Index $ i + 1) -- Why the fuck does it start from 1 and not 0?
rawTermToUPLC m l (RLamAbs n t) =
  foldr
    (.)
    id
    (replicate (fromIntegral $ n + 1) $ UPLC.LamAbs () (DeBruijn . Index $ 0))
    $ (rawTermToUPLC m (l + n + 1) t)
rawTermToUPLC m l (RApply x y) =
  let f y t@(UPLC.LamAbs () _ body) =
        case rawTermToUPLC m l y of
          -- Inline unconditionally if it's a variable or built-in.
          -- These terms are very small and are always WHNF.
          UPLC.Var () (DeBruijn (Index idx)) -> subst 1 (\lvl -> UPLC.Var () (DeBruijn . Index $ idx + lvl - 1)) body
          arg@UPLC.Builtin {} -> subst 1 (\_ -> arg) body
          arg -> UPLC.Apply () t arg
      f y t = UPLC.Apply () t (rawTermToUPLC m l y)
   in foldr (.) id (f <$> y) $ (rawTermToUPLC m l x)
rawTermToUPLC m l (RDelay t) = UPLC.Delay () (rawTermToUPLC m l t)
rawTermToUPLC m l (RForce t) = UPLC.Force () (rawTermToUPLC m l t)
rawTermToUPLC _ _ (RBuiltin f) = UPLC.Builtin () f
rawTermToUPLC _ _ (RConstant c) = UPLC.Constant () c
rawTermToUPLC _ _ (RCompiled code) = code
rawTermToUPLC _ _ RError = UPLC.Error ()
-- rawTermToUPLC m l (RHoisted hoisted) = UPLC.Var () . DeBruijn . Index $ l - m hoisted
rawTermToUPLC m l (RHoisted hoisted) = m hoisted l -- UPLC.Var () . DeBruijn . Index $ l - m hoisted

-- The logic is mostly for hoisting
compile' :: TermResult -> UTerm
compile' t =
  let t' = getTerm t
      deps = getDeps t

      f :: Word64 -> Maybe Word64 -> (Bool, Maybe Word64)
      f n Nothing = (True, Just n)
      f _ (Just n) = (False, Just n)

      g ::
        HoistedTerm ->
        (M.Map Dig Word64, [(Word64, RawTerm)], Word64) ->
        (M.Map Dig Word64, [(Word64, RawTerm)], Word64)
      g (HoistedTerm hash term) (map, defs, n) = case M.alterF (f n) hash map of
        (True, map) -> (map, (n, term) : defs, n + 1)
        (False, map) -> (map, defs, n)

      toInline :: S.Set Dig
      toInline =
        S.fromList
          . fmap (\(HoistedTerm hash _) -> hash)
          . (head <$>)
          . filter ((== 1) . length)
          . groupBy (\(HoistedTerm x _) (HoistedTerm y _) -> x == y)
          . sortOn (\(HoistedTerm hash _) -> hash)
          $ deps

      -- map: term -> de Bruijn level
      -- defs: the terms, level 0 is last
      -- n: # of terms
      (map, defs, n) = foldr g (M.empty, [], 0) $ filter (\(HoistedTerm hash _) -> not $ S.member hash toInline) deps

      map' (HoistedTerm hash term) l = case M.lookup hash map of
        Just l' -> UPLC.Var () . DeBruijn . Index $ l - l'
        Nothing -> rawTermToUPLC map' l term

      body = rawTermToUPLC map' n t'

      wrapped =
        foldl'
          (\b (lvl, def) -> UPLC.Apply () (UPLC.LamAbs () (DeBruijn . Index $ 0) b) (rawTermToUPLC map' lvl def))
          body
          defs
   in wrapped

-- compile :: forall a. IsPType Impl' a => TheImpl a -> TermMonad TermResult
-- compile (TheImpl term) = term deBruijnInitIndex

-- compile :: forall cls a. Config -> ClosedTerm cls a -> Either Text Script
-- compile config t = case asClosedRawTerm @cls @a t of
--   TermMonad ok -> undefined 
--   -- TermMonad t' -> (Script . UPLC.Program () (UPLC.Version () 1 0 0) . compile') <$> t' config


-- -- -- type EUPLC :: EDSLKind -> Constraint
-- -- type EUPLC :: PDSLKind -> Constraint
-- -- type EUPLC edsl = () -- PLC edsl --, PPolymorphic edsl, PSOP edsl)

-- -- compile 
-- --   :: forall m a. HasCallStack
-- --   => Monad m
-- --   => forall edsl. EUPLC edsl => IsPType edsl a
-- --   => (forall edsl. (EUPLC edsl, PEmbeds m edsl) => Term edsl a) 
-- --   -> m UTerm
-- -- compile t = let _unused = callStack in compile' @a @m t
pthrow' :: HasCallStack => Text -> TermMonad a
pthrow' msg = TermMonad $ const $ Left (fromString (prettyCallStack callStack) <> "\n\n" <> msg)

pthrow :: HasCallStack => Text -> Impl a
pthrow = Impl . pure . pthrow'

getArity :: RawTerm -> Maybe Word64
-- We only do this if it's hoisted, since it's only safe if it doesn't
-- refer to any of the variables in the wrapping lambda.
getArity (RHoisted (HoistedTerm _ (RLamAbs n _))) = Just n
getArity (RHoisted (HoistedTerm _ t)) = getArityBuiltin t
getArity t = getArityBuiltin t

getArityBuiltin :: RawTerm -> Maybe Word64
getArityBuiltin (RBuiltin PLC.AddInteger) = Just 1
getArityBuiltin (RBuiltin PLC.SubtractInteger) = Just 1
getArityBuiltin (RBuiltin PLC.MultiplyInteger) = Just 1
getArityBuiltin (RBuiltin PLC.DivideInteger) = Just 1
getArityBuiltin (RBuiltin PLC.QuotientInteger) = Just 1
getArityBuiltin (RBuiltin PLC.RemainderInteger) = Just 1
getArityBuiltin (RBuiltin PLC.ModInteger) = Just 1
getArityBuiltin (RBuiltin PLC.EqualsInteger) = Just 1
getArityBuiltin (RBuiltin PLC.LessThanInteger) = Just 1
getArityBuiltin (RBuiltin PLC.LessThanEqualsInteger) = Just 1
getArityBuiltin (RBuiltin PLC.AppendByteString) = Just 1
getArityBuiltin (RBuiltin PLC.ConsByteString) = Just 1
getArityBuiltin (RBuiltin PLC.SliceByteString) = Just 2
getArityBuiltin (RBuiltin PLC.LengthOfByteString) = Just 0
getArityBuiltin (RBuiltin PLC.IndexByteString) = Just 1
getArityBuiltin (RBuiltin PLC.EqualsByteString) = Just 1
getArityBuiltin (RBuiltin PLC.LessThanByteString) = Just 1
getArityBuiltin (RBuiltin PLC.LessThanEqualsByteString) = Just 1
getArityBuiltin (RBuiltin PLC.Sha2_256) = Just 0
getArityBuiltin (RBuiltin PLC.Sha3_256) = Just 0
getArityBuiltin (RBuiltin PLC.Blake2b_256) = Just 0
getArityBuiltin (RBuiltin PLC.VerifyEd25519Signature) = Just 2
getArityBuiltin (RBuiltin PLC.VerifyEcdsaSecp256k1Signature) = Just 2
getArityBuiltin (RBuiltin PLC.VerifySchnorrSecp256k1Signature) = Just 2
getArityBuiltin (RBuiltin PLC.AppendString) = Just 1
getArityBuiltin (RBuiltin PLC.EqualsString) = Just 1
getArityBuiltin (RBuiltin PLC.EncodeUtf8) = Just 0
getArityBuiltin (RBuiltin PLC.DecodeUtf8) = Just 0
getArityBuiltin (RForce (RBuiltin PLC.IfThenElse)) = Just 2
getArityBuiltin (RForce (RBuiltin PLC.ChooseUnit)) = Just 1
getArityBuiltin (RForce (RBuiltin PLC.Trace)) = Just 1
getArityBuiltin (RForce (RForce (RBuiltin PLC.FstPair))) = Just 0
getArityBuiltin (RForce (RForce (RBuiltin PLC.SndPair))) = Just 0
getArityBuiltin (RForce (RForce (RBuiltin PLC.ChooseList))) = Just 2
getArityBuiltin (RForce (RBuiltin PLC.MkCons)) = Just 1
getArityBuiltin (RForce (RBuiltin PLC.HeadList)) = Just 0
getArityBuiltin (RForce (RBuiltin PLC.TailList)) = Just 0
getArityBuiltin (RForce (RBuiltin PLC.NullList)) = Just 0
getArityBuiltin (RForce (RBuiltin PLC.ChooseData)) = Just 5
getArityBuiltin (RBuiltin PLC.ConstrData) = Just 1
getArityBuiltin (RBuiltin PLC.MapData) = Just 0
getArityBuiltin (RBuiltin PLC.ListData) = Just 0
getArityBuiltin (RBuiltin PLC.IData) = Just 0
getArityBuiltin (RBuiltin PLC.BData) = Just 0
getArityBuiltin (RBuiltin PLC.UnConstrData) = Just 0
getArityBuiltin (RBuiltin PLC.UnMapData) = Just 0
getArityBuiltin (RBuiltin PLC.UnListData) = Just 0
getArityBuiltin (RBuiltin PLC.UnIData) = Just 0
getArityBuiltin (RBuiltin PLC.UnBData) = Just 0
getArityBuiltin (RBuiltin PLC.EqualsData) = Just 1
getArityBuiltin (RBuiltin PLC.MkPairData) = Just 1
getArityBuiltin (RBuiltin PLC.MkNilData) = Just 0
getArityBuiltin (RBuiltin PLC.MkNilPairData) = Just 0
getArityBuiltin _ = Nothing

hashRawTerm' :: HashAlgorithm alg => RawTerm -> Context alg -> Context alg
hashRawTerm' (RVar x) = flip hashUpdate ("0" :: BS.ByteString) . flip hashUpdate (F.flat (fromIntegral x :: Integer))
hashRawTerm' (RLamAbs n x) =
  flip hashUpdate ("1" :: BS.ByteString) . flip hashUpdate (F.flat (fromIntegral n :: Integer)) . hashRawTerm' x
hashRawTerm' (RApply x y) =
  flip hashUpdate ("2" :: BS.ByteString) . hashRawTerm' x . flip (foldl' $ flip hashRawTerm') y
hashRawTerm' (RForce x) = flip hashUpdate ("3" :: BS.ByteString) . hashRawTerm' x
hashRawTerm' (RDelay x) = flip hashUpdate ("4" :: BS.ByteString) . hashRawTerm' x
hashRawTerm' (RConstant x) = flip hashUpdate ("5" :: BS.ByteString) . flip hashUpdate (F.flat x)
hashRawTerm' (RBuiltin x) = flip hashUpdate ("6" :: BS.ByteString) . flip hashUpdate (F.flat x)
hashRawTerm' RError = flip hashUpdate ("7" :: BS.ByteString)
hashRawTerm' (RHoisted (HoistedTerm hash _)) = flip hashUpdate ("8" :: BS.ByteString) . flip hashUpdate hash
hashRawTerm' (RCompiled code) = flip hashUpdate ("9" :: BS.ByteString) . flip hashUpdate (F.flat code)

hashRawTerm :: RawTerm -> Dig
hashRawTerm t = hashFinalize . hashRawTerm' t $ hashInit

punsafeBuiltin :: UPLC.DefaultFun -> TheImpl a
punsafeBuiltin f = TheImpl \_ -> pure $ mkTermRes $ RBuiltin f

punsafeConstantInternal :: Some (ValueOf PLC.DefaultUni) -> Impl a
punsafeConstantInternal c = Impl \_ -> pure 
  case c of
    -- These constants are smaller than variable references.
    Some (ValueOf PLC.DefaultUniBool _) -> mkTermRes $ RConstant c
    Some (ValueOf PLC.DefaultUniUnit _) -> mkTermRes $ RConstant c
    Some (ValueOf PLC.DefaultUniInteger n) | n < 256 -> mkTermRes $ RConstant c
    _ ->
      let hoisted = HoistedTerm (hashRawTerm $ RConstant c) (RConstant c)
       in TermResult (RHoisted hoisted) [hoisted]

-- instance PForce Impl' where
--   pforce :: TheImpl (PDelayed a) -> TheImpl a
--   pforce (TheImpl x) = TheImpl \i ->
--     x i <&> \case
--       -- A force cancels a delay
--       t@(getTerm -> RDelay t') -> t {getTerm = t'}
--       t -> mapTerm RForce t

--   pdelay :: TheImpl a -> TheImpl (PDelayed a)
--   pdelay (TheImpl x) = TheImpl \i -> mapTerm RDelay <$> x i

-- instance PHoist Impl' where
--   -- FIXME: Give proper error message when mutually recursive.
--   phoistAcyclic :: HasCallStack => TheImpl a -> TheImpl a
--   phoistAcyclic (TheImpl t) = TheImpl \_ -> 
--     t 0 >>= \case
--       -- Built-ins are smaller than variable references
--       t'@(getTerm -> RBuiltin _) -> pure $ t'
--       t' -> case evalScript . Script . UPLC.Program () (PLC.defaultVersion ()) $ compile' t' of
--         (Right _, _, _) ->
--           let hoisted = HoistedTerm (hashRawTerm . getTerm $ t') (getTerm t')
--            in pure $ TermResult (RHoisted hoisted) (hoisted : getDeps t')
--         (Left e, _, _) -> pthrow' $ "Hoisted term errs! " <> fromString (show e)


{- |
  Low precedence infixr synonym of 'papp', to be used like
  '$', in combination with '#'. e.g.:

  >>> f # x #$ g # y # z
  f x (g y z)
-}
infixr 0 #$
(#$) :: HasCallStack
     => PLC edsl
     => IsPType edsl a
     => IsPType edsl b
     => Term edsl (a #-> b) -> Term edsl a -> Term edsl b
(#$) = (#)
