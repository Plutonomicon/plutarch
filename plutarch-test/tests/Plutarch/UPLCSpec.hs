module Plutarch.UPLCSpec (spec) where

import qualified PlutusCore as PLC

import Plutarch.Internal (punsafeConstantInternal)
import Plutarch.Prelude
import Plutarch.Test
import Plutarch.Unsafe (punsafeBuiltin)
import Test.Hspec

spec :: Spec
spec = do
  -- Tests for the behaviour of UPLC itself.
  describe "uplc-behaviour" . pgoldenSpec $ do
    "2:[1]"
      @| ( let l :: Term _ (PBuiltinList PInteger) =
                punsafeConstantInternal . PLC.Some $
                  PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
            in pforce (punsafeBuiltin PLC.MkCons) # (2 :: Term _ PInteger) # l
         )
    "fails:True:[1]"
      @| ( let l :: Term _ (PBuiltinList POpaque) =
                punsafeConstantInternal . PLC.Some $
                  PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
            in pforce (punsafeBuiltin PLC.MkCons) # pcon PTrue # l @-> pfails
         )
    "(2,1)"
      @| punsafeConstantInternal . PLC.Some
      $ PLC.ValueOf
        ( PLC.DefaultUniApply
            (PLC.DefaultUniApply PLC.DefaultUniProtoPair PLC.DefaultUniInteger)
            PLC.DefaultUniInteger
        )
        (1, 2)
    "fails:MkPair-1-2"
      @| punsafeBuiltin PLC.MkPairData # (1 :: Term _ PInteger) # (2 :: Term _ PInteger)
        @-> pfails
  describe "uplc-misc" . pgoldenSpec $ do
    "perror" @| perror @-> pfails
    -- FIXME readd test
    -- "perror.arg" @| perror # (1PPlutus' s => Term s PInteger) @-> pfails
    "laziness" @\ do
      "f.d" @| (pforce . pdelay $ (0PPlutus' s => Term s PInteger))
      "d.f.d" @| (pdelay . pforce . pdelay $ (0PPlutus' s => Term s PInteger))
    "hoist" @\ do
      -- hoist id 0 => 0
      "id.0" @| phoistAcyclic $ plam $ \x -> x # (0PPlutus' s => Term s PInteger)
      -- hoist fstPair => fstPair
      "fstPair" @| phoistAcyclic (punsafeBuiltin PLC.FstPair)
