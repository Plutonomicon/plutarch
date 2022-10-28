module Plutarch.IntegerSpec (spec) where

import Plutarch
import Plutarch.Prelude
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "int" $ do
    describe "examples" $ do
      pgoldenSpec $ do
        "add1" @| add1
        "add1Hoisted" @| add1Hoisted
        "example1" @| example1
        "example2" @| example2
        "fib" @\ do
          "lam" @| fib
          "app" @\ do
            "9" @| fib # 9 @:-> \(p, _script, bench) -> do
              p `pshouldBe` (34 :: Term _ PInteger)
              bench `psatisfyWithinBenchmark` Benchmark 1_000_000_000 1_000_000 100
        "uglyDouble" @| uglyDouble

add1PPlutus' s => Term s (PInteger #-> PInteger #-> PInteger)
add1 = plam $ \x y -> x + y + 1

add1HoistedPPlutus' s => Term s (PInteger #-> PInteger #-> PInteger)
add1Hoisted = phoistAcyclic $ plam $ \x y -> x + y + 1

example1PPlutus' s => Term s PInteger
example1 = add1Hoisted # 12 # 32 + add1Hoisted # 5 # 4

example2PPlutus' s => Term s (PEither PInteger PInteger #-> PInteger)
example2 = plam $ \x -> pmatch x $ \case
  PLeft n -> n + 1
  PRight n -> n - 1

fibPPlutus' s => Term s (PInteger #-> PInteger)
fib = phoistAcyclic $
  pfix #$ plam $ \self n ->
    pif
      (n #== 0)
      0
      $ pif
        (n #== 1)
        1
        $ self # (n - 1) + self # (n - 2)

uglyDoublePPlutus' s => Term s (PInteger #-> PInteger)
uglyDouble = plam $ \n -> plet n $ \n1 -> plet n1 $ \n2 -> n2 + n2
