module Plutarch.RecursionSpec (spec) where

import Prelude hiding (succ)

import Plutarch
import Plutarch.Bool (pif, (#==))
import Plutarch.Integer (PInteger)

import Plutarch.Lift (pconstant)
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "recursion" $ do
    describe "example" . pgoldenSpec $ do
      "iterateN" @\ do
        "lam" @| iterateN
        "app" @\ do
          "succ" @| iterateN # 10 # succ # 0
            @== pconstant @PInteger 10
          "double" @| iterateN # 10 # double # 1
            @== pconstant @PInteger 1024

succPPlutus' s => Term s (PInteger #-> PInteger)
succ = plam $ \x -> x + 1

doublePPlutus' s => Term s (PInteger #-> PInteger)
double = plam $ \x -> x * 2

{- |
  Example of a simple recursive function with pfix
-}
iterateNPPlutus' s => Term s (PInteger #-> (a #-> a) #-> a #-> a)
iterateN = pfix # plam iterateN'
  where
    iterateN' ::
      Term s (PInteger #-> (a #-> a) #-> a #-> a) ->
      Term s PInteger ->
      Term s (a #-> a) ->
      Term s a ->
      Term s a

    iterateN' self n f x =
      pif
        (n #== 0)
        x
        (self # (n - 1) # f #$ f # x)
