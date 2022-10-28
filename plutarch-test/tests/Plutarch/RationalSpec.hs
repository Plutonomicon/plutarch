module Plutarch.RationalSpec (spec) where

import Plutarch.Prelude
import Plutarch.Rational (pproperFraction, ptruncate)
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  let ratPPlutus' s => Term s PRational -> Term s PRational
      rat = id
      assertRat :: ClosedTerm PRational -> ClosedTerm PRational -> Expectation
      assertRat x p = passert $ p #== x
  describe "rational" . pgoldenSpec $ do
    "literal" @| rat 0.5
    "ops" @\ do
      "+" @| rat (1 / 2 + 1 / 2) @-> assertRat 1
      "-" @| rat (1 / 2 - 1 / 3) @-> assertRat (1 / 6)
      "*" @| rat ((1 - 3 / 2) * (2 - 5 / 2)) @-> assertRat (1 / 4)
      "harmonic-sum" @| rat (1 / 2 + 1 / 3 + 1 / 4 + 1 / 5)
        @-> assertRat (77 / 60)
      "multi-product" @| rat (1 / 2 * 2 / 3 * 3 / 4 * 4 / 5 * 5 / 6)
        @-> assertRat (1 / 6)
    "compare" @| rat (2 / 9) #< rat (3 / 10) @-> passert
    "round" @\ do
      -- NOTE: These will eventually be replaced by property tests.
      "5/3" @| pround # rat (5 / 3) @== pconstant @PInteger 2
      "4/3" @| pround # rat (4 / 3) @== pconstant @PInteger 1
      "-5/2" @| pround # rat (-5 / 2) @== pconstant @PInteger (-2)
      "-1/4" @| pround # rat (-1 / 4) @== pconstant @PInteger 0
    "truncate" @\ do
      "5/4" @| ptruncate # rat (5 / 4) @== pconstant @PInteger 1
      "7/4" @| ptruncate # rat (7 / 4) @== pconstant @PInteger 1
      "1/4" @| ptruncate # rat (1 / 4) @== pconstant @PInteger 0
      "-7/4" @| ptruncate # rat (-7 / 4) @== pconstant @PInteger (-1)
    "properFraction" @\ do
      let mkP r a b = pmatch (pproperFraction # r) $ \(PPair x y) ->
            x #== a #&& y #== b
      "-1/2" @| mkP (-1 / 2) 0 (-1 / 2) @-> passert
      "-3/2" @| mkP (-3 / 2) (-1) (-1 / 2) @-> passert
      "-4/3" @| mkP (-4 / 3) (-1) (-1 / 3) @-> passert
    "data" @\ do
      "id" @\ do
        "0.5" @| rat 0.5 @-> assertRat (pfromData (pdata 0.5))
        "2" @| rat 2 @-> assertRat (pfromData (pdata 2))
        "11/3" @| rat 11 / 3 @-> assertRat (pfromData (pdata $ 11 / 3))
    "div by 0" @\ do
      "1/0" @| ((1PPlutus' s => Term s PRational) / 0) @-> pfails
      "recip 0" @| recip (0PPlutus' s => Term s PRational) @-> pfails
      "1/(1-1)" @| ((1PPlutus' s => Term s PRational) / (1 - 1)) @-> pfails
