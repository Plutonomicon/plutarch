{-# LANGUAGE TemplateHaskell #-}

module Examples.LetRec (tests) where

import Plutarch (pcon', pmatch', printTerm)
import Plutarch.Bool (PBool (PFalse, PTrue), pif, (#==))
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import Plutarch.Rec (PRecord (PRecord), ScottEncoded, ScottEncoding, field, letrec)
import Plutarch.Rec.TH (deriveAll)
import Plutarch.String (PString)
import qualified Rank2.TH
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Utils
import Prelude hiding (even, odd)

data SampleRecord f = SampleRecord
  { sampleBool :: f PBool
  , sampleInt :: f PInteger
  , sampleString :: f PString
  }

data EvenOdd f = EvenOdd
  { even :: f (PInteger :--> PBool)
  , odd :: f (PInteger :--> PBool)
  }

type instance ScottEncoded EvenOdd a = (PInteger :--> PBool) :--> (PInteger :--> PBool) :--> a

$(Rank2.TH.deriveAll ''EvenOdd)
$(deriveAll ''SampleRecord) -- also autoderives the @type instance ScottEncoded@

sampleRecord :: Term (s :: k) (ScottEncoding SampleRecord (t :: k -> Type))
sampleRecord =
  pcon' $
    PRecord
      SampleRecord
        { sampleBool = pcon PFalse
        , sampleInt = 6
        , sampleString = "Salut, Monde!"
        }

sampleRecur :: Term (s :: k) (ScottEncoding SampleRecord (t :: k -> Type))
sampleRecur =
  letrec $
    const
      SampleRecord
        { sampleBool = pcon PTrue
        , sampleInt = 12
        , sampleString = "Hello, World!"
        }

evenOdd :: Term (s :: k) (ScottEncoding EvenOdd (t :: k -> Type))
evenOdd = letrec evenOddRecursion
  where
    evenOddRecursion :: EvenOdd (Term s) -> EvenOdd (Term s)
    evenOddRecursion EvenOdd {even, odd} =
      EvenOdd
        { even = plam $ \n -> pif (n #== 0) (pcon PTrue) (odd #$ n - 1)
        , odd = plam $ \n -> pif (n #== 0) (pcon PFalse) (even #$ n - 1)
        }

tests :: HasTester => TestTree
tests =
  testGroup
    "Records"
    [ testGroup
        "Simple"
        [ testCase "record construction" $
            printTerm (sampleRecord # field sampleInt)
              @?= "(program 1.0.0 ((\\i0 -> i1 False 6 \"Salut, Monde!\") (\\i0 -> \\i0 -> \\i0 -> i2)))"
        , testCase "record field" $
            equal' (sampleRecord # field sampleInt) "(program 1.0.0 6)"
        , testCase "record match" $
            equal' (pmatch' sampleRecord $ \(PRecord r) -> sampleString r) "(program 1.0.0 \"Salut, Monde!\")"
        ]
    , testGroup
        "Letrec"
        [ testCase "record" $ (printTerm $ sampleRecur # field sampleInt) @?= "(program 1.0.0 ((\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1))) (\\i0 -> \\i0 -> i1 True 12 \"Hello, World!\") (\\i0 -> \\i0 -> \\i0 -> i2)))"
        , testCase "record field" $ equal' (sampleRecur # field sampleInt) "(program 1.0.0 12)"
        , testCase "even" $ (printTerm $ evenOdd # field even) @?= "(program 1.0.0 ((\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1))) (\\i0 -> \\i0 -> i1 (\\i0 -> force (i4 (equalsInteger i1 0) (delay True) (delay (i3 (\\i0 -> \\i0 -> i1) (subtractInteger i1 1))))) (\\i0 -> force (i4 (equalsInteger i1 0) (delay False) (delay (i3 i5 (subtractInteger i1 1)))))) i2) (force ifThenElse)) (\\i0 -> \\i0 -> i2)))"
        , testCase "even 4" $ equal' (evenOdd # field even # (4 :: Term s PInteger)) "(program 1.0.0 True)"
        , testCase "even 5" $ equal' (evenOdd # field even # (5 :: Term s PInteger)) "(program 1.0.0 False)"
        ]
    ]
