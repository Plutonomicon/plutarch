module Plutarch.Test.Suite.Plutarch.Unroll (tests, unrollBenches) where

import Plutarch.Prelude

import Plutarch.Test.Bench (bench)
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
import Plutarch.Test.Unit (testEvalEqual, testEvalFail)
import Test.Tasty (TestTree, testGroup)

regularLength :: forall list a s. PIsListLike list a => Term s (list a :--> PInteger)
regularLength = pfix # plam go #$ 0
  where
    go :: Term s (PInteger :--> list a :--> PInteger) -> Term s (PInteger :--> list a :--> PInteger)
    go self = plam $ \n -> pelimList (\_ xs -> self # (n + 1) # xs) n

unrollLengthBoundNoPreCompute :: forall list a s. PIsListLike list a => Term s (list a :--> PInteger)
unrollLengthBoundNoPreCompute = punrollBound 20 (const $ plam $ \_ _ -> -1) go () # 0
  where
    go ::
      (() -> Term s (PInteger :--> list a :--> PInteger)) ->
      () ->
      Term s (PInteger :--> list a :--> PInteger)
    go self () = plam $ \n -> pelimList (\_ xs -> self () # (n + 1) # xs) n

unrollLengthBound :: forall list a s. PIsListLike list a => Term s (list a :--> PInteger)
unrollLengthBound = punrollBound 20 (const $ plam $ const -1) go 0
  where
    go ::
      (Integer -> Term s (list a :--> PInteger)) ->
      Integer ->
      Term s (list a :--> PInteger)
    go self n = plam $ pelimList (\_ xs -> self (n + 1) # xs) (pconstant n)

unrollLengthBoundFailing :: forall list a s. PIsListLike list a => Term s (list a :--> PInteger)
unrollLengthBoundFailing = punrollBound 20 (const perror) go 0
  where
    go ::
      (Integer -> Term s (list a :--> PInteger)) ->
      Integer ->
      Term s (list a :--> PInteger)
    go self n = plam $ pelimList (\_ xs -> self (n + 1) # xs) (pconstant n)

unrollLengthUnbound :: forall list a s. PIsListLike list a => Term s (list a :--> PInteger)
unrollLengthUnbound = punrollUnbound 20 go # 0
  where
    go :: Term s (PInteger :--> list a :--> PInteger) -> Term s (PInteger :--> list a :--> PInteger)
    go self = plam $ \n -> pelimList (\_ xs -> self # (n + 1) # xs) n

unrollLengthUnboundWhole :: forall list a s. PIsListLike list a => Term s (list a :--> PInteger)
unrollLengthUnboundWhole = punrollUnboundWhole 20 go #$ 0 -- pfix # plam go #$ 0
  where
    go :: Term s (PInteger :--> list a :--> PInteger) -> Term s (PInteger :--> list a :--> PInteger)
    go self = plam $ \n -> pelimList (\_ xs -> self # (n + 1) # xs) n

shortList :: Term s (PBuiltinList PInteger)
shortList = pconstant @(PBuiltinList PInteger) [1 .. 5]

longList :: Term s (PBuiltinList PInteger)
longList = pconstant @(PBuiltinList PInteger) [1 .. 40]

unrollBenches :: [TestTree]
unrollBenches =
  [ testGroup
      "Unrolled 20 times, running 5 recursions"
      [ bench "length, punrollUnbound" (unrollLengthUnbound # shortList)
      , bench "length, punrollUnboundWhole" (unrollLengthUnboundWhole # shortList)
      , bench "length, punrollBound" (unrollLengthBound # shortList)
      , bench "length, no unroll" (regularLength # shortList)
      ]
  , testGroup
      "Unrolled 20 times, running 40 recursions"
      [ bench "length, punrollUnbound" (unrollLengthUnbound # longList)
      , bench "length, punrollUnboundWhole" (unrollLengthUnboundWhole # longList)
      , bench "length, punrollBound" (unrollLengthBound # longList)
      , bench "length, no unroll" (regularLength # longList)
      ]
  , testGroup
      "punrollBound pre-computation"
      [ bench "precompute constant values" (unrollLengthBound # longList)
      , bench "no precompute constant values" (unrollLengthBoundNoPreCompute # longList)
      ]
  ]

tests :: TestTree
tests =
  testGroup
    "Unroll"
    [ plutarchGolden
        "Goldens"
        "unroll"
        [ goldenGroup
            "applied short list"
            [ goldenEval "punrollUnbound" (unrollLengthUnbound # shortList)
            , goldenEval "punrollUnboundWhole" (unrollLengthUnboundWhole # shortList)
            , goldenEval "punrollBound" (unrollLengthBound # shortList)
            , goldenEval "punrollBoundFailing" (unrollLengthBoundFailing # shortList)
            , goldenEval "no unroll" (regularLength # shortList)
            ]
        , goldenGroup
            "applied long list"
            [ goldenEval "punrollUnbound" (unrollLengthUnbound # longList)
            , goldenEval "punrollUnboundWhole" (unrollLengthUnboundWhole # longList)
            , goldenEval "punrollBound" (unrollLengthBound # longList)
            , goldenEval "no unroll" (regularLength # longList)
            ]
        , goldenGroup
            "unapplied"
            [ goldenEval "punrollUnbound" (unrollLengthUnbound @PBuiltinList @PInteger)
            , goldenEval "punrollUnboundWhole" (unrollLengthUnboundWhole @PBuiltinList @PInteger)
            , goldenEval "punrollBound" (unrollLengthBound @PBuiltinList @PInteger)
            , goldenEval "punrollBoundFailing" (unrollLengthBoundFailing @PBuiltinList @PInteger)
            , goldenEval "no unroll" (regularLength @PBuiltinList @PInteger)
            ]
        ]
    , testGroup
        "unrolled length short"
        [ testEvalEqual "punrollUnbound" (unrollLengthUnbound # shortList) (plength # shortList)
        , testEvalEqual "punrollUnboundWhole" (unrollLengthUnboundWhole # shortList) (plength # shortList)
        , testEvalEqual "punrollBound" (unrollLengthBound # shortList) (plength # shortList)
        , testEvalEqual "punrollBoundFailing" (unrollLengthBoundFailing # shortList) (plength # shortList)
        , testEvalEqual "no unroll" (regularLength # shortList) (plength # shortList)
        ]
    , testGroup
        "unrolled length long"
        [ testEvalEqual "punrollUnbound" (unrollLengthUnbound # longList) (plength # longList)
        , testEvalEqual "punrollUnboundWhole" (unrollLengthUnboundWhole # longList) (plength # longList)
        , testEvalEqual "punrollBound" (unrollLengthBound # longList) (-1)
        , testEvalFail "punrollBoundFailing" (unrollLengthBoundFailing # longList)
        , testEvalEqual "no unroll" (regularLength # longList) (plength # longList)
        ]
    ]
