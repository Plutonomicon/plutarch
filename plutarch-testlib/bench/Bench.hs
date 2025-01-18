module Main (main) where

import Plutarch.Internal.Term (
  Config (NoTracing, Tracing),
  LogLevel (LogInfo),
  TracingMode (DoTracing),
 )
import Plutarch.LedgerApi.Utils (PMaybeData, pmapMaybeData, pmaybeDataToMaybe, pmaybeToMaybeData)
import Plutarch.Maybe (pmapMaybe)
import Plutarch.Prelude
import Plutarch.Test.Bench (
  BenchConfig (NonOptimizing, Optimizing),
  bcompare,
  bench,
  benchWithConfig,
  defaultMain,
 )
import Test.Tasty (TestTree, testGroup)

import Plutarch.Test.Suite.Plutarch.Unroll (unrollBenches)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Benchmarks"
      [ testGroup "Maybe" maybeBenches
      , testGroup "Exponentiation" expBenches
      , testGroup "Tracing" tracingBenches
      , testGroup "Unroll" unrollBenches
      ]

-- Suites

tracingBenches :: [TestTree]
tracingBenches =
  [ benchWithConfig "with tracing" traceConfig t
  , bcompare "$(NF-1) == \"Tracing\" && $NF == \"with tracing\"" $
      benchWithConfig "no tracing" noTraceConfig t
  ]
  where
    traceConfig :: BenchConfig
    traceConfig = NonOptimizing $ Tracing LogInfo DoTracing
    noTraceConfig :: BenchConfig
    noTraceConfig = NonOptimizing NoTracing
    t :: forall (s :: S). Term s PBool
    t = ptraceInfo "I am a very long string which hopefully uses a lot of space" (pconstant True)

expBenches :: [TestTree]
expBenches =
  [ bench "linear" (linearExp # 3 # 31)
  , bcompare "$(NF-1) == \"Exponentiation\" && $NF == \"linear\"" $ bench "by squaring" (bySquaringExp # 3 # 31)
  ]

maybeBenches :: [TestTree]
maybeBenches =
  [ testGroup
      "pmaybeToMaybeData . pmaybeDataToMaybe"
      [ bench
          "non-optimized"
          (plam (\m -> pmaybeToMaybeData #$ pmaybeDataToMaybe # m) # pconstant @(PMaybeData PInteger) (Just 42))
      , bcompare
          "$(NF-1) == \"pmaybeToMaybeData . pmaybeDataToMaybe\" && $NF == \"non-optimized\""
          $ benchWithConfig
            "optimized"
            Optimizing
            (plam (\m -> pmaybeToMaybeData #$ pmaybeDataToMaybe # m) # pconstant @(PMaybeData PInteger) (Just 42))
      ]
  , testGroup
      "fmap even"
      [ bench
          "PMaybeData"
          (pmapMaybeData # plam (\v -> pdata (peven # pfromData v)) # pconstant @(PMaybeData PInteger) (Just 42))
      , bcompare "$(NF-1) == \"fmap even\" && $NF == \"PMaybeData\"" $
          bench "PMaybe vs PMaybeData" (pmapMaybe # peven # pconstant @(PMaybe PInteger) (Just 42))
      ]
  , -- We run both cheap and expensive calculation in 'pmap*' to mitigate impact of PAsData encoding/decoding
    let
      n :: Integer = 10
     in
      testGroup
        "fmap fib"
        [ bench
            "PMaybeData"
            (pmapMaybeData # plam (\v -> pdata (pfib # pfromData v)) # pconstant @(PMaybeData PInteger) (Just n))
        , bcompare "$(NF-1) == \"fmap fib\" && $NF == \"PMaybeData\"" $
            bench "PMaybe vs PMaybeData" (pmapMaybe # pfib # pconstant @(PMaybe PInteger) (Just n))
        ]
  ]

-- Helpers

peven :: Term s (PInteger :--> PBool)
peven = plam $ \n -> pmod # n # 2 #== 0

pfib :: Term s (PInteger :--> PInteger)
pfib = pfix #$ plam $ \self n -> pif (n #<= 1) (pconstant 1) ((self # (n - 1)) * (self # (n - 2)))

linearExp :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
linearExp = phoistAcyclic $ plam $ \b e ->
  inner # b # b # e
  where
    inner :: forall (s' :: S). Term s' (PInteger :--> PInteger :--> PInteger :--> PInteger)
    inner = phoistAcyclic $ pfix #$ plam $ \self b acc e ->
      pif
        (e #== pone)
        acc
        (self # b # (acc #* b) # (e #- pone))

bySquaringExp :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
bySquaringExp = phoistAcyclic $ pfix #$ plam $ \self b e ->
  pif
    (e #== pone)
    b
    ( plet (self # b #$ pquot # e # 2) $ \below ->
        plet (below #* below) $ \res ->
          pif
            ((prem # e # 2) #== pone)
            (b #* res)
            res
    )
