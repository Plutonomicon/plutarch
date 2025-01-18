module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Test.Suite.Plutarch.Bool qualified as Bool
import Plutarch.Test.Suite.Plutarch.ByteString qualified as ByteString
import Plutarch.Test.Suite.Plutarch.Either qualified as Either
import Plutarch.Test.Suite.Plutarch.Field qualified as Field
import Plutarch.Test.Suite.Plutarch.Integer qualified as Integer
import Plutarch.Test.Suite.Plutarch.List qualified as List
import Plutarch.Test.Suite.Plutarch.Maybe qualified as Maybe
import Plutarch.Test.Suite.Plutarch.Monadic qualified as Monadic
import Plutarch.Test.Suite.Plutarch.PLam qualified as PLam
import Plutarch.Test.Suite.Plutarch.POrd qualified as POrd
import Plutarch.Test.Suite.Plutarch.Pair qualified as Pair
import Plutarch.Test.Suite.Plutarch.Positive qualified as Positive
import Plutarch.Test.Suite.Plutarch.Rational qualified as Rational
import Plutarch.Test.Suite.Plutarch.Recursion qualified as Recursion
import Plutarch.Test.Suite.Plutarch.Scripts qualified as Scripts
import Plutarch.Test.Suite.Plutarch.Semigroup qualified as Semigroup
import Plutarch.Test.Suite.Plutarch.Show qualified as Show
import Plutarch.Test.Suite.Plutarch.String qualified as String
import Plutarch.Test.Suite.Plutarch.Tracing qualified as Tracing
import Plutarch.Test.Suite.Plutarch.TryFrom qualified as TryFrom
import Plutarch.Test.Suite.Plutarch.Unit qualified as Unit
import Plutarch.Test.Suite.Plutarch.Unroll qualified as Unroll
import Plutarch.Test.Suite.Plutarch.Uplc qualified as Uplc
import Plutarch.Test.Suite.PlutarchLedgerApi.AssocMap qualified as AssocMap
import Plutarch.Test.Suite.PlutarchLedgerApi.Regressions qualified as Regressions
import Plutarch.Test.Suite.PlutarchLedgerApi.Utils qualified as Utils
import Plutarch.Test.Suite.PlutarchLedgerApi.V1 qualified as V1
import Plutarch.Test.Suite.PlutarchLedgerApi.V2 qualified as V2
import Plutarch.Test.Suite.PlutarchLedgerApi.V3 qualified as V3
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests)

main :: IO ()
main = do
  -- Pre-emptively avoid encoding issues
  setLocaleEncoding utf8
  defaultMain . adjustOption moreTests . testGroup "Tests" $
    [ testGroup
        "Plutarch"
        [ Bool.tests
        , ByteString.tests
        , Either.tests
        , Field.tests
        , Integer.tests
        , List.tests
        , Maybe.tests
        , Monadic.tests
        , PLam.tests
        , POrd.tests
        , Pair.tests
        , Positive.tests
        , Rational.tests
        , Recursion.tests
        , Scripts.tests
        , Show.tests
        , String.tests
        , Tracing.tests
        , TryFrom.tests
        , Unit.tests
        , Uplc.tests
        , Semigroup.tests
        , Unroll.tests
        ]
    , testGroup
        "PlutarchLedgerApi"
        [ testGroup
            "Laws"
            [ Utils.tests
            , AssocMap.tests
            , V1.tests
            , V2.tests
            , V3.tests
            ]
        , testGroup "Regressions" Regressions.tests
        ]
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 1_000
