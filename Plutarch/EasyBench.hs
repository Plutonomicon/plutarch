module Plutarch.EasyBench (easyBench) where

import Plutarch.Prelude

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as SBS
import Data.Kind (Type)
import Data.SatInt (fromSatInt)
import Data.Text (Text)
import Data.Text qualified as Txt
import Data.Text.Encoding qualified as Txt
import Prettyprinter (Doc, pretty, (<+>))
import Text.Printf (printf)

import GHC.Generics (Generic)
import Plutarch.Evaluate (evalTerm)
import Plutarch.Internal.Term (Config, compile)
import Plutarch.Script (Script (unScript))

import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (ExBudget))
import PlutusLedgerApi.Common (ExCPU (ExCPU), ExMemory (ExMemory), serialiseUPLC)

data PEasyBenchIteration = PEasyBenchIteration
  { eb'cpu :: Int
  , eb'memory :: Int
  , eb'size :: Int
  , eb'hex :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

reportIteration :: PEasyBenchIteration -> Maybe PEasyBenchIteration -> Doc ()
reportIteration (PEasyBenchIteration cpu mem size _hex) prev =
  case prev of
    Nothing -> "CPU:" <+> pretty cpu <+> "|" <+> "MEM:" <+> pretty mem <+> "|" <+> "SIZE:" <+> pretty size
    Just (PEasyBenchIteration cpuPrev memPrev sizePrev _) ->
      let
        prettyRate :: Double -> Doc ()
        prettyRate d
          | d == 0 = ""
          | d < 0 = (pretty @String $ printf "%.2f" d) <> "%"
          | otherwise = "+" <> (pretty @String $ printf "%.2f" d) <> "%"

        prettyDiff' :: Int -> Doc ()
        prettyDiff' d
          | d > 0 = "+" <> pretty d
          | otherwise = pretty d

        prettyDiff :: Int -> Int -> Doc ()
        prettyDiff new old =
          let
            change = new - old
            percentChange = fromIntegral change / fromIntegral old
           in
            if new == old
              then ""
              else "(" <> prettyDiff' change <> "," <+> prettyRate percentChange <> ")"
       in
        "CPU:"
          <+> pretty cpu
          <+> prettyDiff cpu cpuPrev
          <+> "MEM:"
          <+> pretty mem
          <+> prettyDiff mem memPrev
          <+> "SIZE:"
          <+> pretty size
          <+> prettyDiff size sizePrev

easyBench :: forall (a :: S -> Type). String -> Config -> (forall s. Term s a) -> IO ()
easyBench identifier' cfg x = do
  let
    identifier = "easybench:" <> identifier'
    (_, ExBudget (ExCPU (fromSatInt -> cpu)) (ExMemory (fromSatInt -> mem)), _) = either (error . Txt.unpack) id $ evalTerm @a cfg x
    rawScript =
      serialiseUPLC $
        unScript $
          either (error . Txt.unpack) id $
            compile cfg x
    -- Typically, raw scripts are also base16 encoded, but
    -- consistent encoding isn't really important
    rawScriptEncoded =
      Txt.decodeUtf8 $ Base16.encode $ SBS.fromShort rawScript

    scriptSize = SBS.length rawScript

    newItr = PEasyBenchIteration cpu mem scriptSize rawScriptEncoded

  itrs :: [PEasyBenchIteration] <-
    ( Aeson.eitherDecodeFileStrict @[PEasyBenchIteration] identifier >>= \case
        Left _err -> do
          putStrLn "Existing bench iteration file cannot be parsed. It will be wipped"
          pure mempty
        Right x -> pure x
      )
      `catch` (const @(IO [PEasyBenchIteration]) @IOException $ pure mempty)

  case itrs of
    [] -> do
      Aeson.encodeFile @[PEasyBenchIteration] identifier [newItr]
      print $ reportIteration newItr Nothing
    (x : _) -> do
      when (x /= newItr) $ Aeson.encodeFile @[PEasyBenchIteration] identifier (newItr : itrs)
      print $ reportIteration newItr (Just x)
