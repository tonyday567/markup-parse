{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Control.Monad
import Data.List (intercalate)
import Options.Applicative
import Perf
import Prelude
import MarkupParse
import Data.ByteString qualified as B
import Text.HTML.Parser qualified as HP
import Text.HTML.Tree qualified as HP
import Data.Text.IO qualified as Text

data RunType = RunDefault deriving (Eq, Show)

data Options = Options
  { optionN :: Int,
    optionStatDType :: StatDType,
    optionRunType :: RunType,
    optionMeasureType :: MeasureType,
    optionGolden :: Golden,
    optionReportConfig :: ReportConfig,
    optionRawStats :: Bool,
    optionFile :: FilePath
  }
  deriving (Eq, Show)

parseRun :: Parser RunType
parseRun =
  flag' RunDefault (long "default" <> help "run default performance test")
    <|> pure RunDefault

options :: Parser Options
options =
  Options
    <$> option auto (value 1 <> long "runs" <> short 'n' <> help "number of runs to perform")
    <*> parseStatD
    <*> parseRun
    <*> parseMeasure
    <*> parseGolden "golden"
    <*> parseReportConfig defaultReportConfig
    <*> switch (long "raw" <> short 'w' <> help "write raw statistics to file")
    <*> strOption (value "other/line.svg" <> long "file" <> short 'f' <> help "file to test")

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionN o
  let s = optionStatDType o
  let r = optionRunType o
  let mt = optionMeasureType o
  let gold = goldenFromOptions [show r, show n, show mt] (optionGolden o)
  let f = optionFile o
  let w = optionRawStats o
  let raw =
        "other/"
          <> intercalate "-" [show r, show n, show mt]
          <> ".map"
  let cfg = optionReportConfig o

  case r of
    RunDefault -> do
      bs <- B.readFile f
      t <- Text.readFile f
      m <- execPerfT (measureDs mt n) $ void $ do
        ts' <- ffap "html-parse tokens" HP.parseTokens t
        _ <- ffap "html-parse tree" (either undefined id . HP.tokensToForest) ts'
        tsHtml <- resultError <$>
          ffap "tokenize Html" (tokenize Html) bs
        _ <- resultError <$>
          ffap "gather Html" gather tsHtml
        m <- resultError <$>
          ffap "markup Html" (markup Html) bs
        _ <- ffap "normalize" normalize m
        _ <- ffap "markdown" (markdown Compact) m
        _ <- resultError <$>
          ffap "markup Xml" (markup Xml) bs
        pure ()
      when w (writeFile raw (show m))
      report cfg gold (measureLabels mt) (statify s m)
