{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Control.Monad
import Data.List (intercalate)
import Options.Applicative
import Perf
import Prelude

import MarkupParse hiding (header)
import MarkupParse.Html
import MarkupParse.Xml
import MarkupParse.Common
import Data.ByteString qualified as B
import Text.HTML.Parser qualified as HP
import Text.HTML.Tree qualified as HP
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

data RunType = RunDefault deriving (Eq, Show)

data Options = Options
  { optionN :: Int,
    optionLength :: Int,
    optionStatDType :: StatDType,
    optionRunType :: RunType,
    optionMeasureType :: MeasureType,
    optionExample :: Example,
    optionGolden :: Golden,
    optionReportConfig :: ReportConfig,
    optionRawStats :: Bool
  }
  deriving (Eq, Show)

parseRun :: Parser RunType
parseRun =
  flag' RunDefault (long "default" <> help "run default performance test")
    <|> pure RunDefault

options :: Parser Options
options =
  Options
    <$> option auto (value 1000 <> long "runs" <> short 'n' <> help "number of runs to perform")
    <*> option auto (value 1000 <> long "length" <> short 'l' <> help "length of list")
    <*> parseStatD
    <*> parseRun
    <*> parseMeasure
    <*> parseExample
    <*> parseGolden "golden"
    <*> parseReportConfig defaultReportConfig
    <*> switch (long "raw" <> short 'w' <> help "write raw statistics to file")

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionN o
  let !l = optionLength o
  let s = optionStatDType o
  let a = optionExample o
  let r = optionRunType o
  let mt = optionMeasureType o
  let gold = goldenFromOptions [show r, show n, show l, show mt] (optionGolden o)
  let w = optionRawStats o
  let raw =
        "other/"
          <> intercalate "-" [show r, show n, show l, show mt]
          <> ".map"
  let cfg = optionReportConfig o

  case r of
    RunDefault -> do
      bs <- B.readFile "/Users/tonyday/haskell/markup-parse/other/line.svg"
      t <- Text.readFile "/Users/tonyday/haskell/markup-parse/other/line.svg"
      m <- execPerfT (measureDs mt n) $ void $ do
        ffap "parseTokens" (tokensToTree . parseTokens) bs
        ffap "xmlMarkup" (runParserEither markupP) bs
        ffap "html-parse" (either (const []) id . HP.tokensToForest . HP.parseTokens) t
      when w (writeFile raw (show m))
      report cfg gold (measureLabels mt) (statify s m)
