{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | basic measurement and callibration
module Main where

import Data.ByteString qualified as B
import Data.Text.IO qualified as Text
import FlatParse.Basic (byteStringOf, char, satisfy, skipMany)
import FlatParse.Basic qualified as FP
import MarkupParse
import MarkupParse.FlatParse
import Options.Applicative
import Perf
import Text.HTML.Parser qualified as HP
import Text.HTML.Tree qualified as HP
import Prelude

data RunType = RunDefault | RunReduced | RunMarkup | RunWhitespace | RunWrappedQ | RunIsa | RunByteStringOf deriving (Eq, Show)

data SpeedOptions = SpeedOptions
  { speedReportOptions :: ReportOptions,
    speedRunType :: RunType,
    speedFile :: FilePath,
    speedSnippet :: B.ByteString
  }
  deriving (Eq, Show)

parseRun :: Parser RunType
parseRun =
  flag' RunDefault (long "default" <> help "run default performance test")
    <|> flag' RunMarkup (long "markup" <> short 'm' <> help "run markup performance test")
    <|> flag' RunWhitespace (long "whitespace" <> help "run whitespace parsing test")
    <|> flag' RunWrappedQ (long "wrappedQ" <> help "run wrappedQ parsing test")
    <|> flag' RunReduced (long "reduced" <> help "run with reduced result sizes")
    <|> flag' RunByteStringOf (long "bytestringof" <> help "test bytestringof")
    <|> flag' RunIsa (long "isa" <> help "test isa")
    <|> pure RunDefault

speedOptions :: Parser SpeedOptions
speedOptions =
  SpeedOptions
    <$> parseReportOptions
    <*> parseRun
    <*> strOption (value "other/line.svg" <> long "file" <> short 'f' <> help "file to test")
    <*> strOption (value "'wrapped'" <> long "snippet" <> help "snippet to parse")

speedInfo :: ParserInfo SpeedOptions
speedInfo =
  info
    (speedOptions <**> helper)
    (fullDesc <> progDesc "markup-parse benchmarking" <> header "speed tests")

main :: IO ()
main = do
  o <- execParser speedInfo
  let rep = speedReportOptions o
  let r = speedRunType o
  let f = speedFile o
  let snip = speedSnippet o

  case r of
    RunDefault -> do
      bs <- B.readFile f
      t <- Text.readFile f

      reportMainWith rep (show r) $ do
        ts' <- ffap "html-parse tokens" HP.parseTokens t
        _ <- ffap "html-parse tree" (either undefined id . HP.tokensToForest) ts'
        tsHtml <-
          resultError
            <$> ffap "tokenize" (tokenize Xml) bs
        _ <-
          resultError
            <$> ffap "gather" (gather Xml) tsHtml
        m <-
          resultError
            <$> ffap "markup" (markup Xml) bs
        _ <- ffap "normalize" normalize m
        _ <- ffap "markdown" (markdown Compact) m
        pure ()
    RunMarkup -> do
      bs <- B.readFile f
      reportMainWith rep (show r) $ do
        fap "markup" (length . markupTree . markup_ Xml) bs
    RunWhitespace -> do
      reportMainWith rep (show r) (wsFap " \n\nx")
    RunWrappedQ -> do
      reportMainWith rep (show r) (fapWrappedQ snip)
    RunIsa -> do
      reportMainWith rep (show r) fapIsa
    RunByteStringOf -> do
      reportMainWith rep (show r) fapBSOf
    RunReduced -> do
      bs <- B.readFile f
      t <- Text.readFile f
      let ts' = HP.parseTokens t
      let ts = tokenize_ Xml bs
      let m = markup_ Xml bs
      reportMainWith rep (show r) $ do
        _ <- ffap "html-parse tokens" (length . HP.parseTokens) t
        _ <- ffap "html-parse tree" (either undefined length . HP.tokensToForest) ts'
        _ <- ffap "tokenize" (length . tokenize Xml) bs
        _ <- ffap "gather" (length . gather_ Xml) ts
        _ <- ffap "markup" (length . markupTree . markup_ Xml) bs
        _ <- ffap "normalize" (normalize) m
        _ <- ffap "markdown" (markdown Compact) m
        pure ()

-- | Consume whitespace.
wsSwitch_ :: FP.Parser e ()
wsSwitch_ =
  $( FP.switch
       [|
         case _ of
           " " -> wsSwitch_
           "\n" -> wsSwitch_
           "\t" -> wsSwitch_
           "\r" -> wsSwitch_
           "\f" -> wsSwitch_
           _ -> pure ()
         |]
   )

-- | consume whitespace
wsSatisfy_ :: FP.Parser e ()
wsSatisfy_ = FP.skipMany (FP.satisfy isWhitespace)

-- | consume whitespace
wsFusedSatisfy_ :: FP.Parser e ()
wsFusedSatisfy_ = FP.skipMany (FP.fusedSatisfy isWhitespace (const False) (const False) (const False)) >> pure ()

-- | consume whitespace
wsSatisfyAscii_ :: FP.Parser e ()
wsSatisfyAscii_ = FP.skipMany (FP.satisfyAscii isWhitespace) >> pure ()

wsFap :: B.ByteString -> PerfT IO [[Double]] ()
wsFap bs = do
  fap "wsFusedSatisfy_" (FP.runParser wsFusedSatisfy_) bs
  fap "ws" (FP.runParser ws) bs
  fap "wss" (FP.runParser wss) bs
  fap "ws_" (FP.runParser ws_) bs
  fap "wsSwitch_" (FP.runParser wsSwitch_) bs
  fap "wsSatisfy_" (FP.runParser wsSatisfy_) bs
  fap "wsSatisfyAscii_" (FP.runParser wsSatisfyAscii_) bs
  pure ()

fapWrappedQ :: B.ByteString -> PerfT IO [[Double]] ()
fapWrappedQ bs = do
  fap "wrappedQ" (FP.runParser wrappedQ) bs
  fap "wrappedQSatisfy" (FP.runParser wrappedQSatisfy) bs
  fap "wrappedQSkipSatisfy" (FP.runParser wrappedQSkipSatisfy) bs
  fap "wrappedQNotA" (FP.runParser wrappedQNotA) bs
  fap "wrappedQCandidate" (FP.runParser wrappedQCandidate) bs
  pure ()

fapIsa :: PerfT IO [[Double]] ()
fapIsa = do
  fap "isa isAttrName" (FP.runParser (isa isAttrName)) "name"
  fap "attrName" (FP.runParser attrName) "name"
  pure ()

fapBSOf :: PerfT IO [[Double]] ()
fapBSOf = do
  fap "byteStringOf" (FP.runParser (byteStringOf (attrs Html))) " a=\"a\" b=b c>"
  fap "byteStringOf'" (FP.runParser (byteStringOf (attrs Html))) " a=\"a\" b=b c>"
  pure ()

isAttrName :: Char -> Bool
isAttrName x =
  not $
    (isWhitespace x)
      || (x == '/')
      || (x == '>')
      || (x == '=')

attrName :: FP.Parser e B.ByteString
attrName = byteStringOf $ some (satisfy isAttrName)

wrappedQSatisfy :: FP.Parser e B.ByteString
wrappedQSatisfy =
  ($(char '"') *> (byteStringOf $ many (satisfy (/= '"'))) <* $(char '"'))
    <|> ($(char '\'') *> (byteStringOf $ many (satisfy (/= '\''))) <* $(char '\''))

wrappedQSkipSatisfy :: FP.Parser e B.ByteString
wrappedQSkipSatisfy =
  ($(char '"') *> (byteStringOf $ skipMany (satisfy (/= '"'))) <* $(char '"'))
    <|> ($(char '\'') *> (byteStringOf $ skipMany (satisfy (/= '\''))) <* $(char '\''))

wrappedQNotA :: FP.Parser e B.ByteString
wrappedQNotA =
  ($(char '"') *> (nota '"') <* $(char '"'))
    <|> ($(char '\'') *> (nota '\'') <* $(char '\''))

wrappedQCandidate :: FP.Parser e B.ByteString
wrappedQCandidate =
  wrappedSQ' <|> wrappedDQ'

wrappedSQ' :: FP.Parser b B.ByteString
wrappedSQ' = $(char '\'') *> (nota '\'') <* $(char '\'')

wrappedDQ' :: FP.Parser b B.ByteString
wrappedDQ' = $(char '"') *> (nota '"') <* $(char '"')
