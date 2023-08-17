{-# LANGUAGE OverloadedStrings #-}

-- | A cut-down versin of speed for core-dumping
module Main where

import Control.Monad
import Data.Foldable
import Perf
import Prelude
import MarkupParse
import Data.ByteString qualified as B

main :: IO ()
main = do
  let n = 1000
  bs <- B.readFile "other/line.svg"
  m <- execPerfT (measureDs MeasureTime n) $
    void $ ffap "markup" (length . markupTree . markup_ Xml) bs
  print $ (/1000.0) $ sum $ mconcat $ mconcat $ toList m
