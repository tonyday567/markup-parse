{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless, (>=>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Function
import MarkupParse
import System.Exit (exitFailure)
import Prelude

examples :: [(RenderStyle, Standard, FilePath)]
examples =
  [ (Compact, Xml, "other/line.svg"),
    (Compact, Html, "other/ex1.html")
  ]

main :: IO ()
main = do
  failures <-
    concatMapM
      ( \(r, s, fp) -> do
          expected <- B.readFile fp
          actual <- isoMarkupMarkdown r s <$> B.readFile fp
          if expected == actual
            then do
              putStrLn $ "PASS " ++ fp
              pure []
            else do
              putStrLn $ "FAIL " ++ fp
              pure [fp]
      )
      examples
  unless (null failures) $ do
    putStrLn $ "Markup/markdown roundtrip failures: " ++ show failures
    exitFailure

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

-- | Round trip markdown >>> markup.
isoMarkupMarkdown :: RenderStyle -> Standard -> ByteString -> ByteString
isoMarkupMarkdown r s m = m & (markup s >=> markdown r s) & warnError
