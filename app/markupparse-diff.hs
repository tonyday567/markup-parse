{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Bool
import Data.Bifunctor
import Data.Function
import Control.Category ((>>>))
import Data.Maybe
import Data.Algorithm.DiffOutput
import Data.Algorithm.Diff
import MarkupParse
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ goldenTests
      ]

goldenTests :: TestTree
goldenTests =
  testGroup
    "examples"
    ( testExample
        <$> [ (Compact, Xml, "other/line.svg"),
              (Compact, Html, "other/ex1.html")
            ]
    )

testExample :: (RenderStyle, Standard, FilePath) -> TestTree
testExample (r, s, fp) =
  goldenTest
    fp
    (B.readFile fp)
    (isoMarkupMarkdown r s <$> B.readFile fp)
    (\expected actual -> getDiff (C.lines expected) (C.lines actual) & fmap (bimap (C.unpack >>> pure) (C.unpack >>> pure)) & diffToLineRanges & prettyDiffs & (\xs -> bool (pure $ Just (show xs)) (pure Nothing) (xs==mempty)))
    (\_ -> pure ())

-- round trip markdown >>> markup
isoMarkupMarkdown :: RenderStyle -> Standard -> ByteString -> ByteString
isoMarkupMarkdown r s m = m & (markup s >=> markdown r s) & warnError

