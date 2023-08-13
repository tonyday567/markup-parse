{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import MarkupParse
import MarkupParse.Patch
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Maybe
import Data.TreeDiff
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Prelude
import Data.String.Interpolate
import Data.Function

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ goldenTests
      ]

goldenTests :: TestTree
goldenTests = testGroup "examples" (testExample <$> ["other/line.svg", "other/ex1.html", "other/Parsing - Wikipedia.html"])

testExample :: FilePath -> TestTree
testExample fp =
  goldenTest
    fp
    (getMarkupFile fp)
    (isoMarkdownMarkup <$> getMarkupFile fp)
    (\expected actual -> pure (show . ansiWlEditExpr <$> patch expected actual))
    (\_ -> pure ())

getMarkupFile :: FilePath -> IO Markup
getMarkupFile fp = do
  bs <- BS.readFile fp
  pure $ resultError $ markup Html bs

-- round trip markdown >>> markup
isoMarkdownMarkup :: Markup -> Markup
isoMarkdownMarkup m = m & markdown Compact & markup Html & resultError

-- patch testing
printPatchExamples :: IO ()
printPatchExamples = traverse_ (printPatchExample m0) patchExamples

printPatchExample :: ByteString -> (String, ByteString) -> IO ()
printPatchExample m (s, m') = do
  print s
  case show . ansiWlEditExpr <$> patch (resultError $ markup Html m) (resultError $ markup Html m') of
    Nothing -> putStrLn ("no changes" :: String)
    Just x -> putStrLn x

patchExamples :: [(String, ByteString)]
patchExamples =
  [ ("change an attribute name", m1'),
    ("change an attribute value", m1),
    ("delete an attribute", m2),
    ("insert an attribute", m3),
    ("change a tag", m4),
    ("change a markup leaf", m5),
    ("delete a leaf", m6),
    ("insert a leaf", m7),
    ("insert attribute", m8),
    ("modify content", m9),
    ("deep leaf insertion", m10)
  ]

m0 :: ByteString
m0 = [i|<top class="a" b="c"><leaf></leaf>text</top>|]

-- Changing class
m1 :: ByteString
m1 = [i|<top class="b" b="c"><leaf></leaf>text</top>|]

m1' :: ByteString
m1' = [i|<top classx="a" b="c"><leaf></leaf>text</top>|]

-- deleting an attribute
m2 :: ByteString
m2 = [i|<top class="a"><leaf></leaf>text</top>|]

-- inserting an attribute
m3 :: ByteString
m3 = [i|<top class="a" b="c" d="e"><leaf></leaf>text</top>|]

-- changing a tag
m4 :: ByteString
m4 = [i|<newtop class="a" b="c"><leaf></leaf>text</newtop>|]

-- changing a leaf
m5 :: ByteString
m5 = [i|<top class="a" b="c"><newleaf></newleaf>text</top>|]

-- deleting a leaf
m6 :: ByteString
m6 = [i|<top class="a" b="c">text</top>|]

-- inserting a leaf
m7 :: ByteString
m7 = [i|<top class="a" b="c"><newleaf></newleaf><leaf></leaf>text</top>|]

-- inserting Attributes
m8 :: ByteString
m8 = [i|<top class="a" b="c"><leaf class="a" b="c"></leaf>text</top>|]

-- modifying content
m9 :: ByteString
m9 = [i|<top class="a" b="c"><leaf></leaf>textual content</top>|]

-- inserting a leaf deeper down
m10 :: ByteString
m10 = [i|<top class="a" b="c"><leaf><newdeepleaf></newdeepleaf></leaf>text</top>|]
