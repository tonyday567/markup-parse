{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import MarkupParse
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Maybe
import Data.TreeDiff
import Data.TreeDiff.OMap qualified as O
import GHC.Exts
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Prelude
import Data.String.Interpolate
import Control.Monad
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
  pure $ either error id $ markup Html bs

-- round trip markdown >>> markup
isoMarkdownMarkup :: Markup -> Markup
isoMarkdownMarkup m = m & (markdown >=> markup Html) & either error id

isUnchangedList :: [Edit EditExpr] -> Bool
isUnchangedList xs = all isCpy xs && all isUnchangedExpr (mapMaybe cpy xs)

isCpy :: Edit a -> Bool
isCpy (Cpy _) = True
isCpy _ = False

cpy :: Edit a -> Maybe a
cpy (Cpy a) = Just a
cpy _ = Nothing

isUnchangedEdit :: Edit EditExpr -> Bool
isUnchangedEdit (Cpy e) = isUnchangedExpr e
isUnchangedEdit _ = False

isUnchangedExpr :: EditExpr -> Bool
isUnchangedExpr e = isUnchangedList $ getList e

getList :: EditExpr -> [Edit EditExpr]
getList (EditApp _ xs) = xs
getList (EditRec _ m) = snd <$> O.toList m
getList (EditLst xs) = xs
getList (EditExp _) = []

filterChangedExprs :: EditExpr -> Maybe EditExpr
filterChangedExprs (EditApp n xs) =
  case filter (not . isUnchangedEdit) (filterChangedEdits xs) of
    [] -> Nothing
    xs' -> Just $ EditApp n xs'
filterChangedExprs (EditRec n m) =
  case filterChangedEditMap (O.fromList $ filter (not . isUnchangedEdit . snd) (O.toList m)) of
    Nothing -> Nothing
    Just m' -> Just (EditRec n m')
filterChangedExprs (EditLst xs) =
  case filter (not . isUnchangedEdit) (filterChangedEdits xs) of
    [] -> Nothing
    xs' -> Just (EditLst xs')
filterChangedExprs (EditExp _) = Nothing

filterChangedEdit :: Edit EditExpr -> Maybe (Edit EditExpr)
filterChangedEdit (Cpy a) = Cpy <$> filterChangedExprs a
filterChangedEdit x = Just x

filterChangedEdit' :: (f, Edit EditExpr) -> Maybe (f, Edit EditExpr)
filterChangedEdit' (f, e) = (f,) <$> filterChangedEdit e

filterChangedEdits :: [Edit EditExpr] -> [Edit EditExpr]
filterChangedEdits xs = mapMaybe filterChangedEdit xs

filterChangedEditMap :: O.OMap FieldName (Edit EditExpr) -> Maybe (O.OMap FieldName (Edit EditExpr))
filterChangedEditMap m = case xs' of
  [] -> Nothing
  xs'' -> Just $ O.fromList xs''
  where
    xs = O.toList m
    xs' = mapMaybe filterChangedEdit' xs

patch :: (ToExpr a) => a -> a -> Maybe (Edit EditExpr)
patch m m' = filterChangedEdit $ ediff m m'

-- patch testing
printPatchExamples :: IO ()
printPatchExamples = traverse_ (printPatchExample m0) patchExamples

printPatchExample :: ByteString -> (String, ByteString) -> IO ()
printPatchExample m (s, m') = do
  print s
  case show . ansiWlEditExpr <$> patch (either error id $ markup Html m) (either error id $ markup Html m') of
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
