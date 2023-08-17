-- | A patch function for <https://hackage.haskell.org/package/tree-diff tree-diff>.
module MarkupParse.Patch
  ( patch,
    goldenPatch,
  )
where

import Data.Foldable
import Data.Maybe
import Data.TreeDiff
import Data.TreeDiff.OMap qualified as O
import GHC.Exts
import Test.Tasty (TestTree)
import Test.Tasty.Golden.Advanced (goldenTest)
import Prelude

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.TreeDiff
-- >>> import MarkupParse.Patch

-- | compare a markup file with a round-trip transformation.
goldenPatch :: (ToExpr a) => (FilePath -> IO a) -> (a -> a) -> FilePath -> TestTree
goldenPatch f testf fp =
  goldenTest
    fp
    (f fp)
    (testf <$> f fp)
    (\expected actual -> pure (show . ansiWlEditExpr <$> patch expected actual))
    (\_ -> pure ())

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

-- | 'ediff' with unchanged sections filtered out
--
-- >>> show $ ansiWlEditExpr <$> patch [1, 2, 3, 5] [0, 1, 2, 4, 6]
-- "Just [+0, -3, +4, -5, +6]"
patch :: (ToExpr a) => a -> a -> Maybe (Edit EditExpr)
patch m m' = filterChangedEdit $ ediff m m'
