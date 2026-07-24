{-# LANGUAGE GHC2024 #-}

-- | Warning helpers for markup parsing.
module Data.Markup.Warn
  ( MarkupWarning (..),
    ParserWarning (..),
    Warn,
    warnError,
    warnEither,
    warnMaybe,
    showWarnings,
    concatWarns,
  )
where

import Control.Category ((>>>))
import Control.DeepSeq
import Data.Bifunctor
import Data.Bool
import Data.Data
import Data.List qualified as List
import Data.Markup (NameTag)
import Data.These
import GHC.Generics

-- | Warnings originating in the stream/token parser.
data ParserWarning
  = ParserLeftover String
  | ParserError String
  | ParserUncaught
  deriving (Eq, Ord, Show, Generic, Data)

instance NFData ParserWarning

-- | markup-parse generally tries to continue on parse errors, and return what has/can still be parsed, together with any warnings.
data MarkupWarning
  = -- | A tag ending with "/>" that is not an element of 'selfClosers' (Html only).
    BadEmptyElemTag
  | -- | A tag ending with "/>" that has children. Cannot happen in the parsing phase.
    SelfCloserWithChildren
  | -- | Only a 'StartTag' can have child tokens.
    LeafWithChildren
  | -- | A CloseTag with a different name to the currently open StartTag.
    TagMismatch NameTag NameTag
  | -- | An EndTag with no corresponding StartTag.
    UnmatchedEndTag
  | -- | An StartTag with no corresponding EndTag.
    UnclosedTag
  | -- | An EndTag should never appear in 'Markup'
    EndTagInTree
  | -- | Empty Content, Comment, Decl or Doctype
    EmptyContent
  | -- | Badly formed declaration
    BadDecl
  | MarkupParser ParserWarning
  deriving (Eq, Ord, Show, Generic, Data)

instance NFData MarkupWarning

-- | A type synonym for the common returning type of many functions. A common computation pipeline is to take advantage of the 'These' Monad instance eg
--
-- > markup s bs = bs & (tokenize s >=> gather s) & second (Markup s)
type Warn a = These [MarkupWarning] a

-- | Convert any warnings to an 'error'
warnError :: Warn a -> a
warnError = these (showWarnings >>> error) id (\xs a -> bool (error (showWarnings xs)) a (null xs))

-- | Returns Left on any warnings
warnEither :: Warn a -> Either [MarkupWarning] a
warnEither = these Left Right (\xs a -> bool (Left xs) (Right a) (null xs))

-- | Returns results, if any, ignoring warnings.
warnMaybe :: Warn a -> Maybe a
warnMaybe = these (const Nothing) Just (\_ a -> Just a)

showWarnings :: [MarkupWarning] -> String
showWarnings = List.nub >>> fmap show >>> unlines

concatWarns :: [Warn [a]] -> Warn [a]
concatWarns rs = case bimap mconcat mconcat $ partitionHereThere rs of
  ([], xs) -> That xs
  (es, []) -> This es
  (es, xs) -> These es xs
