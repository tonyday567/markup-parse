{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Various flatparse helpers and combinators.
module MarkupParse.FlatParse
  ( -- * parsing
    ParserWarning (..),
    runParserMaybe,
    runParserEither,
    runParserWarn,
    runParser_,
    runParser,

    -- * parsers
    isWhitespace,
    ws_,
    ws_',
    ws,
    wss,
    sq,
    dq,
    wrappedDq,
    wrappedSq,
    wrappedQ,
    wrappedQNoGuard,
    unwrappedV,
    eq,
    sep,
    bracketed,
    bracketedSB,
    wrapped,
    int,
    double,
    signed,
  )

where

import Data.ByteString (ByteString)
import FlatParse.Basic hiding (cut, take)
import Data.Bool
import Data.Char hiding (isDigit)
import GHC.Exts
import Prelude hiding (replicate)
import GHC.Generics (Generic)
import Data.These
import Control.DeepSeq

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import MarkupParse.FlatParse
-- >>> import FlatParse.Basic

-- | run a Parser, throwing away leftovers. Nothing on 'Fail' or 'Err'.
--
-- >>> runParserMaybe ws "x"
-- Nothing
--
-- >>> runParserMaybe ws " x"
-- Just ' '
runParserMaybe :: Parser e a -> ByteString -> Maybe a
runParserMaybe p b = case runParser p b of
  OK r _ -> Just r
  Fail -> Nothing
  Err _ -> Nothing

-- | run a Parser, throwing away leftovers. Returns Left on 'Fail' or 'Err'.
runParserEither :: Parser String a -> ByteString -> Either String a
runParserEither p bs = case runParser p bs of
  Err e -> Left e
  OK a _ -> Right a
  Fail -> Left "uncaught parse error"

data ParserWarning = ParserLeftover ByteString | ParserError String | ParserUncaught deriving (Eq, Show, Ord, Generic, NFData)

-- | Run parser, returning leftovers and errors as 'ParserWarning's.
runParserWarn :: Parser String a -> ByteString -> These ParserWarning a
runParserWarn p bs = case runParser p bs of
  Err e -> This (ParserError e)
  OK a "" -> That a
  OK a x -> These (ParserLeftover x) a
  Fail -> This ParserUncaught

-- | Run parser, discards leftovers & throws an error on failure.
runParser_ :: Parser String a -> ByteString -> a
runParser_ p bs = case runParser p bs of
  Err e -> error e
  OK a _ -> a
  Fail -> error "uncaught parse error"

-- | Consume whitespace.
--
-- >>> runParser ws_ " \nx"
-- OK () "x"
--
-- >>> runParser ws_ "x"
-- OK () "x"
ws_ :: Parser e ()
ws_ =
  $( switch
       [|
         case _ of
           " " -> ws_
           "\n" -> ws_
           "\t" -> ws_
           "\r" -> ws_
           "\f" -> ws_
           _ -> pure ()
         |]
   )

-- | consume whitespace
ws_' :: Parser e ()
ws_' = many (satisfy isWhitespace) >> pure ()

-- | Equivalent to @inClass "\x09\x0a\x0c "@
isWhitespace :: Char -> Bool
isWhitespace ' '    = True -- \x20 space
isWhitespace '\x0a' = True -- \n linefeed
isWhitespace '\x09' = True -- \t tab
isWhitespace '\x0c' = True -- \f formfeed
isWhitespace '\x0d' = True -- \r carriage return
isWhitespace _      = False

-- | single whitespace
--
-- >>> runParser ws " \nx"
-- OK ' ' "\nx"
ws :: Parser e Char
ws = satisfy isWhitespace

-- | multiple whitespace
--
-- >>> runParser wss " \nx"
-- OK " \n" "x"
--
-- >>> runParser wss "x"
-- Fail
wss :: Parser e ByteString
wss = byteStringOf $ some ws

-- | single quote
--
-- >>> runParserMaybe sq "''"
-- Just ()
sq :: ParserT st e ()
sq = $(char '\'')

-- | double quote
--
-- >>> runParserMaybe dq "\""
-- Just ()
dq :: ParserT st e ()
dq = $(char '"')

wrappedDq :: Parser e ByteString
wrappedDq = wrapped dq (byteStringOf $ many (satisfy (/= '"')))

-- | guard check for closing quote
wrappedSq :: Parser e ByteString
wrappedSq = wrapped sq (byteStringOf $ many (satisfy (/= '\'')))

-- | quote or double quote wrapped
--
-- >>> runParserMaybe wrappedQ "\"quoted\""
-- Just "quoted"
--
-- >>> runParserMaybe wrappedQ "'quoted'"
-- Just "quoted"
wrappedQ :: Parser e ByteString
wrappedQ =
  wrappedDq
    <|> wrappedSq

-- | quote or double quote wrapped
--
-- >>> runParserMaybe (wrappedQNoGuard (many $ satisfy (/= '"'))) "\"name\""
-- Just "name"
--
-- but will consume quotes if the underlying parser does.
--
-- >>> runParserMaybe (wrappedQNoGuard (many anyChar)) "\"name\""
-- Nothing
wrappedQNoGuard :: Parser e a -> Parser e a
wrappedQNoGuard p = wrapped dq p <|> wrapped sq p

unwrappedV :: Parser e ByteString
unwrappedV = byteStringOf $ many $ satisfy (\c -> isWhitespace c || c == '>')

-- | xml production [25]
--
-- >>> runParserMaybe eq " = "
-- Just ()
--
-- >>> runParserMaybe eq "="
-- Just ()
eq :: Parser e ()
eq = optional wss *> $(char '=') <* optional wss

-- | some with a separator
--
-- >>> runParser (sep ws (many (satisfy (/= ' ')))) "a b c"
-- OK ["a","b","c"] ""
sep :: Parser e s -> Parser e a -> Parser e [a]
sep s p = (:) <$> p <*> many (s *> p)

-- | parser bracketed by two other parsers
--
-- >>> runParser (bracketed ($(char '[')) ($(char ']')) (many (satisfy (/= ']')))) "[bracketed]"
-- OK "bracketed" ""
bracketed :: Parser e b -> Parser e b -> Parser e a -> Parser e a
bracketed o c p = o *> p <* c

bracketedSB :: Parser e [Char]
bracketedSB = bracketed $(char '[') $(char ']') (many (satisfy (/= ']')))

-- | parser wrapped by another parser
--
-- >>> runParser (wrapped ($(char '"')) (many (satisfy (/= '"')))) "\"wrapped\""
-- OK "wrapped" ""
wrapped :: Parser e () -> Parser e a -> Parser e a
wrapped x p = bracketed x x p

-- | A single digit
--
-- runParserMaybe digit "5"
-- Just 5
digit :: Parser e Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

-- | (unsigned) Int parser
--
-- runParserMaybe int "567"
-- Just 567
int :: Parser e Int
int = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

digits :: Parser e (Int, Int)
digits = chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))

-- |
-- >>> runParser double "1.234x"
-- OK 1.234 "x"
--
-- >>> runParser double "."
-- Fail
--
-- >>> runParser double "123"
-- OK 123.0 ""
--
-- >>> runParser double ".123"
-- OK 0.123 ""
--
-- >>> runParser double "123."
-- OK 123.0 ""
double :: Parser e Double
double = do
  (placel, nl) <- digits
  withOption
    ($(char '.') *> digits)
    ( \(placer, nr) ->
        case (placel, placer) of
          (1, 1) -> empty
          _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer
    )
    ( case placel of
        1 -> empty
        _ -> pure $ fromIntegral nl
    )

minus :: Parser e ()
minus = $(char '-')

-- |
-- >>> runParser (signed double) "-1.234x"
-- OK (-1.234) "x"
signed :: (Num b) => Parser e b -> Parser e b
signed p = do
  m <- optional minus
  case m of
    Nothing -> p
    Just () -> negate <$> p

