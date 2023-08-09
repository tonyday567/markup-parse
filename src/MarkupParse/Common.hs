{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Lower-level flatparse parsers
module MarkupParse.Common
  ( runParserMaybe,
    runParserEither,
    runParser_,
    runParserTextMaybe,
    runParserTextEither,
    runParserText_,
    testParser,
    Expected (..),
    Error (..),
    prettyError,
    cut,
    cut',
    isWhitespace,
    ws_',
    ws,
    ws_,
    wss,
    lt,
    gt,
    gtc,
    oct,
    sq,
    dq,
    wrappedQ,
    wrappedQNoGuard,
    eq,
    sep,
    comma,
    bracketed,
    wrapped,
    digit,
    int,
    double,
    signed,
    takeText,
  )
where

import Data.Bool
import Data.ByteString (ByteString)
import Data.Char hiding (isDigit)
import Data.List (replicate)
import FlatParse.Basic hiding (cut)
import GHC.Exts
import Prelude hiding (replicate)
import Data.Text (Text, unpack, pack)
import Control.Category ((>>>))

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import MarkupParse.Common
-- >>> import MarkupParse.Xml
-- >>> import FlatParse.Basic

-- * parser error model

--
-- taken from flatparse examples

-- | An expected item which is displayed in error messages.
data Expected
  = -- | An error message.
    Msg String
  | -- | A literal expected thing.
    Lit String
  deriving (Eq, Show, Ord)

instance IsString Expected where fromString = Lit

-- | A parsing error.
data Error
  = -- | A precisely known error, like leaving out "in" from "let".
    Precise Pos Expected
  | -- | An imprecise error, when we expect a number of different things,
    --   but parse something else.
    Imprecise Pos [Expected]
  deriving (Show)

errorPos :: Error -> Pos
errorPos (Precise p _) = p
errorPos (Imprecise p _) = p

-- | Merge two errors. Inner errors (which were thrown at points with more consumed inputs)
--   are preferred. If errors are thrown at identical input positions, we prefer precise errors
--   to imprecise ones.
--
--   The point of prioritizing inner and precise errors is to suppress the deluge of "expected"
--   items, and instead try to point to a concrete issue to fix.
merge :: Error -> Error -> Error
merge e e' = case (errorPos e, errorPos e') of
  (p, p') | p < p' -> e'
  (p, p') | p > p' -> e
  (p, _) -> case (e, e') of
    (Precise {}, _) -> e
    (_, Precise {}) -> e'
    (Imprecise _ es, Imprecise _ es') -> Imprecise p (es <> es')
{-# NOINLINE merge #-} -- merge is "cold" code, so we shouldn't inline it.

-- | Pretty print an error. The `B.ByteString` input is the source file. The offending line from the
--   source is displayed in the output.
prettyError :: ByteString -> Error -> String
prettyError b e =
  let pos :: Pos
      pos = case e of
        Imprecise p _ -> p
        Precise p _ -> p
      ls = linesUtf8 b
      (l, c) = head $ posLineCols b [pos]
      line = if l < length ls then ls !! l else ""
      line' = if length line > 300 then Prelude.drop (c - 50) $ Prelude.take (c + 50) line else line
      linum = show l
      lpad = fmap (const ' ') linum

      expected (Lit s) = show s
      expected (Msg s) = s

      err (Precise _ e) = expected e
      err (Imprecise _ es) = imprec es

      imprec :: [Expected] -> String
      imprec [] = error "impossible"
      imprec [e] = expected e
      imprec (e : es) = (expected e <> go es)
        where
          go [] = ""
          go [e] = (" or " <> expected e)
          go (e : es) = (", " <> expected e <> go es)
   in (show l <> (":" <> (show c <> (":\n" <> (lpad <> ("|\n" <> (linum <> ("| " <> (line' <> ("\n" <> (lpad <> ("| " <> (replicate c ' ' <> ("^\n" <> ("parse error: expected " <> err e)))))))))))))))

-- | Imprecise cut: we slap a list of items on inner errors.
cut :: Parser Error a -> [Expected] -> Parser Error a
cut p es = do
  pos <- getPos
  cutting p (Imprecise pos es) merge

-- | Precise cut: we propagate at most a single error.
cut' :: Parser Error a -> Expected -> Parser Error a
cut' p e = do
  pos <- getPos
  cutting p (Precise pos e) merge

-- | run a Parser, Nothing on failure
runParserMaybe :: Parser e a -> ByteString -> Maybe a
runParserMaybe p b = case runParser p b of
  OK r _ -> Just r
  Fail -> Nothing
  Err _ -> Nothing

-- | Run parser, Left error on failure.
runParserEither :: Parser Error a -> ByteString -> Either String a
runParserEither p bs = case runParser p bs of
  Err e -> Left $ prettyError bs e
  OK a _ -> Right a
  Fail -> Left "uncaught parse error"

-- | Run parser, throws an error on failure.
runParser_ :: Parser Error a -> ByteString -> a
runParser_ p bs = case runParser p bs of
  Err e -> error $ prettyError bs e
  OK a _ -> a
  Fail -> error "uncaught parse error"

-- | Run parser, print pretty error on failure.
testParser :: (Show a) => Parser Error a -> String -> IO ()
testParser p str = case fromString str of
  b -> case runParser p b of
    Err e -> putStrLn $ prettyError b e
    OK a _ -> print a
    Fail -> putStrLn "uncaught parse error"

-- | run a Parser on Text, Nothing on failure
runParserTextMaybe :: Parser e a -> Text -> Maybe a
runParserTextMaybe p = unpack >>> strToUtf8 >>> runParserMaybe p

-- | Run parser on Text, Left error on failure.
runParserTextEither :: Parser Error a -> Text -> Either String a
runParserTextEither p = unpack >>> strToUtf8 >>> runParserEither p

-- | Run parser on Text, throws an error on failure.
runParserText_ :: Parser Error a -> Text -> a
runParserText_ p = unpack >>> strToUtf8 >>> runParser_ p

-- * parsing
isWs :: Char -> Bool
isWs x =
  x == ' '
    || x == '\n'
    || x == '\t'
    || x == '\r'

-- | single whitespace
--
-- >>> runParser ws " \nx"
-- OK ' ' "\nx"
ws :: Parser e Char
ws = satisfy isWs

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
           _ -> pure ()
         |]
   )

-- | Equivalent to @inClass "\x09\x0a\x0c "@
isWhitespace :: Char -> Bool
isWhitespace '\x09' = True
isWhitespace '\x0a' = True
isWhitespace '\x0c' = True
isWhitespace '\x0d' = True
isWhitespace ' '    = True
isWhitespace _      = False


-- | consume whitespace
ws_' :: Parser e ()
ws_' = many (satisfy isWhitespace) >> pure ()

-- | multiple whitespace
--
-- >>> runParser wss " \nx"
-- OK " \n" "x"
--
-- >>> runParser wss "x"
-- Fail
wss :: Parser e ByteString
wss = byteStringOf $ some ws

-- >>> runParserMaybe lt "<"
-- Just ()
lt :: Parser e ()
lt = $(char '<')

-- | closing tag char
--
-- >>> runParserMaybe gt ">"
-- Just ()
gt :: Parser e ()
gt = $(char '>')

-- | self-closing tag
--
-- >>> runParserMaybe gtc "/>"
-- Just ()
gtc :: Parser e ()
gtc = $(string "/>")

-- | open closer tag
--
-- >>> runParserMaybe oct "</"
-- Just ()
oct :: Parser e ()
oct = $(string "</")

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
-- >>> runParserMaybe (wrappedQNoGuard xmlName) "\"name\""
-- Just "name"
--
-- but will consume quotes if the underlying parser does.
--
-- >>> runParserMaybe (wrappedQNoGuard (many anyChar)) "\"name\""
-- Nothing
wrappedQNoGuard :: Parser e a -> Parser e a
wrappedQNoGuard p = wrapped dq p <|> wrapped sq p

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

comma :: Parser e ()
comma = $(char ',')

-- | parser bracketed by two other parsers
--
-- >>> runParser (bracketed ($(char '[')) ($(char ']')) (many (satisfy (/= ']')))) "[bracketed]"
-- OK "bracketed" ""
bracketed :: Parser e b -> Parser e b -> Parser e a -> Parser e a
bracketed o c p = o *> p <* c

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

takeText :: Parser e Text
takeText = pack <$> takeRestString
