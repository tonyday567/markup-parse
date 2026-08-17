{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parsing and tree operations for markup.
module Data.Markup.Parser
  ( -- * high-level parsers
    markup,
    markup_,
    tokenize,
    tokenize_,
    tokenP,

    -- * token-stream tree builder
    gather,
    gather_,
    degather,
    degather_,

    -- * normalisation & well-formedness
    normalize,
    normContent,
    wellFormed,
    isWellFormed,

    -- * element construction
    element,
    element_,
    emptyElem,
    elementc,
    contentRaw,
    addAttrs,

    -- * token parser helpers
    runMarkupParser,
    runParser_,
    runParserWarn,
    nameP,
    attrsP,
    ws,
    ws_,

    -- * constants
    doctypeHtml,
    doctypeXml,
    selfClosers,

    -- * internal token parsers (exported for reuse / testing)
    tokenHtmlP,
    tokenXmlP,
    isWhitespace,
    isNameChar,
    isNameCharXml,
    isNameStartChar,
    isAttrName,
    isBooleanAttrName,
    bs,
    eq_,
    wrappedQ,
    nameStartCharXmlP,
    nameCharXmlP,
    nameXmlP,
    commentP_,
    contentP_,
    declXmlP_,
    doctypeXmlP_,
    startTagsXmlP_,
    attrXmlP_,
    endTagXmlP_,
    nameHtmlP,
    startTagsHtmlP_,
    endTagHtmlP_,
    attrHtmlP_,
    attrsHtmlP_,
    doctypeHtmlP_,
    bogusCommentHtmlP_,
  )
where

import Circuit.Parser
  ( Parser,
    capturedBS,
    char,
    many,
    satisfy,
    skipWhile,
    some,
    string,
    (<|>),
  )
import Circuit.Parser qualified as CP
import Circuit.Parser.Primitives (isLatinLetter)
import Control.Category ((>>>))
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char
import Data.Function
import Data.Functor.Identity (Identity)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Markup
import Data.Markup.Warn
import Data.Maybe
import Data.These
import Data.Tree

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Markup
-- >>> import Data.Tree

-- | Convert bytestrings to 'Markup'
--
-- Two-phase pipeline: lexical (tokenize) then semantic (gather)
--
-- >>> markup Html "<foo><br></foo><baz"
-- These [MarkupParser (ParserLeftover "<baz")] (Markup {elements = [Node {rootLabel = OpenTag StartTag "foo" [], subForest = [Node {rootLabel = OpenTag StartTag "br" [], subForest = []}]}]})
markup :: Standard -> ByteString -> Warn Markup
markup s b = b & (tokenize s >=> gatherTokens s)

-- | 'markup' but errors on warnings.
markup_ :: Standard -> ByteString -> Markup
markup_ s b = markup s b & warnError

-- | Wrapper for gather to work with Kleisli composition in markup pipeline
gatherTokens :: Standard -> [Token] -> Warn Markup
gatherTokens s ts = case runTP (gather s) ts of
  ([], result) -> result
  _ -> error "Impossible: gather should consume all tokens"

-- | A 'Token' parser.
--
-- >>> runMarkupParser (tokenP Html) "<foo>content</foo>"
-- ("content</foo>",These () (OpenTag StartTag "foo" []))
tokenP :: Standard -> Parser Identity ByteString Char Token
tokenP Html = tokenHtmlP
tokenP Xml = tokenXmlP

-- | Parse a bytestring into tokens
--
-- >>> tokenize Html "<foo>content</foo>"
-- That [OpenTag StartTag "foo" [],Content "content",EndTag "foo"]
tokenize :: Standard -> ByteString -> Warn [Token]
tokenize s b = first ((: []) . MarkupParser) $ runParserWarn (many (tokenP s)) b

-- | tokenize but errors on warnings.
tokenize_ :: Standard -> ByteString -> [Token]
tokenize_ s b = tokenize s b & warnError

-- | Standard Html Doctype
doctypeHtml :: Markup
doctypeHtml = Markup $ pure $ pure (Doctype "DOCTYPE html")

-- | Standard Xml Doctype
doctypeXml :: Markup
doctypeXml =
  Markup
    [ pure $ Decl "xml" [Attr "version" "1.0", Attr "encoding" "utf-8"],
      pure $ Doctype "DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\""
    ]

-- ============================================================================
-- Character predicates (local, replacing mpar imports)
-- ============================================================================

isWhitespace :: Char -> Bool
isWhitespace ' ' = True
isWhitespace '\n' = True
isWhitespace '\t' = True
isWhitespace '\r' = True
isWhitespace _ = False

-- ============================================================================
-- Token parsers (Circuit.Parser, ByteString stream)
-- ============================================================================

-- | Matched span as a 'ByteString' ('capturedBS' — flatparse specialty).
bs :: Parser Identity ByteString Char a -> Parser Identity ByteString Char ByteString
bs p = fst <$> capturedBS p

-- | equals sign with optional whitespace
eq_ :: Parser Identity ByteString Char ()
eq_ = skipWhile isWhitespace *> char '=' *> skipWhile isWhitespace

-- | quoted string: single or double quoted
wrappedQ :: Parser Identity ByteString Char ByteString
wrappedQ =
  (char '\'' *> bs (many (satisfy (/= '\''))) <* char '\'')
    <|> (char '"' *> bs (many (satisfy (/= '"'))) <* char '"')

tokenXmlP :: Parser Identity ByteString Char Token
tokenXmlP =
  (string "<!--" *> commentP_)
    <|> (string "<!" *> doctypeXmlP_)
    <|> (string "</" *> endTagXmlP_)
    <|> (string "<?" *> declXmlP_)
    <|> (string "<" *> startTagsXmlP_)
    <|> contentP_

tokenHtmlP :: Parser Identity ByteString Char Token
tokenHtmlP =
  (string "<!--" *> commentP_)
    <|> (string "<!" *> doctypeHtmlP_)
    <|> (string "</" *> endTagHtmlP_)
    <|> (string "<?" *> bogusCommentHtmlP_)
    <|> (string "<" *> startTagsHtmlP_)
    <|> contentP_

-- XML name start char (production [4])
isNameStartChar :: Char -> Bool
isNameStartChar x =
  isLatinLetter x
    || x == ':'
    || x == '_'
    || (x >= '\xC0' && x <= '\xD6')
    || (x >= '\xD8' && x <= '\xF6')
    || (x >= '\xF8' && x <= '\xFF')

-- XML/HMTL name char
isNameChar :: Char -> Bool
isNameChar x = not (isWhitespace x || x == '/' || x == '<' || x == '>')

isNameCharXml :: Char -> Bool
isNameCharXml x =
  isLatinLetter x
    || Data.Char.isDigit x
    || x `elem` (":_-.·" :: String)
    || (x >= '\xC0' && x <= '\xD6')
    || (x >= '\xD8' && x <= '\xF6')
    || (x >= '\xF8' && x <= '\xFF')

isAttrName :: Char -> Bool
isAttrName x = not (isWhitespace x || x == '/' || x == '>' || x == '=' || x == '<')

isBooleanAttrName :: Char -> Bool
isBooleanAttrName x = not (isWhitespace x || x == '/' || x == '>' || x == '<')

-- XML parsers

nameStartCharXmlP :: Parser Identity ByteString Char Char
nameStartCharXmlP = satisfy isNameStartChar

nameCharXmlP :: Parser Identity ByteString Char Char
nameCharXmlP = satisfy isNameCharXml

nameXmlP :: Parser Identity ByteString Char ByteString
nameXmlP = bs (nameStartCharXmlP *> many nameCharXmlP)

commentP_ :: Parser Identity ByteString Char Token
commentP_ = Comment <$> (bs (many (satisfy (/= '-') <|> (char '-' *> satisfy (/= '-')))) <* string "-->")

contentP_ :: Parser Identity ByteString Char Token
contentP_ = Content <$> bs (some (satisfy (/= '<')))

declXmlP_ :: Parser Identity ByteString Char Token
declXmlP_ =
  let attr key = Attr (B.pack key) <$> (skipWhile isWhitespace *> string key *> eq_ *> wrappedQ)
      one x = [x]
   in string "xml"
        *> (Decl "xml" <$> ((:) <$> attr "version" <*> (one <$> attr "encoding")))
        <* skipWhile isWhitespace
        <* string "?>"

doctypeXmlP_ :: Parser Identity ByteString Char Token
doctypeXmlP_ =
  Doctype
    <$> ( bs
            ( string "DOCTYPE"
                *> skipWhile isWhitespace
                *> void nameXmlP
                *> skipWhile isWhitespace
                *> many (satisfy (/= '>'))
            )
            <* char '>'
        )

startTagsXmlP_ :: Parser Identity ByteString Char Token
startTagsXmlP_ =
  OpenTag EmptyElemTag
    <$> (nameXmlP <* skipWhile isWhitespace)
    <*> (many (skipWhile isWhitespace *> attrXmlP_) <* skipWhile isWhitespace <* string "/>")
      <|> OpenTag StartTag
    <$> (nameXmlP <* skipWhile isWhitespace)
    <*> (many (skipWhile isWhitespace *> attrXmlP_) <* skipWhile isWhitespace <* string ">")

attrXmlP_ :: Parser Identity ByteString Char Attr
attrXmlP_ = Attr <$> (nameXmlP <* eq_) <*> wrappedQ

endTagXmlP_ :: Parser Identity ByteString Char Token
endTagXmlP_ = EndTag <$> (nameXmlP <* skipWhile isWhitespace <* char '>')

-- HTML parsers

nameHtmlP :: Parser Identity ByteString Char ByteString
nameHtmlP = bs (satisfy isLatinLetter *> many (satisfy isNameChar))

startTagsHtmlP_ :: Parser Identity ByteString Char Token
startTagsHtmlP_ =
  OpenTag StartTag
    <$> (nameHtmlP <* skipWhile isWhitespace)
    <*> (attrsHtmlP_ <* skipWhile isWhitespace <* string ">")
      <|> OpenTag EmptyElemTag
    <$> (nameHtmlP <* skipWhile isWhitespace)
    <*> (attrsHtmlP_ <* skipWhile isWhitespace <* string "/>")

endTagHtmlP_ :: Parser Identity ByteString Char Token
endTagHtmlP_ = EndTag <$> (nameHtmlP <* skipWhile isWhitespace <* char '>')

attrHtmlP_ :: Parser Identity ByteString Char Attr
attrHtmlP_ =
  (Attr <$> (bs (many (satisfy isAttrName)) <* eq_) <*> (wrappedQ <|> bs (some (satisfy isBooleanAttrName))))
    <|> (flip Attr B.empty <$> bs (some (satisfy isBooleanAttrName)))

attrsHtmlP_ :: Parser Identity ByteString Char [Attr]
attrsHtmlP_ = many (skipWhile isWhitespace *> attrHtmlP_) <* skipWhile isWhitespace

doctypeHtmlP_ :: Parser Identity ByteString Char Token
doctypeHtmlP_ =
  Doctype
    <$> ( bs
            ( string "DOCTYPE"
                *> skipWhile isWhitespace
                *> void nameHtmlP
                *> skipWhile isWhitespace
            )
            <* char '>'
        )

bogusCommentHtmlP_ :: Parser Identity ByteString Char Token
bogusCommentHtmlP_ = Comment <$> bs (some (satisfy (/= '<')))

-- | Parse a tag name.
nameP :: Standard -> Parser Identity ByteString Char ByteString
nameP Html = nameHtmlP
nameP Xml = nameXmlP

-- | Parse an attribute.
-- | Parse attributes list.
attrsP :: Standard -> Parser Identity ByteString Char [Attr]
attrsP Html = attrsHtmlP_
attrsP Xml = many (skipWhile isWhitespace *> attrXmlP_) <* skipWhile isWhitespace

-- | Alias for single whitespace (backward compat with mpar)
ws :: Parser Identity ByteString Char Char
ws = satisfy isWhitespace

-- | Alias for skip whitespace (backward compat with mpar)
ws_ :: Parser Identity ByteString Char ()
ws_ = skipWhile isWhitespace

-- | Run parser, returning leftovers and errors as 'ParserWarning's.
--
-- >>> runParserWarn ws " "
-- That ' '
--
-- >>> runParserWarn ws "x"
-- This ParserUncaught
--
-- >>> runParserWarn ws " x"
-- These (ParserLeftover "x") ' '
runParserWarn :: Parser Identity ByteString Char a -> ByteString -> These ParserWarning a
runParserWarn p s = case CP.runParserIdentity p s of
  These a rest | B.null rest -> That a
  These a rest -> These (ParserLeftover (take 200 (B.unpack rest))) a
  This a -> That a
  That _ -> This ParserUncaught

-- | Run a parser and return the remaining input and result as a tuple
runMarkupParser :: Parser Identity ByteString Char a -> ByteString -> (ByteString, These () a)
runMarkupParser p s = case CP.runParserIdentity p s of
  These a s' | B.null s' -> (B.empty, That a)
  These a s' -> (s', These () a)
  This a -> (B.empty, That a)
  That s' -> (s', This ())

runParser_ :: Parser Identity ByteString Char a -> ByteString -> a
runParser_ p s = case CP.runParserIdentity p s of
  These a _ -> a
  This a -> a
  That _ -> error "Uncaught parse failure"

-- ============================================================================
-- Tree operations
-- ============================================================================

-- | Append attributes to an existing Token attribute list. Returns Nothing for tokens that do not have attributes.
addAttrs :: [Attr] -> Token -> Maybe Token
addAttrs as (OpenTag t n as') = Just $ OpenTag t n (as <> as')
addAttrs _ _ = Nothing

-- | Html tags that self-close
selfClosers :: [NameTag]
selfClosers =
  [ "area",
    "base",
    "br",
    "col",
    "embed",
    "hr",
    "img",
    "input",
    "link",
    "meta",
    "param",
    "source",
    "track",
    "wbr"
  ]

-- | Create 'Markup' from a name tag and attributes that wraps some other markup.
--
-- >>> element "div" [] (element_ "br" [])
-- Markup {elements = [Node {rootLabel = OpenTag StartTag "div" [], subForest = [Node {rootLabel = OpenTag StartTag "br" [], subForest = []}]}]}
element :: NameTag -> [Attr] -> Markup -> Markup
element n as (Markup xs) = Markup [Node (OpenTag StartTag n as) xs]

-- | Create 'Markup' from a name tag and attributes that doesn't wrap some other markup. The 'OpenTagType' used is 'StartTag'. Use 'emptyElem' if you want to create 'EmptyElemTag' based markup.
--
-- >>> (element_ "br" [])
-- Markup {elements = [Node {rootLabel = OpenTag StartTag "br" [], subForest = []}]}
element_ :: NameTag -> [Attr] -> Markup
element_ n as = Markup [Node (OpenTag StartTag n as) []]

-- | Create 'Markup' from a name tag and attributes using 'EmptyElemTag', that doesn't wrap some other markup. No checks are made on whether this creates well-formed markup.
--
-- >>> emptyElem "br" []
-- Markup {elements = [Node {rootLabel = OpenTag EmptyElemTag "br" [], subForest = []}]}
emptyElem :: NameTag -> [Attr] -> Markup
emptyElem n as = Markup [Node (OpenTag EmptyElemTag n as) []]

-- | Create 'Markup' from a name tag and attributes that wraps some 'Content'. No escaping is performed.
--
-- >>> elementc "div" [] "content"
-- Markup {elements = [Node {rootLabel = OpenTag StartTag "div" [], subForest = [Node {rootLabel = Content "content", subForest = []}]}]}
elementc :: NameTag -> [Attr] -> ByteString -> Markup
elementc n as b = element n as (contentRaw b)

-- | Create a Markup element from a bytestring, not escaping the usual characters.
--
-- >>> contentRaw "<content>"
-- Markup {elements = [Node {rootLabel = Content "<content>", subForest = []}]}
contentRaw :: ByteString -> Markup
contentRaw b = Markup [pure $ Content b]

normTokenAttrs :: Token -> Token
normTokenAttrs (OpenTag t n as) = OpenTag t n (normAttrs as)
normTokenAttrs x = x

-- | normalize an attribution list, removing duplicate AttrNames, and space concatenating class values.
normAttrs :: [Attr] -> [Attr]
normAttrs as =
  uncurry Attr
    <$> Map.toList
      ( foldl'
          ( \s (Attr n v) ->
              Map.insertWithKey
                ( \k new old ->
                    case k of
                      "class" -> old <> " " <> new
                      _ -> new
                )
                n
                v
                s
          )
          Map.empty
          as
      )

-- | Concatenate sequential content and normalize attributes; unwording class values and removing duplicate attributes (taking last).
normalize :: Markup -> Markup
normalize m = normContent $ Markup $ fmap (fmap normTokenAttrs) (elements m)

-- | Are the trees in the markup well-formed?
isWellFormed :: Standard -> Markup -> Bool
isWellFormed s = (== []) . wellFormed s

-- | Check for well-formedness and return warnings encountered.
--
-- >>> wellFormed Html $ Markup [Node (Comment "") [], Node (EndTag "foo") [], Node (OpenTag EmptyElemTag "foo" []) [Node (Content "bar") []], Node (OpenTag EmptyElemTag "foo" []) []]
-- [EmptyContent,EndTagInTree,LeafWithChildren,BadEmptyElemTag]
wellFormed :: Standard -> Markup -> [MarkupWarning]
wellFormed s (Markup trees) = List.nub $ mconcat (foldTree checkNode <$> trees)
  where
    checkNode (OpenTag StartTag _ _) xs = mconcat xs
    checkNode (OpenTag EmptyElemTag n _) [] =
      bool [] [BadEmptyElemTag] (notElem n selfClosers && s == Html)
    checkNode (EndTag _) [] = [EndTagInTree]
    checkNode (Content b) [] = bool [] [EmptyContent] (b == "")
    checkNode (Comment b) [] = bool [] [EmptyContent] (b == "")
    checkNode (Decl b as) []
      | b == "" = [EmptyContent]
      | s == Html && as /= [] = [BadDecl]
      | s == Xml && ("version" `elem` (attrName <$> as)) && ("encoding" `elem` (attrName <$> as)) =
          [BadDecl]
      | otherwise = []
    checkNode (Doctype b) [] = bool [] [EmptyContent] (b == "")
    checkNode _ _ = [LeafWithChildren]

-- | Normalise Content in Markup, concatenating adjacent Content, and removing mempty Content.
normContent :: Markup -> Markup
normContent (Markup trees) = Markup $ foldTree (\x xs -> Node x (filter ((/= Content "") . rootLabel) $ concatContent xs)) <$> concatContent trees

concatContent :: [Tree Token] -> [Tree Token]
concatContent = \case
  ((Node (Content t) _) : (Node (Content t') _) : ts) -> concatContent $ Node (Content (t <> t')) [] : ts
  (t : ts) -> t : concatContent ts
  [] -> []

-- | Gather together token trees from a token list, placing child elements in nodes and removing EndTags.
gather :: Standard -> TokenParser [MarkupWarning] Markup
gather s = TokenParser $ \ts ->
  let (Cursor finalSibs finalParents, warnings) =
        foldl' (\(c, xs) t -> incCursor s t c & second (maybeToList >>> (<> xs))) (Cursor [] [], []) ts
   in case (finalSibs, finalParents, warnings) of
        (sibs, [], []) -> ([], That (Markup (reverse sibs)))
        ([], [], xs) -> ([], This xs)
        (sibs, ps, xs) ->
          let result = reverse $ foldl' (\ss' (p, ss) -> Node p (reverse ss') : ss) sibs ps
           in ([], These (xs <> [UnclosedTag]) (Markup result))

-- | 'gather' but errors on warnings.
gather_ :: Standard -> [Token] -> Markup
gather_ s ts = case runTP (gather s) ts of
  ([], That m) -> m
  ([], This w) -> error (showWarnings w)
  ([], These w m) -> if null w then m else error (showWarnings w)
  _ -> error "Impossible: gather should consume all tokens"

incCursor :: Standard -> Token -> Cursor -> (Cursor, Maybe MarkupWarning)
-- Only StartTags are ever pushed on to the parent list, here:
incCursor Xml t@(OpenTag StartTag _ _) (Cursor ss ps) = (Cursor [] ((t, ss) : ps), Nothing)
incCursor Html t@(OpenTag StartTag n _) (Cursor ss ps) =
  (bool (Cursor [] ((t, ss) : ps)) (Cursor (Node t [] : ss) ps) (n `elem` selfClosers), Nothing)
incCursor Xml t@(OpenTag EmptyElemTag _ _) (Cursor ss ps) = (Cursor (Node t [] : ss) ps, Nothing)
incCursor Html t@(OpenTag EmptyElemTag n _) (Cursor ss ps) =
  ( Cursor (Node t [] : ss) ps,
    bool (Just BadEmptyElemTag) Nothing (n `elem` selfClosers)
  )
incCursor _ (EndTag n) (Cursor ss ((p@(OpenTag StartTag n' _), ss') : ps)) =
  ( Cursor (Node p (reverse ss) : ss') ps,
    bool (Just (TagMismatch n n')) Nothing (n == n')
  )
-- Non-StartTag on parent list
incCursor _ (EndTag _) (Cursor ss ((p, ss') : ps)) =
  ( Cursor (Node p (reverse ss) : ss') ps,
    Just LeafWithChildren
  )
incCursor _ (EndTag _) (Cursor ss []) =
  ( Cursor ss [],
    Just UnmatchedEndTag
  )
incCursor _ t (Cursor ss ps) = (Cursor (Node t [] : ss) ps, Nothing)

data Cursor = Cursor
  { -- siblings, not (yet) part of another element
    _sibs :: [Tree Token],
    -- open elements and their siblings.
    _stack :: [(Token, [Tree Token])]
  }

-- | Convert a markup into a token list, adding end tags.
degather :: Standard -> Markup -> Warn [Token]
degather s (Markup tree) = concatWarns $ foldTree (addCloseTags s) <$> tree

-- | 'degather' but errors on warning
degather_ :: Standard -> Markup -> [Token]
degather_ s m = degather s m & warnError

addCloseTags :: Standard -> Token -> [Warn [Token]] -> Warn [Token]
addCloseTags std s@(OpenTag StartTag n _) children
  | children /= [] && n `elem` selfClosers && std == Html =
      These [SelfCloserWithChildren] [s] <> concatWarns children
  | n `elem` selfClosers && std == Html =
      That [s] <> concatWarns children
  | otherwise =
      That [s] <> concatWarns children <> That [EndTag n]
addCloseTags _ x xs = case xs of
  [] -> That [x]
  cs -> These [LeafWithChildren] [x] <> concatWarns cs
