{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | A 'Markup' parser and printer of strict bytestrings. 'Markup' is a representation of data such as HTML, SVG or XML but the parsing is sub-standard.
module MarkupParse
  ( -- $usage

    -- * Markup
    Markup (..),
    Standard (..),
    markup,
    markup_,
    RenderStyle (..),
    markdown,
    normalize,
    wellFormed,
    isWellFormed,

    -- * Warnings
    MarkupWarning (..),
    Result,
    resultError,
    resultEither,
    resultMaybe,

    -- * Token components
    TagName,
    name,
    selfClosers,
    AttrName,
    AttrValue,
    Attr (..),
    attrs,

    -- * Tokens
    Token (..),
    tokenize,
    tokenize_,
    token,
    detokenize,
    gather,
    gather_,
    degather,
    degather_,

    -- * XML specific Parsers
    xmlVersionInfo,
    xmlEncodingDecl,
    xmlStandalone,
    xmlVersionNum,
    xmlEncName,
    xmlYesNo,
  )
where

import Control.Category ((>>>))
import Control.DeepSeq
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char hiding (isDigit)
import Data.Foldable
import Data.Function
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.String.Interpolate
import Data.These
import Data.Tree
import Data.TreeDiff
import FlatParse.Basic hiding (Result, cut, take)
import GHC.Generics
import MarkupParse.FlatParse
import Prelude hiding (replicate)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings
-- >>> import MarkupParse
-- >>> import MarkupParse.Patch
-- >>> import MarkupParse.FlatParse
-- >>> import FlatParse.Basic
-- >>> import Data.String.Interpolate
-- >>> import Data.ByteString.Char8 qualified as B
-- >>> import Data.Tree

-- $usage
--
-- > import MarkupParse
-- > import Data.ByteString qualified as B
-- >
-- > bs <- B.readFile "other/line.svg"
-- > m = markup_ bs
--
-- @'markdown' . ''markup_'@ is an approximate round trip from 'ByteString' to 'Markup' back to ByteString'. The underscores represent versions of main functions that throw an exception on warnings encountered along the way.
--
-- At a lower level, a round trip pipeline might look something like:
--
-- > :t tokenize Html >=> gather Html >>> fmap (Markup Html >>> normalize) >=> degather >>> fmap (fmap (detokenize Html) >>> mconcat)
-- > ByteString -> These [MarkupWarning] ByteString
--
-- From left to right:
--
-- - 'tokenize' converts a 'ByteString' to a 'Token' list,
--
-- - 'gather' takes the tokens and gathers them into 'Tree's of tokens
--
-- - this is then wrapped into the 'Markup' data type.
--
-- - 'normalize' concatenates content, and normalizes attributes,
--
-- - 'degather' turns the markup tree back into a token list. Finally,
--
-- - 'detokenize' turns a token back into a bytestring.
--
-- Along the way, the kleisi fishies and compose forward usage accumulates any warnings via the 'These' monad instance.

-- | From a parsing pov, Html & Xml (& Svg) are close enough that they share a lot of parsing logic, so that parsing and printing just need some tweaking.
--
-- The xml parsing logic is based on the XML productions found in https://www.w3.org/TR/xml/
--
-- The html parsing was based on a reading of <https://hackage.haskell.org/package/html-parse html-parse>, but ignores the various '\x00' to '\xfffd' & eof directives that form part of the html standards.
data Standard = Html | Xml deriving (Eq, Show, Ord, Generic, NFData)

instance ToExpr Standard

-- | A 'Tree' list of markup 'Token's
--
-- >>> markup Html "<foo class=\"bar\">baz</foo>"
-- That (Markup {standard = Html, markupTree = [Node {rootLabel = StartTag "foo" [Attr "class" "bar"], subForest = [Node {rootLabel = Content "baz", subForest = []}]}]})
data Markup = Markup {standard :: Standard, markupTree :: [Tree Token]} deriving (Show, Eq, Ord, Generic, NFData)

instance ToExpr Markup

-- | markup-parse generally tries to continue on parse errors, and return what has/can still be parsed, together with any warnings.
data MarkupWarning
  = -- | A tag ending with "/>" that is not an element of 'selfClosers' (Html only).
    BadEmptyElemTag
  | -- | A tag ending with "/>" that has children. Cannot happen in the parsing phase.
    SelfCloserWithChildren
  | -- | Only a 'StartTag' can have child tokens.
    LeafWithChildren
  | -- | A CloseTag with a different name to the currently open StartTag.
    TagMismatch TagName TagName
  | -- | An EndTag with no corresponding StartTag.
    UnmatchedEndTag
  | -- | An EndTag with corresponding StartTag.
    UnclosedTag
  | -- | An EndTag should never appear in 'Markup'
    EndTagInTree
  | -- | Empty Content, Comment, Decl or Doctype
    EmptyContent
  | MarkupParser ParserWarning
  deriving (Eq, Show, Ord, Generic, NFData)

showWarnings :: [MarkupWarning] -> String
showWarnings = List.nub >>> fmap show >>> unlines

-- | The structure of many returning functions.
--
-- A common computation pipeline is to take advantage of the 'These' Monad instance eg
--
-- > markup s bs = bs & (tokenize s >=> gather s) & second (Markup s)
type Result a = These [MarkupWarning] a

-- | Convert any warnings to an 'error'
--
-- >>> resultError $ (tokenize Html) "<foo"
-- *** Exception: MarkupParser (ParserLeftover "<foo")
-- ...
resultError :: Result a -> a
resultError = these (showWarnings >>> error) id (\xs a -> bool (error (showWarnings xs)) a (xs == []))

-- | Returns Left on any warnings
--
-- >>> resultEither $ (tokenize Html) "<foo><baz"
-- Left [MarkupParser (ParserLeftover "<baz")]
resultEither :: Result a -> Either [MarkupWarning] a
resultEither = these Left Right (\xs a -> bool (Left xs) (Right a) (xs == []))

-- | Returns results if any, ignoring warnings.
--
-- >>> resultMaybe $ (tokenize Html) "<foo><baz"
-- Just [StartTag "foo" []]
resultMaybe :: Result a -> Maybe a
resultMaybe = these (const Nothing) Just (\_ a -> Just a)

-- | Convert bytestrings to 'Markup'
--
-- >>> markup Html "<foo><br></foo><baz"
-- These [MarkupParser (ParserLeftover "<baz")] (Markup {standard = Html, markupTree = [Node {rootLabel = StartTag "foo" [], subForest = [Node {rootLabel = StartTag "br" [], subForest = []}]}]})
markup :: Standard -> ByteString -> These [MarkupWarning] Markup
markup s bs = bs & (tokenize s >=> gather s) & second (Markup s)

-- | markup but errors on warnings.
markup_ :: Standard -> ByteString -> Markup
markup_ s bs = markup s bs & resultError

-- | concatenate sequential content, and normalize attributes; unwording class values and removing duplicate attributes (taking last).
--
-- >>> B.putStr $ markdown Compact $ normalize (markup_ Xml [i|<foo class="a" class="b" bar="first" bar="last"/>|])
-- <foo bar="last" class="a b"/>"
normalize :: Markup -> Markup
normalize (Markup s trees) = Markup s (normContentTrees $ fmap (fmap normTokenAttrs) trees)

-- | Are the trees in the markup well-formed?
isWellFormed :: Markup -> Bool
isWellFormed = (== []) . wellFormed

-- | Check for well-formedness and rerturn warnings encountered.
--
-- >>> wellFormed $ Markup Html [Node (Comment "") [], Node (EndTag "foo") [], Node (EmptyElemTag "foo" []) [Node (Content "bar") []], Node (EmptyElemTag "foo" []) []]
-- [EmptyContent,EndTagInTree,LeafWithChildren,BadEmptyElemTag]
wellFormed :: Markup -> [MarkupWarning]
wellFormed (Markup s trees) = List.nub $ mconcat (foldTree checkNode <$> trees)
  where
    checkNode (StartTag _ _) xs = mconcat xs
    checkNode (EmptyElemTag n _) [] =
      bool [] [BadEmptyElemTag] (not (n `elem` selfClosers) && s == Html)
    checkNode (EndTag _) [] = [EndTagInTree]
    checkNode (Content bs) [] = bool [] [EmptyContent] (bs == "")
    checkNode (Comment bs) [] = bool [] [EmptyContent] (bs == "")
    checkNode (Decl bs) [] = bool [] [EmptyContent] (bs == "")
    checkNode (Doctype bs) [] = bool [] [EmptyContent] (bs == "")
    checkNode _ _ = [LeafWithChildren]

-- | Name of token
type TagName = ByteString

-- | A Markup token
--
-- >>> runParser_ (many (token Html)) [i|<foo>content</foo>|]
-- [StartTag "foo" [],Content "content",EndTag "foo"]
--
-- >>> runParser_ (token Xml) [i|<foo/>|]
-- EmptyElemTag "foo" []
--
-- >>> runParser_ (token Html) "<!-- Comment -->"
-- Comment " Comment "
--
-- >>> runParser_ (token Xml) [i|<?xml version="1.0" encoding="UTF-8"?>|]
-- Decl "xml version=\"1.0\" encoding=\"UTF-8\""
--
-- >>> runParser_ (token Html) "<!DOCTYPE html>"
-- Doctype "DOCTYPE html"
--
-- >>> runParser_ (token Xml) "<!DOCTYPE foo [ declarations ]>"
-- Doctype "DOCTYPE foo [ declarations ]"
--
-- >>> runParser (token Html) [i|<foo a="a" b="b" c=c check>|]
-- OK (StartTag "foo" [Attr "a" "a",Attr "b" "b",Attr "c" "c",Attr "check" ""]) ""
--
-- >>> runParser (token Xml) [i|<foo a="a" b="b" c=c check>|]
-- Fail
data Token
  = -- | A start tag. https://developer.mozilla.org/en-US/docs/Glossary/Tag
    StartTag !TagName ![Attr]
  | -- | An empty element tag. Optional for XML and kind of not allowed in HTML.
    EmptyElemTag !TagName ![Attr]
  | -- | A closing tag.
    EndTag !TagName
  | -- | The content between tags.
    Content !ByteString
  | -- | Contents of a comment.
    Comment !ByteString
  | -- | Contents of a declaration
    Decl !ByteString
  | -- | Contents of a doctype declaration.
    Doctype !ByteString
  deriving (Show, Ord, Eq, Generic)

instance NFData Token

instance ToExpr Token

-- | A flatparse 'Token' parser.
--
-- >>> runParser (token Html) "<foo>content</foo>"
-- OK (StartTag "foo" []) "content</foo>"
token :: Standard -> Parser String Token
token Html = tokenHtml
token Xml = tokenXml

-- | Parse a bytestring into tokens
--
-- >>> tokenize Html [i|<foo>content</foo>|]
-- That [StartTag "foo" [],Content "content",EndTag "foo"]
tokenize :: Standard -> ByteString -> These [MarkupWarning] [Token]
tokenize s bs = first ((: []) . MarkupParser) $ runParserWarn (many (token s)) bs

-- | tokenize but errors on warnings.
tokenize_ :: Standard -> ByteString -> [Token]
tokenize_ s bs = tokenize s bs & resultError

-- | Html tags that self-close
selfClosers :: [TagName]
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

-- | Name of an attribute.
type AttrName = ByteString

-- | Value of an attribute. "" is equivalent to true with respect to boolean attributes.
type AttrValue = ByteString

-- | An attribute of a tag
--
-- In parsing, boolean attributes, which are not required to have a value in HTML,
-- will be set a value of "", which is ok. But this will then be rendered.
--
-- >>> detokenize Html <$> tokenize_ Html [i|<input checked>|]
-- ["<input checked=\"\">"]
data Attr = Attr !AttrName !AttrValue
  deriving (Generic, Show, Eq, Ord)

instance NFData Attr

instance ToExpr Attr

normTokenAttrs :: Token -> Token
normTokenAttrs (StartTag n as) = StartTag n (normAttrs as)
normTokenAttrs (EmptyElemTag n as) = EmptyElemTag n (normAttrs as)
normTokenAttrs x = x

-- | normalize an attribution list, removing duplicate AttrNames, and space concatenating class values.
normAttrs :: [Attr] -> [Attr]
normAttrs as =
  uncurry Attr
    <$> ( Map.toList $
            foldl'
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

-- | render attributes
renderAttrs :: [Attr] -> ByteString
renderAttrs = B.unwords . fmap renderAttr

-- | render an attribute
--
-- Does not attempt to escape double quotes.
renderAttr :: Attr -> ByteString
renderAttr (Attr k v) = [i|#{k}="#{v}"|]

commentClose :: Parser e ()
commentClose = $(string "-->")

charNotMinus :: Parser e ByteString
charNotMinus = byteStringOf $ satisfy (/= '-')

minusPlusChar :: Parser e ByteString
minusPlusChar = byteStringOf ($(char '-') *> charNotMinus)

comment :: Parser e Token
comment = Comment <$> byteStringOf (many (charNotMinus <|> minusPlusChar)) <* commentClose

content :: Parser e Token
content = Content <$> byteStringOf (some (satisfy (/= '<')))

-- | bytestring representation of 'Token'.
--
-- >>> detokenize Html (StartTag "foo" [])
-- "<foo>"
detokenize :: Standard -> Token -> ByteString
detokenize s = \case
  (StartTag n []) -> [i|<#{n}>|]
  (StartTag n as) -> [i|<#{n} #{renderAttrs as}>|]
  (EmptyElemTag n as) ->
    bool
      [i|<#{n} #{renderAttrs as}/>|]
      [i|<#{n} #{renderAttrs as} />|]
      (s == Html)
  (EndTag n) -> [i|</#{n}>|]
  (Content t) -> t
  (Comment t) -> [i|<!--#{t}-->|]
  (Doctype t) -> [i|<!#{t}>|]
  (Decl t) -> bool [i|<?#{t}?>|] [i|<!#{t}!>|] (s == Html)

-- | Indented 0 puts newlines in between the tags.
data RenderStyle = Compact | Indented Int deriving (Eq, Show, Generic)

indentChildren :: RenderStyle -> [ByteString] -> [ByteString]
indentChildren Compact = id
indentChildren (Indented x) =
  fmap (B.replicate x ' ' <>)

finalConcat :: RenderStyle -> [ByteString] -> ByteString
finalConcat Compact = mconcat
finalConcat (Indented _) =
  B.intercalate (B.singleton '\n')
    . filter (/= "")

-- | Convert 'Markup' to bytestrings
--
-- >>> B.putStr $ markdown (Indented 4) (markup_ Html [i|<foo><br></foo>|])
-- <foo>
--     <br>
-- </foo>
markdown :: RenderStyle -> Markup -> ByteString
markdown r (Markup std tree) =
  finalConcat r $ mconcat $ foldTree (renderBranch r std) <$> normContentTrees tree

-- note that renderBranch adds in EndTags for StartTags when needed
renderBranch :: RenderStyle -> Standard -> Token -> [[ByteString]] -> [ByteString]
renderBranch r std s@(StartTag n _) children
  | n `elem` selfClosers && std == Html =
      [detokenize std s] <> indentChildren r (mconcat children)
  | otherwise =
      [detokenize std s] <> indentChildren r (mconcat children) <> [detokenize std (EndTag n)]
renderBranch r std x children =
  -- ignoring that this should be an error
  [detokenize std x] <> indentChildren r (mconcat children)

normContentTrees :: [Tree Token] -> [Tree Token]
normContentTrees trees = foldTree (\x xs -> Node x (filter ((/= Content "") . rootLabel) $ concatContent xs)) <$> concatContent trees

concatContent :: [Tree Token] -> [Tree Token]
concatContent = \case
  ((Node (Content t) _) : (Node (Content t') _) : ts) -> concatContent $ Node (Content (t <> t')) [] : ts
  (t : ts) -> t : concatContent ts
  [] -> []

-- | Gather together token trees from a token list, placing child elements in nodes and removing EndTags.
--
-- >>> gather Html =<< tokenize Html "<foo class=\"bar\">baz</foo>"
-- That [Node {rootLabel = StartTag "foo" [Attr "class" "bar"], subForest = [Node {rootLabel = Content "baz", subForest = []}]}]
gather :: Standard -> [Token] -> These [MarkupWarning] [Tree Token]
gather s ts =
  case (finalSibs, finalParents, warnings) of
    (sibs, [], []) -> That (reverse sibs)
    ([], [], xs) -> This xs
    (sibs, ps, xs) ->
      These (xs <> [UnclosedTag]) (reverse $ foldl' (\ss' (p, ss) -> Node p (reverse ss') : ss) sibs ps)
  where
    (Cursor finalSibs finalParents, warnings) =
      foldl' (\(c, xs) t -> incCursor s t c & second (maybeToList >>> (<> xs))) (Cursor [] [], []) ts

-- | gather but errors on warnings.
gather_ :: Standard -> [Token] -> [Tree Token]
gather_ s ts = gather s ts & resultError

incCursor :: Standard -> Token -> Cursor -> (Cursor, Maybe MarkupWarning)
-- Only StartTags are ever pushed on to the parent list, here:
incCursor Xml t@(StartTag _ _) (Cursor ss ps) = (Cursor [] ((t, ss) : ps), Nothing)
incCursor Html t@(StartTag n _) (Cursor ss ps) =
  (bool (Cursor [] ((t, ss) : ps)) (Cursor (Node t [] : ss) ps) (n `elem` selfClosers), Nothing)
incCursor Xml t@(EmptyElemTag _ _) (Cursor ss ps) = (Cursor (Node t [] : ss) ps, Nothing)
incCursor Html t@(EmptyElemTag n _) (Cursor ss ps) =
  ( Cursor (Node t [] : ss) ps,
    bool (Just BadEmptyElemTag) Nothing (n `elem` selfClosers)
  )
incCursor _ (EndTag n) (Cursor ss ((p@(StartTag n' _), ss') : ps)) =
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
--
-- >>> degather =<< markup Html "<foo class=\"bar\">baz</foo>"
-- That [StartTag "foo" [Attr "class" "bar"],Content "baz",EndTag "foo"]
degather :: Markup -> These [MarkupWarning] [Token]
degather (Markup s tree) = rconcats $ foldTree (addCloseTags s) <$> tree

-- | degather but errors on warning
degather_ :: Markup -> [Token]
degather_ m = degather m & resultError

rconcats :: [Result [a]] -> Result [a]
rconcats rs = case bimap mconcat mconcat $ partitionHereThere rs of
  ([], xs) -> That xs
  (es, []) -> This es
  (es, xs) -> These es xs

addCloseTags :: Standard -> Token -> [These [MarkupWarning] [Token]] -> These [MarkupWarning] [Token]
addCloseTags std s@(StartTag n _) children
  | children /= [] && n `elem` selfClosers && std == Html =
      These [SelfCloserWithChildren] [s] <> rconcats children
  | n `elem` selfClosers && std == Html =
      That [s] <> rconcats children
  | otherwise =
      That [s] <> rconcats children <> That [EndTag n]
addCloseTags _ x xs = case xs of
  [] -> That [x]
  cs -> These [LeafWithChildren] [x] <> rconcats cs

tokenXml :: Parser e Token
tokenXml =
  $( switch
       [|
         case _ of
           "<!--" -> comment
           "<!" -> doctypeXml
           "</" -> endTagXml
           "<?" -> declXml
           "<" -> startTagsXml
           _ -> content
         |]
   )

-- [4]
nameStartChar :: Parser e Char
nameStartChar = fusedSatisfy isLatinLetter isNameStartChar isNameStartChar isNameStartChar

isNameStartChar :: Char -> Bool
isNameStartChar x =
  (x >= 'a' && x <= 'z')
    || (x >= 'A' && x <= 'Z')
    || (x == ':')
    || (x == '_')
    || (x >= '\xC0' && x <= '\xD6')
    || (x >= '\xD8' && x <= '\xF6')
    || (x >= '\xF8' && x <= '\x2FF')
    || (x >= '\x370' && x <= '\x37D')
    || (x >= '\x37F' && x <= '\x1FFF')
    || (x >= '\x200C' && x <= '\x200D')
    || (x >= '\x2070' && x <= '\x218F')
    || (x >= '\x2C00' && x <= '\x2FEF')
    || (x >= '\x3001' && x <= '\xD7FF')
    || (x >= '\xF900' && x <= '\xFDCF')
    || (x >= '\xFDF0' && x <= '\xFFFD')
    || (x >= '\x10000' && x <= '\xEFFFF')

-- [4a]
nameChar :: Parser e Char
nameChar = fusedSatisfy isNameCharAscii isNameCharExt isNameCharExt isNameCharExt

isNameCharAscii :: Char -> Bool
isNameCharAscii x =
  (x >= 'a' && x <= 'z')
    || (x >= 'A' && x <= 'Z')
    || (x >= '0' && x <= '9')
    || (x == ':')
    || (x == '_')
    || (x == '-')
    || (x == '.')

isNameCharExt :: Char -> Bool
isNameCharExt x =
  (x >= 'a' && x <= 'z')
    || (x >= 'A' && x <= 'Z')
    || (x >= '0' && x <= '9')
    || (x == ':')
    || (x == '_')
    || (x == '-')
    || (x == '.')
    || (x == '\xB7')
    || (x >= '\xC0' && x <= '\xD6')
    || (x >= '\xD8' && x <= '\xF6')
    || (x >= '\xF8' && x <= '\x2FF')
    || (x >= '\x300' && x <= '\x36F')
    || (x >= '\x370' && x <= '\x37D')
    || (x >= '\x37F' && x <= '\x1FFF')
    || (x >= '\x200C' && x <= '\x200D')
    || (x >= '\x203F' && x <= '\x2040')
    || (x >= '\x2070' && x <= '\x218F')
    || (x >= '\x2C00' && x <= '\x2FEF')
    || (x >= '\x3001' && x <= '\xD7FF')
    || (x >= '\xF900' && x <= '\xFDCF')
    || (x >= '\xFDF0' && x <= '\xFFFD')
    || (x >= '\x10000' && x <= '\xEFFFF')

-- | name string according to xml production rule [5]
nameXml :: Parser e ByteString
nameXml = byteStringOf (nameStartChar >> many nameChar)

-- | XML declaration as per production rule [23]
declXml :: Parser e Token
declXml =
  Decl
    <$> byteStringOf
      ( $(string "xml")
          >> xmlVersionInfo
          >> optional xmlEncodingDecl
          >> optional xmlStandalone
          >> ws_
      )
    <* $(string "?>")

-- | xml production [24]
xmlVersionInfo :: Parser e ByteString
xmlVersionInfo = byteStringOf $ ws_ >> $(string "version") >> eq >> wrappedQNoGuard xmlVersionNum

-- | xml production [26]
xmlVersionNum :: Parser e ByteString
xmlVersionNum =
  byteStringOf ($(string "1.") >> some (satisfy isDigit))

-- | Doctype declaration as per production rule [28]
doctypeXml :: Parser e Token
doctypeXml =
  Doctype
    <$> byteStringOf
      ( $(string "DOCTYPE")
          >> ws_
          >> nameXml
          >>
          -- optional (ws_ >> xmlExternalID) >>
          ws_
          >> optional bracketedSB
          >> ws_
      )
    <* $(char '>')

-- | Xml production [32]
xmlStandalone :: Parser e ByteString
xmlStandalone =
  byteStringOf $
    ws_ *> $(string "standalone") *> eq *> xmlYesNo

-- | Xml yes/no
xmlYesNo :: Parser e ByteString
xmlYesNo = wrappedQNoGuard (byteStringOf $ $(string "yes") <|> $(string "no"))

-- | xml production [80]
xmlEncodingDecl :: Parser e ByteString
xmlEncodingDecl = ws_ *> $(string "encoding") *> eq *> wrappedQNoGuard xmlEncName

-- | xml production [81]
xmlEncName :: Parser e ByteString
xmlEncName = byteStringOf (satisfyAscii isLatinLetter >> many (satisfyAscii (\x -> isLatinLetter x || isDigit x || elem x ("._-" :: [Char]))))

-- | open xml tag as per xml production rule [40]
--  self-closing xml tag as per [44]
startTagsXml :: Parser e Token
startTagsXml = do
  !n <- nameXml
  !as <- many (ws_ *> attrXml)
  _ <- ws_
  $( switch
       [|
         case _ of
           "/>" -> pure (EmptyElemTag n as)
           ">" -> pure (StartTag n as)
         |]
   )

attrXml :: Parser e Attr
attrXml = Attr <$> (nameXml <* eq) <*> wrappedQ

-- | closing tag as per [42]
endTagXml :: Parser e Token
endTagXml = EndTag <$> (nameXml <* ws_ <* $(char '>'))

-- | Parse a single 'Token'.
tokenHtml :: Parser e Token
tokenHtml =
  $( switch
       [|
         case _ of
           "<!--" -> comment
           "<!" -> doctypeHtml
           "</" -> endTagHtml
           "<?" -> bogusCommentHtml
           "<" -> startTagsHtml
           _ -> content
         |]
   )

bogusCommentHtml :: Parser e Token
bogusCommentHtml = Comment <$> byteStringOf (some (satisfy (/= '<')))

doctypeHtml :: Parser e Token
doctypeHtml =
  Doctype
    <$> byteStringOf
      ( $(string "DOCTYPE")
          >> ws_
          >> nameHtml
          >> ws_
      )
    <* $(char '>')

startTagsHtml :: Parser e Token
startTagsHtml = do
  n <- nameHtml
  as <- attrs Html
  _ <- ws_
  $( switch
       [|
         case _ of
           "/>" -> pure (EmptyElemTag n as)
           ">" -> pure (StartTag n as)
         |]
   )

endTagHtml :: Parser e Token
endTagHtml = EndTag <$> nameHtml <* ws_ <* $(char '>')

-- | Parse a tag name. Each standard is slightly different.
name :: Standard -> Parser e ByteString
name Html = nameHtml
name Xml = nameXml

nameHtml :: Parser e ByteString
nameHtml = do
  byteStringOf (nameStartCharHtml >> many (satisfy isNameChar))

nameStartCharHtml :: Parser e Char
nameStartCharHtml = satisfyAscii isLatinLetter

isNameChar :: Char -> Bool
isNameChar x =
  not
    ( isWhitespace x
        || (x == '/')
        || (x == '<')
        || (x == '>')
    )

attrHtml :: Parser e Attr
attrHtml =
  (Attr <$> (attrName <* eq) <*> (wrappedQ <|> attrBooleanName))
    <|> ((`Attr` mempty) <$> attrBooleanName)

attrBooleanName :: Parser e ByteString
attrBooleanName = byteStringOf $ some (satisfy isBooleanAttrName)

-- | Parse an 'Attr'
attr :: Standard -> Parser a Attr
attr Html = attrHtml
attr Xml = attrXml

-- | Parse attributions
attrs :: Standard -> Parser a [Attr]
attrs s = many (ws_ *> attr s) <* ws_

attrName :: Parser e ByteString
attrName = isa isAttrName

isAttrName :: Char -> Bool
isAttrName x =
  not $
    isWhitespace x
      || (x == '/')
      || (x == '>')
      || (x == '=')

isBooleanAttrName :: Char -> Bool
isBooleanAttrName x =
  not $
    isWhitespace x
      || (x == '/')
      || (x == '>')
