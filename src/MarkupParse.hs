{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}

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
    normContent,
    wellFormed,
    isWellFormed,

    -- * Warnings
    MarkupWarning (..),
    Warn,
    warnError,
    warnEither,
    warnMaybe,

    -- * element creation
    element,
    element_,
    elementc,
    content,
    contentRaw,

    -- * Token components
    TagName,
    selfClosers,
    tag,
    addAttrs,
    doctypeHtml,
    doctypeXml,
    AttrName,
    AttrValue,
    Attr (..),
    attrsP,
    nameP,

    -- * Tokens
    Token (..),
    tokenize,
    escapeChar,
    escape,
    tokenize_,
    tokenP,
    detokenize,
    gather,
    gather_,
    degather,
    degather_,

    -- * XML specific Parsers
    xmlVersionInfoP,
    xmlEncodingDeclP,
    xmlStandaloneP,
    xmlVersionNumP,
    xmlEncNameP,
    xmlYesNoP,

    -- * bytestring support
    utf8ToStr,
    strToUtf8,

    -- * Tree support
    Tree (..),
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
-- That (Markup {ttree = [Node {rootLabel = StartTag "foo" [Attr {attrName = "class", attrValue = "bar"}], subForest = [Node {rootLabel = Content "baz", subForest = []}]}]})
newtype Markup = Markup { ttree :: [Tree Token]}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, ToExpr)
  deriving newtype (Semigroup, Monoid)

-- | Escape a single character.
escapeChar :: Char -> ByteString
escapeChar '<' = "&lt"
escapeChar '>' = "&gt"
escapeChar '&' = "&amp"
escapeChar '\'' = "&apos"
escapeChar '"' = "&quot"
escapeChar x = B.singleton x

-- | Escape Content
--
-- >>> escape [i|<foo class="a" bar='b'>|]
-- "&ltfoo class=&quota&quot bar=&aposb&apos&gt"
escape :: ByteString -> ByteString
escape bs = B.concatMap escapeChar bs

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
  | -- | Badly formed declaration
    BadDecl
  | MarkupParser ParserWarning
  deriving (Eq, Show, Ord, Generic, NFData)

showWarnings :: [MarkupWarning] -> String
showWarnings = List.nub >>> fmap show >>> unlines

-- | A synonym for These [MarkupWarning] which is the structure of some functions.
--
-- A common computation pipeline is to take advantage of the 'These' Monad instance eg
--
-- > markup s bs = bs & (tokenize s >=> gather s) & second (Markup s)
type Warn a = These [MarkupWarning] a

-- | Convert any warnings to an 'error'
--
-- >>> warnError $ (tokenize Html) "<foo"
-- *** Exception: MarkupParser (ParserLeftover "<foo")
-- ...
warnError :: Warn a -> a
warnError = these (showWarnings >>> error) id (\xs a -> bool (error (showWarnings xs)) a (xs == []))

-- | Returns Left on any warnings
--
-- >>> warnEither $ (tokenize Html) "<foo><baz"
-- Left [MarkupParser (ParserLeftover "<baz")]
warnEither :: Warn a -> Either [MarkupWarning] a
warnEither = these Left Right (\xs a -> bool (Left xs) (Right a) (xs == []))

-- | Returns results, if any, ignoring warnings.
--
-- >>> warnMaybe $ (tokenize Html) "<foo><baz"
-- Just [StartTag "foo" []]
warnMaybe :: Warn a -> Maybe a
warnMaybe = these (const Nothing) Just (\_ a -> Just a)

-- | Convert bytestrings to 'Markup'
--
-- >>> markup Html "<foo><br></foo><baz"
-- These [MarkupParser (ParserLeftover "<baz")] (Markup {ttree = [Node {rootLabel = StartTag "foo" [], subForest = [Node {rootLabel = StartTag "br" [], subForest = []}]}]})
markup :: Standard -> ByteString -> Warn Markup
markup s bs = bs & (tokenize s >=> gather s) & second Markup

-- | markup but errors on warnings.
markup_ :: Standard -> ByteString -> Markup
markup_ s bs = markup s bs & warnError

-- | concatenate sequential content, and normalize attributes; unwording class values and removing duplicate attributes (taking last).
--
-- >>> B.putStr $ markdown Xml Compact $ normalize (markup_ Xml [i|<foo class="a" class="b" bar="first" bar="last"/>|])
-- <foo bar="last" class="a b"/>
normalize :: Markup -> Markup
normalize m = normContent $ Markup $ fmap (fmap normTokenAttrs) (ttree m)

-- | Are the trees in the markup well-formed?
isWellFormed :: Standard -> Markup -> Bool
isWellFormed s = (== []) . wellFormed s

-- | Check for well-formedness and rerturn warnings encountered.
--
-- >>> wellFormed Html $ Markup [Node (Comment "") [], Node (EndTag "foo") [], Node (EmptyElemTag "foo" []) [Node (Content "bar") []], Node (EmptyElemTag "foo" []) []]
-- [EmptyContent,EndTagInTree,LeafWithChildren,BadEmptyElemTag]
wellFormed :: Standard -> Markup -> [MarkupWarning]
wellFormed s (Markup trees) = List.nub $ mconcat (foldTree checkNode <$> trees)
  where
    checkNode (StartTag _ _) xs = mconcat xs
    checkNode (EmptyElemTag n _) [] =
      bool [] [BadEmptyElemTag] (not (n `elem` selfClosers) && s == Html)
    checkNode (EndTag _) [] = [EndTagInTree]
    checkNode (Content bs) [] = bool [] [EmptyContent] (bs == "")
    checkNode (Comment bs) [] = bool [] [EmptyContent] (bs == "")
    checkNode (Decl bs as) []
      | bs == "" = [EmptyContent]
      | s == Html && as /= [] = [BadDecl]
      | s == Xml && ("version" `elem` (attrName <$> as)) && ("encoding" `elem` (attrName <$> as))
        = [BadDecl]
      | otherwise = []
    checkNode (Doctype bs) [] = bool [] [EmptyContent] (bs == "")
    checkNode _ _ = [LeafWithChildren]

-- | Name of token
type TagName = ByteString

-- | A Markup token
--
-- >>> runParser_ (many (tokenP Html)) [i|<foo>content</foo>|]
-- [StartTag "foo" [],Content "content",EndTag "foo"]
--
-- >>> runParser_ (tokenP Xml) [i|<foo/>|]
-- EmptyElemTag "foo" []
--
-- >>> runParser_ (tokenP Html) "<!-- Comment -->"
-- Comment " Comment "
--
-- >>> runParser_ (tokenP Xml) [i|<?xml version="1.0" encoding="UTF-8"?>|]
-- Decl "xml" [Attr {attrName = "version", attrValue = " version=\"1.0\""},Attr {attrName = "encoding", attrValue = "UTF-8"}]
--
-- >>> runParser_ (tokenP Html) "<!DOCTYPE html>"
-- Doctype "DOCTYPE html"
--
-- >>> runParser_ (tokenP Xml) "<!DOCTYPE foo [ declarations ]>"
-- Doctype "DOCTYPE foo [ declarations ]"
--
-- >>> runParser (tokenP Html) [i|<foo a="a" b="b" c=c check>|]
-- OK (StartTag "foo" [Attr {attrName = "a", attrValue = "a"},Attr {attrName = "b", attrValue = "b"},Attr {attrName = "c", attrValue = "c"},Attr {attrName = "check", attrValue = ""}]) ""
--
-- >>> runParser (tokenP Xml) [i|<foo a="a" b="b" c=c check>|]
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
    Decl !ByteString ![Attr]
  | -- | Contents of a doctype declaration.
    Doctype !ByteString
  deriving (Show, Ord, Eq, Generic)

instance NFData Token

instance ToExpr Token


-- | Append attributes to the existing Token attribute list.
addAttrs :: [Attr] -> Token -> Maybe Token
addAttrs as (StartTag n as') = Just $ StartTag n (as' <> as)
addAttrs as (EmptyElemTag n as') = Just $ EmptyElemTag n (as' <> as)
addAttrs _ _ = Nothing

-- | Standard Html Doctype
--
-- >>> markdown Html Compact doctypeHtml
-- "<!DOCTYPE html>"
doctypeHtml :: Markup
doctypeHtml = Markup $ pure $ pure (Doctype "DOCTYPE html")

-- | Standard Xml Doctype
--
-- >>> markdown Xml Compact doctypeXml
-- "<?xml version=\"1.0\" encoding=\"utf-8\"?><!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
doctypeXml :: Markup
doctypeXml = Markup [ pure $ Decl "xml" [Attr "version" "1.0", Attr "encoding" "utf-8"],
               pure $ Doctype "DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\""]

-- | A flatparse 'Token' parser.
--
-- >>> runParser (tokenP Html) "<foo>content</foo>"
-- OK (StartTag "foo" []) "content</foo>"
tokenP :: Standard -> Parser e Token
tokenP Html = tokenHtmlP
tokenP Xml = tokenXmlP

-- | Parse a bytestring into tokens
--
-- >>> tokenize Html [i|<foo>content</foo>|]
-- That [StartTag "foo" [],Content "content",EndTag "foo"]
tokenize :: Standard -> ByteString -> Warn [Token]
tokenize s bs = first ((: []) . MarkupParser) $ runParserWarn (many (tokenP s)) bs

-- | tokenize but errors on warnings.
tokenize_ :: Standard -> ByteString -> [Token]
tokenize_ s bs = tokenize s bs & warnError

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

-- | Create a Token from a name and attribute list
--
-- >>> tag "br" []
-- EmptyElemTag "br" []
--
-- >>> tag "a" []
-- StartTag "a" []
tag :: TagName -> [Attr] -> Token
tag n as = bool (StartTag n as) (EmptyElemTag n as) (n `elem` selfClosers)

-- | Create a Markup element from a TagName and attributes that wraps some other Markup.
--
-- >>> element "div" [] (element_ "br" [])
-- Markup {ttree = [Node {rootLabel = StartTag "div" [], subForest = [Node {rootLabel = EmptyElemTag "br" [], subForest = []}]}]}
element :: TagName -> [Attr] -> Markup -> Markup
element n as (Markup xs) = Markup [Node (StartTag n as) xs]

-- | Create a Markup element from a TagName and attributes that wraps some other Markup.
--
-- >>> element_ "div" []
-- Markup {ttree = [Node {rootLabel = StartTag "div" [], subForest = []}]}
element_ :: TagName -> [Attr] -> Markup
element_ n as = Markup [Node (tag n as) []]

-- | Create a Markup element from a TagName and attributes that wraps some 'Content'.
--
-- >>> elementc "div" [] "content"
-- Markup {ttree = [Node {rootLabel = StartTag "div" [], subForest = [Node {rootLabel = Content "content", subForest = []}]}]}
elementc :: TagName -> [Attr] -> ByteString -> Markup
elementc n as bs = element n as (content bs)

-- | Create a Markup element from a bytestring, escaping the usual characters.
--
-- >>> content "<content>"
-- Markup {ttree = [Node {rootLabel = Content "&ltcontent&gt", subForest = []}]}
content :: ByteString -> Markup
content bs = Markup [pure $ Content (escape bs)]

-- | Create a Markup element from a bytestring, not escaping the usual characters.
--
-- >>> contentRaw "<content>"
-- Markup {ttree = [Node {rootLabel = Content "<content>", subForest = []}]}
--
-- >>> markup_ Html $ markdown Html Compact $ contentRaw "<content>"
-- *** Exception: UnclosedTag
-- ...
contentRaw :: ByteString -> Markup
contentRaw bs = Markup [pure $ Content bs]

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
data Attr = Attr { attrName :: !AttrName, attrValue :: !AttrValue }
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
  (Decl t as) -> bool [i|<?#{t} #{renderAttrs as}?>|] [i|<!#{t}!>|] (s == Html)

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
-- >>> B.putStr $ markdown Html (Indented 4) (markup_ Html [i|<foo><br></foo>|])
-- <foo>
--     <br>
-- </foo>
markdown :: Standard -> RenderStyle -> Markup -> ByteString
markdown std r m =
  finalConcat r $ mconcat $ foldTree (renderBranch r std) <$> (ttree $ normContent m)

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

-- | Normalise Content in Markup, concatenating adjacent Content, and removing mempty Content.
--
-- >>> normContent $ content "a" <> content "" <> content "b"
-- Markup {ttree = [Node {rootLabel = Content "ab", subForest = []}]}
normContent :: Markup -> Markup
normContent (Markup trees) = Markup $ foldTree (\x xs -> Node x (filter ((/= Content "") . rootLabel) $ concatContent xs)) <$> concatContent trees

concatContent :: [Tree Token] -> [Tree Token]
concatContent = \case
  ((Node (Content t) _) : (Node (Content t') _) : ts) -> concatContent $ Node (Content (t <> t')) [] : ts
  (t : ts) -> t : concatContent ts
  [] -> []

-- | Gather together token trees from a token list, placing child elements in nodes and removing EndTags.
--
-- >>> gather Html =<< tokenize Html "<foo class=\"bar\">baz</foo>"
-- That [Node {rootLabel = StartTag "foo" [Attr {attrName = "class", attrValue = "bar"}], subForest = [Node {rootLabel = Content "baz", subForest = []}]}]
gather :: Standard -> [Token] -> Warn [Tree Token]
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
gather_ s ts = gather s ts & warnError

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
-- >>> degather Html =<< markup Html "<foo class=\"bar\">baz</foo>"
-- That [StartTag "foo" [Attr {attrName = "class", attrValue = "bar"}],Content "baz",EndTag "foo"]
degather :: Standard -> Markup -> Warn [Token]
degather s (Markup tree) = rconcats $ foldTree (addCloseTags s) <$> tree

-- | degather but errors on warning
degather_ :: Standard -> Markup -> [Token]
degather_ s m = degather s m & warnError

rconcats :: [Warn [a]] -> Warn [a]
rconcats rs = case bimap mconcat mconcat $ partitionHereThere rs of
  ([], xs) -> That xs
  (es, []) -> This es
  (es, xs) -> These es xs

addCloseTags :: Standard -> Token -> [Warn [Token]] -> Warn [Token]
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

tokenXmlP:: Parser e Token
tokenXmlP=
  $( switch
       [|
         case _ of
           "<!--" -> commentP
           "<!" -> doctypeXmlP
           "</" -> endTagXmlP
           "<?" -> declXmlP
           "<" -> startTagsXmlP
           _ -> contentP
         |]
   )

-- [4]
nameStartCharP:: Parser e Char
nameStartCharP = fusedSatisfy isLatinLetter isNameStartChar isNameStartChar isNameStartChar

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
nameCharP :: Parser e Char
nameCharP= fusedSatisfy isNameCharAscii isNameCharExt isNameCharExt isNameCharExt

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
nameXmlP :: Parser e ByteString
nameXmlP = byteStringOf (nameStartCharP >> many nameCharP)

commentCloseP :: Parser e ()
commentCloseP = $(string "-->")

charNotMinusP :: Parser e ByteString
charNotMinusP = byteStringOf $ satisfy (/= '-')

minusPlusCharP :: Parser e ByteString
minusPlusCharP = byteStringOf ($(char '-') *> charNotMinusP)

commentP :: Parser e Token
commentP = Comment <$> byteStringOf (many (charNotMinusP <|> minusPlusCharP)) <* commentCloseP

contentP :: Parser e Token
contentP = Content <$> byteStringOf (some (satisfy (/= '<')))

-- | XML declaration as per production rule [23]
declXmlP :: Parser e Token
declXmlP = do
  _ <- $(string "xml")
  av <- Attr "version" <$> xmlVersionInfoP
  en <- Attr "encoding" <$> xmlEncodingDeclP
  st <- optional $ Attr "standalone" <$> xmlStandaloneP
  _ <- ws_
  _ <- $(string "?>")
  pure $ Decl "xml" $ [av, en] <> (maybe [] (:[]) st)

-- | xml production [24]
xmlVersionInfoP :: Parser e ByteString
xmlVersionInfoP = byteStringOf $ ws_ >> $(string "version") >> eq >> wrappedQNoGuard xmlVersionNumP

-- | xml production [26]
xmlVersionNumP :: Parser e ByteString
xmlVersionNumP =
  byteStringOf ($(string "1.") >> some (satisfy isDigit))

-- | Doctype declaration as per production rule [28]
doctypeXmlP :: Parser e Token
doctypeXmlP =
  Doctype
    <$> byteStringOf
      ( $(string "DOCTYPE")
          >> ws_
          >> nameXmlP
          >>
          -- optional (ws_ >> xmlExternalID) >>
          ws_
          >> optional bracketedSB
          >> ws_
      )
    <* $(char '>')

-- | Xml production [32]
xmlStandaloneP :: Parser e ByteString
xmlStandaloneP =
  byteStringOf $
    ws_ *> $(string "standalone") *> eq *> xmlYesNoP

-- | Xml yes/no
xmlYesNoP :: Parser e ByteString
xmlYesNoP = wrappedQNoGuard (byteStringOf $ $(string "yes") <|> $(string "no"))

-- | xml production [80]
xmlEncodingDeclP :: Parser e ByteString
xmlEncodingDeclP = ws_ *> $(string "encoding") *> eq *> wrappedQNoGuard xmlEncNameP

-- | xml production [81]
xmlEncNameP :: Parser e ByteString
xmlEncNameP = byteStringOf (satisfyAscii isLatinLetter >> many (satisfyAscii (\x -> isLatinLetter x || isDigit x || elem x ("._-" :: [Char]))))

-- | open xml tag as per xml production rule [40]
--  self-closing xml tag as per [44]
startTagsXmlP :: Parser e Token
startTagsXmlP = do
  !n <- nameXmlP
  !as <- many (ws_ *> attrXmlP)
  _ <- ws_
  $( switch
       [|
         case _ of
           "/>" -> pure (EmptyElemTag n as)
           ">" -> pure (StartTag n as)
         |]
   )

attrXmlP :: Parser e Attr
attrXmlP = Attr <$> (nameXmlP <* eq) <*> wrappedQ

-- | closing tag as per [42]
endTagXmlP :: Parser e Token
endTagXmlP = EndTag <$> (nameXmlP <* ws_ <* $(char '>'))

-- | Parse a single 'Token'.
tokenHtmlP :: Parser e Token
tokenHtmlP =
  $( switch
       [|
         case _ of
           "<!--" -> commentP
           "<!" -> doctypeHtmlP
           "</" -> endTagHtmlP
           "<?" -> bogusCommentHtmlP
           "<" -> startTagsHtmlP
           _ -> contentP
         |]
   )

bogusCommentHtmlP :: Parser e Token
bogusCommentHtmlP = Comment <$> byteStringOf (some (satisfy (/= '<')))

doctypeHtmlP :: Parser e Token
doctypeHtmlP =
  Doctype
    <$> byteStringOf
      ( $(string "DOCTYPE")
          >> ws_
          >> nameHtmlP
          >> ws_
      )
    <* $(char '>')

startTagsHtmlP :: Parser e Token
startTagsHtmlP = do
  n <- nameHtmlP
  as <- attrsP Html
  _ <- ws_
  $( switch
       [|
         case _ of
           "/>" -> pure (EmptyElemTag n as)
           ">" -> pure (StartTag n as)
         |]
   )

endTagHtmlP :: Parser e Token
endTagHtmlP = EndTag <$> nameHtmlP <* ws_ <* $(char '>')

-- | Parse a tag name. Each standard is slightly different.
nameP :: Standard -> Parser e ByteString
nameP Html = nameHtmlP
nameP Xml = nameXmlP

nameHtmlP :: Parser e ByteString
nameHtmlP = do
  byteStringOf (nameStartCharHtmlP >> many (satisfy isNameChar))

nameStartCharHtmlP :: Parser e Char
nameStartCharHtmlP = satisfyAscii isLatinLetter

isNameChar :: Char -> Bool
isNameChar x =
  not
    ( isWhitespace x
        || (x == '/')
        || (x == '<')
        || (x == '>')
    )

attrHtmlP :: Parser e Attr
attrHtmlP =
  (Attr <$> (attrNameP <* eq) <*> (wrappedQ <|> attrBooleanNameP))
    <|> ((`Attr` mempty) <$> attrBooleanNameP)

attrBooleanNameP :: Parser e ByteString
attrBooleanNameP = byteStringOf $ some (satisfy isBooleanAttrName)

-- | Parse an 'Attr'
attrP :: Standard -> Parser a Attr
attrP Html = attrHtmlP
attrP Xml = attrXmlP

-- | Parse attributions
attrsP :: Standard -> Parser a [Attr]
attrsP s = many (ws_ *> attrP s) <* ws_

attrNameP :: Parser e ByteString
attrNameP = isa isAttrName

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
