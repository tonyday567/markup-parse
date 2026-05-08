{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A 'Markup' parser and printer of strict 'ByteString's focused on optimising performance. 'Markup' is a representation of data such as HTML, SVG or XML but the parsing is not always at standards.
module MarkupParse
  ( -- * Usage

    --
    -- $usage

    -- * Markup
    Markup (..),
    Standard (..),
    markup,
    markup_,
    RenderStyle (..),
    markdown,
    markdown_,
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

    -- * Element
    Element,
    element,
    element_,
    emptyElem,
    elementc,
    content,
    contentRaw,

    -- * Token components
    NameTag,
    selfClosers,
    doctypeHtml,
    doctypeXml,
    AttrName,
    AttrValue,
    Attr (..),
    addAttrs,
    attrsP,
    nameP,

    -- * Tokens
    OpenTagType (..),
    Token (..),
    tokenize,
    tokenize_,
    tokenP,
    detokenize,
    gather,
    gather_,
    degather,
    degather_,

    -- * bytestring support
    escapeChar,
    escape,

    -- * Tree support
    Tree (..),

    -- * token parsing support
    ParserWarning (..),
    runParserWarn,
    runParser_,
    runParser,

    -- * parsing
    Parser,
  )
where

import Control.Applicative hiding (many, some, (<|>))
import Control.Category ((>>>))
import Control.DeepSeq
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char
import Data.Data
import Data.Foldable
import Data.Function
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.These
import Data.Tree
import GHC.Generics
import Circuit.Parser
  ( Parser, These (..), satisfy, char, string, many, some, (<|>), empty
  , captured, skipWhile )
import qualified Circuit.Parser as CP
import Prelude hiding (replicate)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XOverloadedStrings
-- >>> import MarkupParse
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
-- @'markdown_' . 'markup_'@ is an isomorphic round trip from 'Markup' to 'ByteString' to 'Markup':
--
-- - This is subject to the 'Markup' being 'wellFormed'.
--
-- - The round-trip @'markup_' . 'markdown_'@ is not isomorphic as parsing forgets whitespace within tags, comments and declarations.
--
-- - The underscores represent versions of main functions that throw an exception on warnings encountered along the way.
--
-- At a lower level, a round trip pipeline might look something like:
--
-- > tokenize Html >=>
--
-- - 'tokenize' converts a 'ByteString' to a 'Token' list.
--
-- > gather Html >=>
--
-- - 'gather' takes the tokens and gathers them into 'Tree's of 'Token's which is what 'Markup' is.
--
-- > (normalize >>> pure) >=>
--
-- - 'normalize' concatenates content, and normalizes attributes,
--
-- > degather Html >=>
--
-- - 'degather' turns the markup tree back into a token list. Finally,
--
-- > fmap (detokenize Html) >>> pure
--
-- - 'detokenize' turns a token back into a bytestring.
--
-- Along the way, the kleisi fishies and compose forward usage accumulates any warnings via the 'These' monad instance, which is wrapped into a type synonym named 'Warn'.

-- | From a parsing pov, Html & Xml (& Svg) are close enough that they share a lot of parsing logic, so that parsing and printing just need some tweaking.
--
-- The xml parsing logic is based on the XML productions found in https://www.w3.org/TR/xml/
--
-- The html parsing was based on a reading of <https://hackage.haskell.org/package/html-parse html-parse>, but ignores the various '\x00' to '\xfffd' & eof directives that form part of the html standards.
data Standard = Html | Xml
  deriving stock (Eq, Ord, Show, Generic, Data)

instance NFData Standard

-- | A list of 'Element's or 'Tree' 'Token's
--
-- >>> markup Html "<foo class=\"bar\">baz</foo>"
-- That (Markup {elements = [Node {rootLabel = OpenTag StartTag "foo" [Attr {attrName = "class", attrValue = "bar"}], subForest = [Node {rootLabel = Content "baz", subForest = []}]}]})
newtype Markup = Markup {elements :: [Element]}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving newtype (Semigroup, Monoid)

instance NFData Markup

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

showWarnings :: [MarkupWarning] -> String
showWarnings = List.nub >>> fmap show >>> unlines

-- | A type synonym for the common returning type of many functions. A common computation pipeline is to take advantage of the 'These' Monad instance eg
--
-- > markup s bs = bs & (tokenize s >=> gather s) & second (Markup s)
type Warn a = These [MarkupWarning] a

-- | TokenParser: semantic phase parser operating on token streams
--
-- State-threading parser over token lists with error/warning accumulation.
-- Replaces the mpar StateThreader (which was in the removed FlatParse module).
newtype TokenParser e a = TokenParser { runTP :: [Token] -> ([Token], These e a) }

runTP' :: TokenParser e a -> [Token] -> ([Token], These e a)
runTP' = runTP

-- | A single-quoted or double-quoted wrapped parser (no guard check).
wrappedQNoGuard :: Parser String Char a -> Parser String Char a
wrappedQNoGuard p = (char '"' *> p <* char '"') <|> (char '\'' *> p <* char '\'')

-- | Parser bracketed by square brackets.
bracketedSB :: Parser String Char String
bracketedSB = char '[' *> many (satisfy (/= ']')) <* char ']'

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
-- Just [OpenTag StartTag "foo" []]
warnMaybe :: Warn a -> Maybe a
warnMaybe = these (const Nothing) Just (\_ a -> Just a)

-- | Convert bytestrings to 'Markup'
--
-- Two-phase pipeline: lexical (tokenize) then semantic (gather)
--
-- >>> markup Html "<foo><br></foo><baz"
-- These [MarkupParser (ParserLeftover "<baz")] (Markup {elements = [Node {rootLabel = OpenTag StartTag "foo" [], subForest = [Node {rootLabel = OpenTag StartTag "br" [], subForest = []}]}]})
markup :: Standard -> ByteString -> Warn Markup
markup s bs = bs & (tokenize s >=> gatherTokens s)

-- | 'markup' but errors on warnings.
markup_ :: Standard -> ByteString -> Markup
markup_ s bs = markup s bs & warnError

-- | Concatenate sequential content and normalize attributes; unwording class values and removing duplicate attributes (taking last).
--
-- >>> B.putStr $ warnError $ markdown Compact Xml $ normalize (markup_ Xml "<foo class=\"a\" class=\"b\" bar=\"first\" bar=\"last\"/>")
-- <foo bar="last" class="a b"/>
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
      bool [] [BadEmptyElemTag] (not (n `elem` selfClosers) && s == Html)
    checkNode (EndTag _) [] = [EndTagInTree]
    checkNode (Content bs) [] = bool [] [EmptyContent] (bs == "")
    checkNode (Comment bs) [] = bool [] [EmptyContent] (bs == "")
    checkNode (Decl bs as) []
      | bs == "" = [EmptyContent]
      | s == Html && as /= [] = [BadDecl]
      | s == Xml && ("version" `elem` (attrName <$> as)) && ("encoding" `elem` (attrName <$> as)) =
          [BadDecl]
      | otherwise = []
    checkNode (Doctype bs) [] = bool [] [EmptyContent] (bs == "")
    checkNode _ _ = [LeafWithChildren]

-- | Name of token
type NameTag = ByteString

-- | Whether an opening tag is a start tag or an empty element tag.
data OpenTagType = StartTag | EmptyElemTag
  deriving (Eq, Ord, Show, Generic, Data)

instance NFData OpenTagType

-- | A Markup token. The term is borrowed from <https://www.w3.org/html/wg/spec/tokenization.html#tokenization HTML> standards but is used across 'Html' and 'Xml' in this library.
--
-- Note that the 'Token' type is used in two slightly different contexts:
--
-- - As an intermediary representation of markup between 'ByteString' and 'Markup'.
--
-- - As the primitives of 'Markup' 'Element's
--
-- Specifically, an 'EndTag' will occur in a list of tokens, but not as a primitive in 'Markup'. It may turn out to be better to have two different types for these two uses and future iterations of this library may head in this direction.
--
-- >>> runParser_ (many (tokenP Html)) "<foo>content</foo>"
-- [OpenTag StartTag "foo" [],Content "content",EndTag "foo"]
--
-- >>> runParser_ (tokenP Xml) "<foo/>"
-- OpenTag EmptyElemTag "foo" []
--
-- >>> runParser_ (tokenP Html) "<!-- Comment -->"
-- Comment " Comment "
--
-- >>> runParser_ (tokenP Xml) "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
-- Decl "xml" [Attr {attrName = "version", attrValue = " version=\"1.0\""},Attr {attrName = "encoding", attrValue = "UTF-8"}]
--
-- >>> runParser_ (tokenP Html) "<!DOCTYPE html>"
-- Doctype "DOCTYPE html"
--
-- >>> runParser_ (tokenP Xml) "<!DOCTYPE foo [ declarations ]>"
-- Doctype "DOCTYPE foo [ declarations ]"
--
-- >>> runParser (tokenP Html) "<foo a=\"a\" b=\"b\" c=c check>"
-- OK (OpenTag StartTag "foo" [Attr {attrName = "a", attrValue = "a"},Attr {attrName = "b", attrValue = "b"},Attr {attrName = "c", attrValue = "c"},Attr {attrName = "check", attrValue = ""}]) ""
--
-- >>> runParser (tokenP Xml) "<foo a=\"a\" b=\"b\" c=c check>"
-- Fail
data Token
  = -- | A tag. https://developer.mozilla.org/en-US/docs/Glossary/Tag
    OpenTag !OpenTagType !NameTag ![Attr]
  | -- | A closing tag.
    EndTag !NameTag
  | -- | The content between tags.
    Content !ByteString
  | -- | Contents of a comment.
    Comment !ByteString
  | -- | Contents of a declaration
    Decl !ByteString ![Attr]
  | -- | Contents of a doctype declaration.
    Doctype !ByteString
  deriving (Eq, Ord, Show, Generic, Data)

instance NFData Token

-- | Escape a single character.
escapeChar :: Char -> ByteString
escapeChar '<' = "&lt;"
escapeChar '>' = "&gt;"
escapeChar '&' = "&amp;"
escapeChar '\'' = "&apos;"
escapeChar '"' = "&quot;"
escapeChar x = B.singleton x

-- | Escape the following predefined character entity references:
--
-- @
-- escapeChar \'<\' = "&lt;"
-- escapeChar \'>\' = "&gt;"
-- escapeChar \'&\' = "&amp;"
-- escapeChar '\'' = "&apos;"
-- escapeChar '"' = "&quot;"
-- @
--
-- No attempt is made to meet the <https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references HTML Standards>
--
-- >>> escape "<foo class=\"a\" bar='b'>"
-- "&lt;foo class=&quot;a&quot; bar=&apos;b&apos;&gt;"
escape :: ByteString -> ByteString
escape bs = B.concatMap escapeChar bs

-- | Append attributes to an existing Token attribute list. Returns Nothing for tokens that do not have attributes.
addAttrs :: [Attr] -> Token -> Maybe Token
addAttrs as (OpenTag t n as') = Just $ OpenTag t n (as <> as')
addAttrs _ _ = Nothing

-- | Standard Html Doctype
--
-- >>> markdown_ Compact Html doctypeHtml
-- "<!DOCTYPE html>"
doctypeHtml :: Markup
doctypeHtml = Markup $ pure $ pure (Doctype "DOCTYPE html")

-- | Standard Xml Doctype
--
-- >>> markdown_ Compact Xml doctypeXml
-- "<?xml version=\"1.0\" encoding=\"utf-8\"?><!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
doctypeXml :: Markup
doctypeXml =
  Markup
    [ pure $ Decl "xml" [Attr "version" "1.0", Attr "encoding" "utf-8"],
      pure $ Doctype "DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\""
    ]

-- | A 'Token' parser.
--
-- >>> runParser (tokenP Html) "<foo>content</foo>"
-- OK (OpenTag StartTag "foo" []) "content</foo>"
tokenP :: Standard -> Parser String Char Token
tokenP Html = tokenHtmlP
tokenP Xml = tokenXmlP

-- | Parse a bytestring into tokens
--
-- >>> tokenize Html "<foo>content</foo>"
-- That [OpenTag StartTag "foo" [],Content "content",EndTag "foo"]
tokenize :: Standard -> ByteString -> Warn [Token]
tokenize s bs = first ((: []) . MarkupParser) $ runParserWarn (many (tokenP s)) (B.unpack bs)

-- | tokenize but errors on warnings.
tokenize_ :: Standard -> ByteString -> [Token]
tokenize_ s bs = tokenize s bs & warnError

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

-- | Most functions return a 'Markup' rather than an 'Element' because it is often more ergonomic to use the free monoid (aka a list) in preference to returning a 'Maybe' 'Element' (say).
type Element = Tree Token

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
elementc n as bs = element n as (contentRaw bs)

-- | Create 'Markup' 'Content' from a bytestring, escaping the usual characters.
--
-- >>> content "<content>"
-- Markup {elements = [Node {rootLabel = Content "&lt;content&gt;", subForest = []}]}
content :: ByteString -> Markup
content bs = Markup [pure $ Content (escape bs)]

-- | Create a Markup element from a bytestring, not escaping the usual characters.
--
-- >>> contentRaw "<content>"
-- Markup {elements = [Node {rootLabel = Content "<content>", subForest = []}]}
--
-- >>> markup_ Html $ markdown_ Compact Html $ contentRaw "<content>"
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
-- >>> detokenize Html <$> tokenize_ Html "<input checked>"
-- ["<input checked=\"\">"]
data Attr = Attr {attrName :: !AttrName, attrValue :: !AttrValue}
  deriving (Eq, Ord, Show, Generic, Data)

instance NFData Attr

normTokenAttrs :: Token -> Token
normTokenAttrs (OpenTag t n as) = OpenTag t n (normAttrs as)
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
renderAttrs [] = mempty
renderAttrs xs = B.singleton ' ' <> (B.unwords . fmap renderAttr $ xs)

-- | render an attribute
--
-- Does not attempt to escape double quotes.
renderAttr :: Attr -> ByteString
renderAttr (Attr k v) = k <> "=\"" <> v <> "\""

-- | bytestring representation of 'Token'.
--
-- >>> detokenize Html (OpenTag StartTag "foo" [])
-- "<foo>"
detokenize :: Standard -> Token -> ByteString
detokenize s = \case
  (OpenTag StartTag n []) -> "<" <> n <> ">"
  (OpenTag StartTag n as) -> "<" <> n <> renderAttrs as <> ">"
  (OpenTag EmptyElemTag n as) ->
    bool
      ("<" <> n <> renderAttrs as <> "/>")
      ("<" <> n <> renderAttrs as <> " />")
      (s == Html)
  (EndTag n) -> "</" <> n <> ">"
  (Content t) -> t
  (Comment t) -> "<!--" <> t <> "-->"
  (Doctype t) -> "<!" <> t <> ">"
  (Decl t as) -> bool ("<?" <> t <> renderAttrs as <> "?>") ("<!" <> t <> "!>") (s == Html)

-- | @Indented 0@ puts newlines in between the tags.
data RenderStyle = Compact | Indented Int deriving (Eq, Ord, Show, Generic, Data)

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
-- >>> markdown (Indented 4) Html (markup_ Html "<foo><br></foo>")
-- That "<foo>\n    <br>\n</foo>"
markdown :: RenderStyle -> Standard -> Markup -> Warn ByteString
markdown r s m = second (finalConcat r) $ concatWarns $ foldTree (renderBranch r s) <$> (elements $ normContent m)

-- | Convert 'Markup' to 'ByteString' and error on warnings.
--
-- >>> B.putStr $ markdown_ (Indented 4) Html (markup_ Html "<foo><br></foo>")
-- <foo>
--     <br>
-- </foo>
markdown_ :: RenderStyle -> Standard -> Markup -> ByteString
markdown_ r s = markdown r s >>> warnError

-- note that renderBranch adds in EndTags for StartTags when needed
renderBranch :: RenderStyle -> Standard -> Token -> [Warn [ByteString]] -> Warn [ByteString]
renderBranch r std s@(OpenTag StartTag n _) xs
  | n `elem` selfClosers && std == Html =
      That [detokenize std s] <> second (indentChildren r) (concatWarns xs)
  | otherwise =
      That [detokenize std s] <> second (indentChildren r) (concatWarns xs) <> That [detokenize std (EndTag n)]
renderBranch _ std x [] =
  That [detokenize std x]
renderBranch r std x xs =
  These [LeafWithChildren] [detokenize std x] <> second (indentChildren r) (concatWarns xs)

-- | Normalise Content in Markup, concatenating adjacent Content, and removing mempty Content.
--
-- >>> normContent $ content "a" <> content "" <> content "b"
-- Markup {elements = [Node {rootLabel = Content "ab", subForest = []}]}
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
-- That (Markup {elements = [Node {rootLabel = OpenTag StartTag "foo" [Attr {attrName = "class", attrValue = "bar"}], subForest = [Node {rootLabel = Content "baz", subForest = []}]}]})
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
  ([], This ws) -> error (showWarnings ws)
  ([], These ws m) -> if ws == [] then m else error (showWarnings ws)
  _ -> error "Impossible: gather should consume all tokens"

-- | Wrapper for gather to work with Kleisli composition in markup pipeline
gatherTokens :: Standard -> [Token] -> Warn Markup
gatherTokens s ts = case runTP (gather s) ts of
  ([], result) -> result
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
--
-- >>> degather Html =<< markup Html "<foo class=\"bar\">baz</foo>"
-- That [OpenTag StartTag "foo" [Attr {attrName = "class", attrValue = "bar"}],Content "baz",EndTag "foo"]
degather :: Standard -> Markup -> Warn [Token]
degather s (Markup tree) = concatWarns $ foldTree (addCloseTags s) <$> tree

-- | 'degather' but errors on warning
degather_ :: Standard -> Markup -> [Token]
degather_ s m = degather s m & warnError

concatWarns :: [Warn [a]] -> Warn [a]
concatWarns rs = case bimap mconcat mconcat $ partitionHereThere rs of
  ([], xs) -> That xs
  (es, []) -> This es
  (es, xs) -> These es xs

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

-- ============================================================================
-- Character predicates (local, replacing mpar imports)
-- ============================================================================

isWhitespace :: Char -> Bool
isWhitespace ' '  = True
isWhitespace '\n' = True
isWhitespace '\t' = True
isWhitespace '\r' = True
isWhitespace _    = False

isLatinLetter :: Char -> Bool
isLatinLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

-- ============================================================================
-- Token parsers (Circuit.Parser, String-based)
-- ============================================================================

-- | capture consumed chars and convert to ByteString
bs :: Parser String Char a -> Parser String Char ByteString
bs p = B.pack . fst <$> captured p

-- | capture consumed list of items and convert first element to ByteString
bs1 :: Parser String Char [a] -> Parser String Char ByteString
bs1 p = B.pack . fst <$> captured (void p)

-- | equals sign with optional whitespace
eq_ :: Parser String Char ()
eq_ = skipWhile isWhitespace *> char '=' *> skipWhile isWhitespace

-- | quoted string: single or double quoted
wrappedQ :: Parser String Char ByteString
wrappedQ =
  (char '\'' *> bs (many (satisfy (/= '\''))) <* char '\'')
    <|> (char '"' *> bs (many (satisfy (/= '"'))) <* char '"')

tokenXmlP :: Parser String Char Token
tokenXmlP =
  (string "<!--" *> commentP_)
    <|> (string "<!" *> doctypeXmlP_)
    <|> (string "</" *> endTagXmlP_)
    <|> (string "<?" *> declXmlP_)
    <|> (string "<" *> startTagsXmlP_)
    <|> contentP_

tokenHtmlP :: Parser String Char Token
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
  isLatinLetter x || x == ':' || x == '_'
  || (x >= '\xC0' && x <= '\xD6')
  || (x >= '\xD8' && x <= '\xF6')
  || (x >= '\xF8' && x <= '\xFF')

-- XML/HMTL name char
isNameChar :: Char -> Bool
isNameChar x = not (isWhitespace x || x == '/' || x == '<' || x == '>')

isNameCharXml :: Char -> Bool
isNameCharXml x =
  isLatinLetter x || Data.Char.isDigit x || x `elem` (":_-.·" :: String)
  || (x >= '\xC0' && x <= '\xD6')
  || (x >= '\xD8' && x <= '\xF6')
  || (x >= '\xF8' && x <= '\xFF')

isAttrName :: Char -> Bool
isAttrName x = not (isWhitespace x || x == '/' || x == '>' || x == '=' || x == '<')

isBooleanAttrName :: Char -> Bool
isBooleanAttrName x = not (isWhitespace x || x == '/' || x == '>' || x == '<')

-- XML parsers

nameStartCharXmlP :: Parser String Char Char
nameStartCharXmlP = satisfy isNameStartChar

nameCharXmlP :: Parser String Char Char
nameCharXmlP = satisfy isNameCharXml

nameXmlP :: Parser String Char ByteString
nameXmlP = bs (nameStartCharXmlP *> many nameCharXmlP)

commentP_ :: Parser String Char Token
commentP_ = Comment <$> (bs (many (satisfy (/= '-') <|> (char '-' *> satisfy (/= '-')))) <* string "-->")

contentP_ :: Parser String Char Token
contentP_ = Content <$> bs (some (satisfy (/= '<')))

declXmlP_ :: Parser String Char Token
declXmlP_ =
  let attr key = Attr (B.pack key) <$> (skipWhile isWhitespace *> string key *> eq_ *> wrappedQ)
      one x = [x]
  in string "xml" *>
     (Decl "xml" <$> ((:) <$> attr "version" <*> (one <$> attr "encoding")))
       <* skipWhile isWhitespace <* string "?>"

doctypeXmlP_ :: Parser String Char Token
doctypeXmlP_ = Doctype <$> (bs (string "DOCTYPE" *> skipWhile isWhitespace *> void nameXmlP
  *> skipWhile isWhitespace *> many (satisfy (/= '>'))) <* char '>')

startTagsXmlP_ :: Parser String Char Token
startTagsXmlP_ =
  OpenTag EmptyElemTag <$> (nameXmlP <* skipWhile isWhitespace <* string "/>")
    <*> pure []
    <|> OpenTag StartTag <$> (nameXmlP <* skipWhile isWhitespace <* string ">")
    <*> many (skipWhile isWhitespace *> attrXmlP_)

attrXmlP_ :: Parser String Char Attr
attrXmlP_ = Attr <$> (nameXmlP <* eq_) <*> wrappedQ

endTagXmlP_ :: Parser String Char Token
endTagXmlP_ = EndTag <$> (nameXmlP <* skipWhile isWhitespace <* char '>')

-- HTML parsers

nameHtmlP :: Parser String Char ByteString
nameHtmlP = bs (satisfy isLatinLetter *> many (satisfy isNameChar))

startTagsHtmlP_ :: Parser String Char Token
startTagsHtmlP_ =
  OpenTag StartTag
    <$> (nameHtmlP <* skipWhile isWhitespace)
    <*> (attrsHtmlP_ <* skipWhile isWhitespace <* string ">")
    <|> OpenTag EmptyElemTag
    <$> (nameHtmlP <* skipWhile isWhitespace)
    <*> (attrsHtmlP_ <* skipWhile isWhitespace <* string "/>")

endTagHtmlP_ :: Parser String Char Token
endTagHtmlP_ = EndTag <$> (nameHtmlP <* skipWhile isWhitespace <* char '>')

attrHtmlP_ :: Parser String Char Attr
attrHtmlP_ =
  (Attr <$> (bs (many (satisfy isAttrName)) <* eq_) <*> (wrappedQ <|> bs (some (satisfy isBooleanAttrName))))
    <|> (flip Attr B.empty <$> bs (some (satisfy isBooleanAttrName)))

attrsHtmlP_ :: Parser String Char [Attr]
attrsHtmlP_ = many (skipWhile isWhitespace *> attrHtmlP_) <* skipWhile isWhitespace

doctypeHtmlP_ :: Parser String Char Token
doctypeHtmlP_ = Doctype <$> (bs (string "DOCTYPE" *> skipWhile isWhitespace *> void nameHtmlP
  *> skipWhile isWhitespace) <* char '>')

bogusCommentHtmlP_ :: Parser String Char Token
bogusCommentHtmlP_ = Comment <$> bs (some (satisfy (/= '<')))

-- | Parse a tag name.
nameP :: Standard -> Parser String Char ByteString
nameP Html = nameHtmlP
nameP Xml = nameXmlP

-- | Parse an attribute.
attrP :: Standard -> Parser String Char Attr
attrP Html = attrHtmlP_
attrP Xml = attrXmlP_

-- | Parse attributes list.
attrsP :: Standard -> Parser String Char [Attr]
attrsP Html = attrsHtmlP_
attrsP Xml = many (skipWhile isWhitespace *> attrXmlP_) <* skipWhile isWhitespace

-- | Alias for single whitespace (backward compat with mpar)
ws :: Parser String Char Char
ws = satisfy isWhitespace

-- | Alias for skip whitespace (backward compat with mpar)
ws_ :: Parser String Char ()
ws_ = skipWhile isWhitespace
--
-- >>> runParserWarn ws " x"
-- These (ParserLeftover "x") ' '
--
-- >>> runParserWarn ws "x"
-- This ParserUncaught
--
data ParserWarning
  = ParserLeftover String
  | ParserError String
  | ParserUncaught
  deriving (Eq, Ord, Show, Generic, Data)

instance NFData ParserWarning

-- | Run parser, returning leftovers and errors as 'ParserWarning's.
--
-- >>> runParserWarn ws " "
-- That ' '
--
-- >>> runParserWarn ws "x"
-- This ParserUncaught
--
-- >>> runParserWarn ws " x"
-- | Run parser, returning leftovers and errors as 'ParserWarning's.
runParserWarn :: Parser String Char a -> String -> These ParserWarning a
runParserWarn p s = case CP.runParser p s of
  These a "" -> That a
  These a rest -> These (ParserLeftover (take 200 rest)) a
  This a      -> That a
  That _      -> This ParserUncaught

-- | Run a parser and return the remaining input and result as a tuple
runParser :: Parser String Char a -> String -> (String, These () a)
runParser p s = case CP.runParser p s of
  These a s' -> (s', These () a)
  This a     -> ([], That a)
  That s'    -> (s', This ())

runParser_ :: Parser String Char a -> String -> a
runParser_ p s = case CP.runParser p s of
  These a _ -> a
  This a    -> a
  That _    -> error "Uncaught parse failure"
