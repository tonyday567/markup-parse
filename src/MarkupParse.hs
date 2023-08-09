{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- | An intermediary representation not unlike HTML, SVG or XML but only forming a subset of these standards.
module MarkupParse
  (

    Attributes (..),
    attribute,
    Markup (..),
    Content (..),
    renderMarkup,
    encodeMarkup,
    encodeHtml,
    header,
    encodeNum,
    encodePx,
    voidElements,
  )
where

import Data.Bool
import Data.ByteString (ByteString, intercalate)
import Data.ByteString.Char8 (pack)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.TreeDiff
import GHC.Generics
import Prelude
import NumHask.Space
import Data.FormatN
import GHC.Exts
import Data.Tree

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Optics.Core
-- >>> import MarkupParse
-- >>> import NumHask.Space

-- | Show a Double, or rounded to 4 decimal places if this is shorter.
--
-- >>> encodeNum 1
-- "1.0"
--
-- >>> encodeNum 1.23456
-- "1.2346"
encodeNum :: Double -> ByteString
encodeNum = encodeUtf8 . formatOrShow (FixedStyle 4) Nothing

-- | SVG width and height, without any unit suffix, are defined as pixels, which are Integers
--
-- >>> encodePx 300.0
-- "300"
encodePx :: Double -> ByteString
encodePx = pack . show . (floor :: Double -> Int)

-- | A collection of attributes as a ByteString key-value map.
newtype Attributes = Attributes {attMap :: Map ByteString ByteString} deriving (Eq, Show, Generic)

instance ToExpr Attributes

-- Like Last for most attributes but concatenates the "class" attribute.
instance Semigroup Attributes where
  (<>) (Attributes m) (Attributes m') =
    Attributes $
      Map.unionWithKey
        ( \k a b ->
            case k of
              "class" -> a <> " " <> b
              _ -> b
        )
        m
        m'

instance Monoid Attributes where
  mempty = Attributes Map.empty

-- | Create a singleton Attributes
attribute :: (ByteString, ByteString) -> Attributes
attribute (k, v) = Attributes $ Map.singleton k v

instance IsList Attributes
  where
    type (Item Attributes) = (ByteString, ByteString)

    fromList = foldMap attribute
    toList = Map.toList . attMap

-- | A representation of SVG (and XML) markup with no specific knowledge of SVG or XML syntax rules.
--
data Markup = Markup
  { tag :: ByteString,
    atts :: Attributes,
    contents :: [Content]
  }
  deriving (Eq, Show, Generic)

instance ToExpr Markup

-- | The things that can be inside (form the Content of) a Markup element, especially in a DOM context. Comments are unused by the library representation of a chart and are here to help with parsing arbitrary svg in the wild.
--
data Content = Content ByteString | Comment ByteString | MarkupLeaf Markup deriving (Eq, Show, Generic)

instance ToExpr Content

-- | A tag name (e.g. @body@)
type TagName   = ByteString

-- | An attribute name (e.g. @href@)
type AttrName  = ByteString

-- | The value of an attribute
type AttrValue = ByteString

-- | An attribute of a tag
data Attr = Attr !AttrName !AttrValue
          deriving (Show, Eq, Ord)

-- | A markup token, covering (subsets of) xml, svg and html
data Token
  -- | An opening tag. Attribute ordering is arbitrary.
  = TagOpen !TagName [Attr]
  -- | A self-closing tag.
  | TagSelfClose !TagName [Attr]
  -- | A closing tag.
  | TagClose !TagName
  -- | The content between tags.
  | ContentByteString !ByteString
  -- | A single character of content
  | ContentChar !Char
  -- | Contents of a comment.
  | CommentToken !ByteString
  -- | Doctype
  | Doctype !ByteString
  deriving (Show, Ord, Eq, Generic)

data MarkupTree = MarkupTree { tokens:: [Tree Token] } deriving (Eq, Show)



-- | render markup to Text compliant with being an SVG object (and XML element)
--
renderMarkup :: Markup -> Text
renderMarkup (Markup n as xs) =
  bool [i|<#{na}>#{ls}</#{n}>|] [i|<#{na}/>|] (xs == mempty)
  where
    na = intercalate " " ([n] <> (uncurry encodeAttribute <$> Map.toList (attMap as)))
    ls = mconcat (encodeContent <$> xs)

-- | render markup to a ByteString compliant with being an SVG object (and XML element)
--
encodeMarkup :: Markup -> ByteString
encodeMarkup (Markup n as xs) =
  bool [i|<#{na}>#{ls}</#{n}>|] [i|<#{na}/>|] (xs == mempty)
  where
    na = intercalate " " ([n] <> (uncurry encodeAttribute <$> Map.toList (attMap as)))
    ls = mconcat (encodeContent <$> xs)

-- | render markup to a ByteString compliant with being a HTML object
--
encodeHtml :: Markup -> ByteString
encodeHtml (Markup n as xs) =
  bool [i|<#{na}>#{ls}</#{n}>|] [i|<#{na}>|] (xs == mempty && (n `elem` voidElements))
  where
    na = intercalate " " ([n] <> (uncurry encodeAttribute <$> Map.toList (attMap as)))
    ls = mconcat (encodeContent <$> xs)


encodeContent :: Content -> ByteString
encodeContent (Content c) = c
encodeContent (Comment c) = encodeComment c
encodeContent (MarkupLeaf x) = encodeMarkup x

encodeComment :: ByteString -> ByteString
encodeComment c = "<!--" <> c <> "-->"

encodeAttribute :: ByteString -> ByteString -> ByteString
encodeAttribute a b = [i|#{a}="#{b}"|]

-- | Create the classic SVG element
--
-- >>> header 100 (Rect 0 1 0 1) [Markup "foo" mempty mempty]
-- Markup {tag = "svg", atts = Attributes {attMap = fromList [("height","100"),("viewBox","0 -1.0 1.0 1.0"),("width","100"),("xmlns","http://www.w3.org/2000/svg"),("xmlns:xlink","http://www.w3.org/1999/xlink")]}, contents = [MarkupLeaf (Markup {tag = "foo", atts = Attributes {attMap = fromList []}, contents = []})]}
header :: Double -> Rect Double -> [Markup] -> Markup
header markupheight viewbox content' =
  Markup
    "svg"
    ( foldMap
        attribute
        [ ("xmlns", "http://www.w3.org/2000/svg"),
          ("xmlns:xlink", "http://www.w3.org/1999/xlink"),
          ("width", encodePx w''),
          ("height", encodePx h'),
          ("viewBox", encodeNum x <> " " <> encodeNum (-w) <> " " <> encodeNum (z - x) <> " " <> encodeNum (w - y))
        ]
    )
    (MarkupLeaf <$> content')
  where
    (Rect x z y w) = viewbox
    Point w' h = width viewbox
    Point w'' h' = Point (markupheight / h * w') markupheight

voidElements :: [ByteString]
voidElements = [
    "area"
  , "base"
  , "br"
  , "col"
  , "embed"
  , "hr"
  , "img"
  , "input"
  , "link"
  , "meta"
  , "param"
  , "source"
  , "track"
  , "wbr" ]

