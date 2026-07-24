{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TypeFamilies #-}

-- | Core markup data types.
module Data.Markup
  ( Standard (..),
    Markup (..),
    Token (..),
    OpenTagType (..),
    Attr (..),
    AttrName,
    AttrValue,
    Element,
    NameTag,
    RenderStyle (..),
    TokenParser (..),
  )
where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Data
import Data.These
import Data.Tree
import GHC.Generics

-- | From a parsing pov, Html & Xml (& Svg) are close enough that they share a lot of parsing logic, so that parsing and printing just need some tweaking.
--
-- The xml parsing logic is based on the XML productions found in https://www.w3.org/TR/xml/
--
-- The html parsing was based on a reading of <https://hackage.haskell.org/package/html-parse html-parse>, but ignores the various '\x00' to '\xfffd' & eof directives that form part of the html standards.
data Standard = Html | Xml
  deriving stock (Eq, Ord, Show, Generic, Data)

instance NFData Standard

-- | Name of token
type NameTag = ByteString

-- | Name of an attribute.
type AttrName = ByteString

-- | Value of an attribute. "" is equivalent to true with respect to boolean attributes.
type AttrValue = ByteString

-- | Whether an opening tag is a start tag or an empty element tag.
data OpenTagType = StartTag | EmptyElemTag
  deriving (Eq, Ord, Show, Generic, Data)

instance NFData OpenTagType

-- | An attribute of a tag
--
-- In parsing, boolean attributes, which are not required to have a value in HTML,
-- will be set a value of "", which is ok. But this will then be rendered.
data Attr = Attr {attrName :: !AttrName, attrValue :: !AttrValue}
  deriving (Eq, Ord, Show, Generic, Data)

instance NFData Attr

-- | A Markup token. The term is borrowed from <https://www.w3.org/html/wg/spec/tokenization.html#tokenization HTML> standards but is used across 'Html' and 'Xml' in this library.
--
-- Note that the 'Token' type is used in two slightly different contexts:
--
-- - As an intermediary representation of markup between 'ByteString' and 'Markup'.
--
-- - As the primitives of 'Markup' 'Element's
--
-- Specifically, an 'EndTag' will occur in a list of tokens, but not as a primitive in 'Markup'. It may turn out to be better to have two different types for these two uses and future iterations of this library may head in this direction.
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

-- | A list of 'Element's or 'Tree' 'Token's
newtype Markup = Markup {elements :: [Element]}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving newtype (Semigroup, Monoid)

instance NFData Markup

-- | Most functions return a 'Markup' rather than an 'Element' because it is often more ergonomic to use the free monoid (aka a list) in preference to returning a 'Maybe' 'Element' (say).
type Element = Tree Token

-- | @Indented 0@ puts newlines in between the tags.
data RenderStyle = Compact | Indented Int deriving (Eq, Ord, Show, Read, Generic, Data)

instance NFData RenderStyle

-- | TokenParser: semantic phase parser operating on token streams
--
-- State-threading parser over token lists with error/warning accumulation.
newtype TokenParser e a = TokenParser {runTP :: [Token] -> ([Token], These e a)}
