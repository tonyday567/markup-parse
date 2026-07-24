{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering functions for markup.
module Data.Markup.Render
  ( markdown,
    markdown_,
    detokenize,
    escapeChar,
    escape,
    content,
  )
where

import Control.Category ((>>>))
import Data.Bifunctor
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Markup
import Data.Markup.Parser (normContent, selfClosers)
import Data.Markup.Warn (MarkupWarning (..), Warn, concatWarns, warnError)
import Data.These
import Data.Tree

-- $setup
-- >>> :set -XOverloadedStrings

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
-- escapeChar \'\'\' = "&apos;"
-- escapeChar '"' = "&quot;"
-- @
--
-- No attempt is made to meet the <https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references HTML Standards>
--
-- >>> escape "<foo class=\"a\" bar='b'>"
-- "&lt;foo class=&quot;a&quot; bar=&apos;b&apos;&gt;"
escape :: ByteString -> ByteString
escape = B.concatMap escapeChar

-- | Create 'Markup' 'Content' from a bytestring, escaping the usual characters.
--
-- >>> content "<content>"
-- Markup {elements = [Node {rootLabel = Content "&lt;content&gt;", subForest = []}]}
content :: ByteString -> Markup
content b = Markup [pure $ Content (escape b)]

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
markdown :: RenderStyle -> Standard -> Markup -> Warn ByteString
markdown r s m = second (finalConcat r) $ concatWarns $ foldTree (renderBranch r s) <$> elements (normContent m)

-- | Convert 'Markup' to 'ByteString' and error on warnings.
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
