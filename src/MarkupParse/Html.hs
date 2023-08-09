{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
module MarkupParse.Html where

import MarkupParse
import MarkupParse.Common
import Data.ByteString.Char8 qualified as B
import Data.ByteString (ByteString)
import FlatParse.Basic hiding (cut, take)
import FlatParse.Basic qualified as FP
import GHC.Generics
import Prelude
import Data.Char
import Data.Bool
import Control.Monad
import Data.List qualified as List
import Data.Tree

-- Section numbers refer to W3C HTML 5.2 specification.

-- | A tag name (e.g. @body@)
type TagName   = ByteString

-- | An attribute name (e.g. @href@)
type AttrName  = ByteString

-- | The value of an attribute
type AttrValue = ByteString

-- | An HTML token
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

-- | This is a bit of a hack
endOfFileToken :: Token
endOfFileToken = ContentByteString ""

-- | An attribute of a tag
data Attr = Attr !AttrName !AttrValue
          deriving (Show, Eq, Ord)

-- | Parse a single 'Token'.
token :: Parser e Token
token = dataState -- Start in the data state.

-- | /§8.2.4.1/: Data state
dataState :: Parser e Token
dataState =
  (lt >> tagOpen) <|> contentByteStringP

contentByteStringP :: Parser e Token
contentByteStringP = ContentByteString <$> (byteStringOf $ some $ satisfy (/= '<'))

-- | /§8.2.4.6/: Tag open state
tagOpen :: Parser e Token
tagOpen =
  $(switch [| case _ of
      "!" -> markupDeclOpen
      "/" -> endTagOpen
      "?" -> bogusCommentToken mempty
      _ -> (tagNameOpen <|> pure (ContentChar '<')) |])

-- | /§8.2.4.7/: End tag open state
endTagOpen :: Parser e Token
endTagOpen = tagNameClose


-- | /§8.2.4.8/: Tag name state: the open case
--
-- deviation: no lower-casing, don't handle NULL characters
tagNameOpen :: Parser e Token
tagNameOpen = do
    tag <- tagName'
    (satisfy isWhitespace >> beforeAttrName tag [])
      <|> ($(char '/') >> selfClosingStartTag tag [])
      <|> ($(char '>') >> return (TagOpen tag []))

-- | /§8.2.4.10/: Tag name state: close case
tagNameClose :: Parser e Token
tagNameClose = do
    tag <- tagName'
    $(char '>') >> return (TagClose tag)

-- | /§8.2.4.10/: Tag name state: common code
--
-- deviation: no lower-casing, don't handle NULL characters
tagName' :: Parser e ByteString
tagName' = do
  byteStringOf (tagNameStartChar >> many (satisfy isNameChar))

tagNameStartChar :: Parser e Char
tagNameStartChar = satisfyAscii (\x -> isLower x || isUpper x)

isNameChar :: Char -> Bool
isNameChar x =
  not $
  ((isWhitespace x) ||
  (x=='/') ||
  (x=='<') ||
  (x=='>'))

-- | /§8.2.4.40/: Self-closing start tag state
selfClosingStartTag :: TagName -> [Attr] -> Parser e Token
selfClosingStartTag tag attrs = do
        ($(char '>') >> return (TagSelfClose tag attrs))
    <|> (eof >> return endOfFileToken)
    <|> beforeAttrName tag attrs

-- | /§8.2.4.32/: Before attribute name state
--
-- deviation: no lower-casing
beforeAttrName :: TagName -> [Attr] -> Parser e Token
beforeAttrName tag attrs =
    ws_ >>
    (($(char '/') >> selfClosingStartTag tag attrs)
      <|> ($(char '>') >> return (TagOpen tag attrs))
      -- <|> (char '\x00' >> attrName tag attrs) -- TODO: NULL
      <|> attrName tag attrs)

-- | /§8.2.4.33/: Attribute name state
attrName :: TagName -> [Attr] -> Parser e Token
attrName tag attrs = do
    name <- byteStringOf $ many (satisfy isAttrName)
    (eof >> afterAttrName tag attrs name)
      <|> ($(char '=') >> beforeAttrValue tag attrs name)
      <|> try (do mc <- lookahead anyChar
                  bool empty (afterAttrName tag attrs name) (isAfterAttrName mc))

isAfterAttrName :: Char -> Bool
isAfterAttrName x = not $
  (isWhitespace x) ||
  (x=='/') ||
  (x=='>')

isAttrName :: Char -> Bool
isAttrName x = not $
  (isWhitespace x) ||
  (x=='/') ||
  (x=='>') ||
  (x=='=')

-- | /§8.2.4.34/: After attribute name state
afterAttrName :: TagName -> [Attr] -> AttrName -> Parser e Token
afterAttrName tag attrs name =
    ws_ >>
    ($(char '/') >> selfClosingStartTag tag attrs)
      <|> ($(char '=') >> beforeAttrValue tag attrs name)
      <|> ($(char '>') >> return (TagOpen tag (Attr name mempty : attrs)))
      <|> (eof >> return endOfFileToken)
      <|> attrName tag (Attr name mempty : attrs)  -- not exactly sure this is right

-- | /§8.2.4.35/: Before attribute value state
beforeAttrValue :: TagName -> [Attr] -> AttrName -> Parser e Token
beforeAttrValue tag attrs name =
    ws_ >>
    ($(char '"') >> attrValueDQuoted tag attrs name)
      <|> ($(char '\\') >> attrValueSQuoted tag attrs name)
      <|> ($(char '>') >> return (TagOpen tag (Attr name mempty : attrs)))
      <|> attrValueUnquoted tag attrs name

-- | /§8.2.4.36/: Attribute value (double-quoted) state
attrValueDQuoted :: TagName -> [Attr] -> AttrName -> Parser e Token
attrValueDQuoted tag attrs name = do
    value <- byteStringOf $ many (satisfy (/= '"'))
    _ <- $(char '"')
    afterAttrValueQuoted tag attrs name value

-- | /§8.2.4.37/: Attribute value (single-quoted) state
attrValueSQuoted :: TagName -> [Attr] -> AttrName -> Parser e Token
attrValueSQuoted tag attrs name = do
    value <- byteStringOf $ many (satisfy (/= '\''))
    _ <- $(char '\\')
    afterAttrValueQuoted tag attrs name value

-- | /§8.2.4.38/: Attribute value (unquoted) state
attrValueUnquoted :: TagName -> [Attr] -> AttrName -> Parser e Token
attrValueUnquoted tag attrs name = do
    value <- byteStringOf $ many $ satisfy (\c -> isWhitespace c || c == '>')
    id $  (satisfy isWhitespace >> beforeAttrName tag attrs) -- unsure: don't emit?
      <|> ($(char '>') >> return (TagOpen tag (Attr name value : attrs)))
      <|> (eof >> return endOfFileToken)

-- | /§8.2.4.39/: After attribute value (quoted) state
afterAttrValueQuoted :: TagName -> [Attr] -> AttrName -> AttrValue -> Parser e Token
afterAttrValueQuoted tag attrs name value =
          (satisfy isWhitespace >> beforeAttrName tag attrs')
      <|> ($(char '/') >> selfClosingStartTag tag attrs')
      <|> ($(char '>') >> return (TagOpen tag attrs'))
      <|> (eof >> return endOfFileToken)
  where attrs' = Attr name value : attrs

-- | /§8.2.4.41/: Bogus comment state
bogusCommentToken :: ByteString -> Parser e Token
bogusCommentToken content = do
        ($(char '>') >> return (CommentToken content))
    <|> (eof >> return (CommentToken content))
    <|> ($(char '\x00') >> bogusCommentToken (content <> "\xfffd"))
    <|> (anyChar >>= \c -> bogusCommentToken (content <> B.singleton c))

-- | /§8.2.4.42/: Markup declaration open state
markupDeclOpen :: Parser e Token
markupDeclOpen =
        try comment_
    <|> try docType
    <|> bogusCommentToken mempty
  where
    comment_ = $(char '-') >> $(char '-') >> commentStart
    docType = do
        -- switching this to asciiCI slowed things down by a factor of two
        s <- FP.take 7
        _ <- guard $ (toLower <$> (B.unpack s)) == "doctype"
        doctype

-- | /§8.2.4.43/: CommentToken start state
commentStart :: Parser e Token
commentStart = do
          ($(char '-') >> commentStartDash)
      <|> ($(char '>') >> return (CommentToken mempty))
      <|> comment mempty

-- | /§8.2.4.44/: CommentToken start dash state
commentStartDash :: Parser e Token
commentStartDash =
          ($(char '-') >> commentEnd mempty)
      <|> ($(char '>') >> return (CommentToken mempty))
      <|> (eof >> return (CommentToken mempty))
      <|> (comment (B.singleton '-'))

-- | /§8.2.4.45/: CommentToken state
comment :: ByteString -> Parser e Token
comment content0 = do
    content <- byteStringOf $ many $ satisfy isCommentChar
    id $  ($(char '<') >> commentLessThan (content0 <> content <> "<"))
      <|> ($(char '-') >> commentEndDash (content0 <> content))
      <|> ($(char '\x00') >> comment (content0 <> content <> B.singleton '\xfffd'))
      <|> (eof >> return (CommentToken $ content0 <> content))

isCommentChar :: Char -> Bool
isCommentChar x = not $
  (x=='-') ||
  (x=='\x00') ||
  (x=='<')

-- | /§8.2.46/: CommentToken less-than sign state
commentLessThan :: ByteString -> Parser e Token
commentLessThan content =
        ($(char '!') >> commentLessThanBang (content <> "!"))
    <|> ($(char '<') >> commentLessThan (content <> "<"))
    <|> comment content

-- | /§8.2.47/: CommentToken less-than sign bang state
commentLessThanBang :: ByteString -> Parser e Token
commentLessThanBang content =
        ($(char '-') >> commentLessThanBangDash content)
    <|> comment content

-- | /§8.2.48/: CommentToken less-than sign bang dash state
commentLessThanBangDash :: ByteString -> Parser e Token
commentLessThanBangDash content =
        ($(char '-') >> commentLessThanBangDashDash content)
    <|> commentEndDash content

-- | /§8.2.49/: CommentToken less-than sign bang dash dash state
commentLessThanBangDashDash :: ByteString -> Parser e Token
commentLessThanBangDashDash content =
        ($(char '>') >> comment content)
    <|> (eof >> comment content)
    <|> commentEnd content

-- | /§8.2.4.50/: CommentToken end dash state
commentEndDash :: ByteString -> Parser e Token
commentEndDash content = do
        ($(char '-') >> commentEnd content)
    <|> (eof >> return (CommentToken content))
    <|> (comment (content <> "-"))

-- | /§8.2.4.51/: CommentToken end state
commentEnd :: ByteString -> Parser e Token
commentEnd content = do
        ($(char '>') >> return (CommentToken content))
    <|> ($(char '!') >> commentEndBang content)
    <|> ($(char '-') >> commentEnd (content <> "-"))
    <|> (eof >> return (CommentToken content))
    <|> (comment (content <> "--"))

-- | /§8.2.4.52/: CommentToken end bang state
commentEndBang :: ByteString -> Parser e Token
commentEndBang content = do
        ($(char '-') >> commentEndDash (content <> "--!"))
    <|> ($(char '>') >> return (CommentToken content))
    <|> (eof >> return (CommentToken content))
    <|> (comment (content <> "--!"))

-- | /§8.2.4.53/: DOCTYPE state
-- FIXME
doctype :: Parser e Token
doctype = do
    content <- byteStringOf $ many $ satisfy (/='>')
    _ <- $(char '>')
    return $ Doctype content

-- | Parse a lazy list of tokens from a 'ByteString'.
parseTokens :: ByteString -> [Token]
parseTokens = List.unfoldr f
  where
    f :: ByteString -> Maybe (Token, ByteString)
    f t
      | mempty==t = Nothing
      | otherwise =
        case runParser token t of
            OK tok rest -> Just (tok, rest)
            _ -> Nothing

-- | See 'renderToken'.
renderTokens :: [Token] -> ByteString
renderTokens = mconcat . fmap renderToken

-- | (Somewhat) canonical string representation of 'Token'.
renderToken :: Token -> ByteString
renderToken = mconcat . \case
    (TagOpen n [])         -> ["<", n, ">"]
    (TagOpen n attrs)      -> ["<", n, " ", renderAttrs attrs, ">"]
    (TagSelfClose n attrs) -> ["<", n, " ", renderAttrs attrs, " />"]
    (TagClose n)           -> ["</", n, ">"]
    (ContentChar c)        -> [B.singleton c]
    (ContentByteString t)        -> [t]
    (CommentToken builder)      -> ["<!--", builder, "-->"]
    (Doctype t)            -> ["<!DOCTYPE", t, ">"]

-- | See 'renderAttr'.
renderAttrs :: [Attr] -> ByteString
renderAttrs = B.unwords . fmap renderAttr . reverse

-- | Does not escape quotation in attribute values!
renderAttr :: Attr -> ByteString
renderAttr (Attr k v) = mconcat [k, "=\"", v, "\""]

-- | Meld neighoring 'ContentChar' and 'ContentByteString' constructors together and drops empty text
-- elements.
canonicalizeTokens :: [Token] -> [Token]
canonicalizeTokens = filter (/= ContentByteString "") . meldByteStringTokens

meldByteStringTokens :: [Token] -> [Token]
meldByteStringTokens = concatByteStrings . fmap charToByteString
  where
    charToByteString (ContentChar c) = ContentByteString (B.singleton c)
    charToByteString t = t

    concatByteStrings = \case
      (ContentByteString t : ContentByteString t' : ts) -> concatByteStrings $ ContentByteString (t <> t') : ts
      (t : ts) -> t : concatByteStrings ts
      [] -> []

tokensToTree :: [Token] -> Either ByteString [Tree Token]
tokensToTree = f (Stack [] [])
  where
    f (Stack ss []) [] = Right (reverse ss)
    f _ []         = Left "tag mismatch"
    f pstack (t : ts)   = case t of
        TagOpen n _     -> if n `elem` voidElements
                             then f (pushFlatSibling t pstack) ts
                             else f (pushParent t pstack) ts
        TagSelfClose {} -> f (pushFlatSibling t pstack) ts
        TagClose n      -> (`f` ts) =<< popParent n pstack
        ContentChar _   -> f (pushFlatSibling t pstack) ts
        ContentByteString _   -> f (pushFlatSibling t pstack) ts
        CommentToken _       -> f (pushFlatSibling t pstack) ts
        Doctype _       -> f (pushFlatSibling t pstack) ts

data Stack = Stack
    { _siblings :: [ Tree Token ]
    , _parents :: [(Token, [ Tree Token ])]
    }
  deriving (Eq, Show)

pushParent :: Token -> Stack -> Stack
pushParent t (Stack ss ps) = Stack [] ((t, ss) : ps)

popParent :: TagName -> Stack -> Either ByteString Stack
popParent n (Stack ss ((p@(TagOpen n' _), ss') : ps))
    | n == n' = Right $ Stack (Node p (reverse ss) : ss') ps
popParent _ _
    = Left "tag mismatch"

pushFlatSibling :: Token -> Stack -> Stack
pushFlatSibling t (Stack ss ps) = Stack (Node t [] : ss) ps

tokensFromForest :: Forest Token -> [Token]
tokensFromForest = mconcat . fmap tokensFromTree

tokensFromTree :: Tree Token -> [Token]
tokensFromTree (Node o@(TagOpen n _) ts) | n `notElem` voidElements
    = [o] <> tokensFromForest ts <> [TagClose n]
tokensFromTree (Node t [])
    = [t]
tokensFromTree _
    = error "renderTokenTree: leaf node with children."

-- | test round-trip sans whitespace differences
isoNonWhitespace :: ByteString -> Bool
isoNonWhitespace bs = (== B.filter (not . isWhitespace) bs) $ B.filter (not . isWhitespace) $ renderTokens $ mconcat $ fmap tokensFromTree (either undefined id (tokensToTree (parseTokens bs)))
