{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | A 'Markup' parser of strict bytestrings. 'Markup' is an intermediary representation of data such as HTML, SVG or XML but the parsing is sub-standard.
--
-- The xml parsing logic is based on the XML productions found in https://www.w3.org/TR/xml/#NT-content.
--
-- The html parsing was based on a reading of html-parse, but ignores the various $(char '\x00') to '\xfffd' & eof directives that form part of the html standards.
--
module MarkupParse
  ( Markup (..),
    Standard (..),
    markup,
    markdown,
    normalize,

    TagName,
    selfClosers,
    Token (..),
    tokenize,
    detokenize,
    gather,
    gatherEither,
    degather,
    degatherEither,

    AttrName,
    AttrValue,
    Attr (..),
  )

where

import Data.ByteString (ByteString)
import FlatParse.Basic hiding (cut, take)
import Data.Bool
import Data.Char hiding (isDigit)
import Prelude hiding (replicate)
import Control.DeepSeq
import GHC.Generics
import Data.Tree
import Data.ByteString.Char8 qualified as B
import Data.Map.Strict qualified as Map
import Data.Foldable
import Data.Bifunctor
import Data.Function
import Control.Monad
import MarkupParse.FlatParse
import Control.Category ((>>>))
import Data.TreeDiff

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import MarkupParse.Common
-- >>> import MarkupParse.Xml
-- >>> import FlatParse.Basic

data Standard = Html | Xml deriving (Eq, Show, Ord, Generic, NFData)

instance ToExpr Standard

data Markup = Markup { standard :: Standard, markupTree :: [Tree Token] } deriving (Show, Eq, Ord, Generic, NFData)

instance ToExpr Markup


markup :: Standard -> ByteString -> Either String Markup
markup Html bs = bs & (tokenize Html >=> gatherEither) & second (Markup Html)
markup Xml bs = bs & (tokenize Xml >=> gatherEither) & second (Markup Xml)

markdown :: Markup -> Either String ByteString
markdown (Markup s trees) =
  trees &
  degatherEither s &
  second (fmap detokenize >>> mconcat)

normalize :: Markup -> Either String Markup
normalize (Markup s trees) =
  trees &
  degather s &
  snd &
  meldContent &
  fmap normTokenAttrs &
  gather &
  (\(warnings, ts) ->
     bool
     (Left $ unlines $ warnings)
     (Right $ Markup s ts)
     (warnings == []))

type TagName = ByteString

-- | A Markup token
data Token
  -- | An opening tag. Attribute ordering is arbitrary.
  = TagOpen !TagName [Attr]
  -- | A self-closing tag.
  | TagSelfClose !TagName [Attr]
  -- | A closing tag.
  | TagClose !TagName
  -- | The content between tags.
  | Content !ByteString
  -- | Contents of a comment.
  | Comment !ByteString
  -- | Declarations
  | Decl !ByteString
  -- | Doctypes
  | Doctype !ByteString
  -- | EOF
  | EofToken
  deriving (Show, Ord, Eq, Generic)

instance NFData Token
instance ToExpr Token

tokenize :: Standard -> ByteString -> Either String [Token]
tokenize Html bs = runParserEither (many tokenHtml) bs
tokenize Xml bs = runParserEither (many tokenXml) bs

normTokenAttrs :: Token -> Token
normTokenAttrs (TagOpen n as) = TagOpen n (normAttrs as)
normTokenAttrs (TagSelfClose n as) = TagSelfClose n (normAttrs as)
normTokenAttrs x = x

-- | Html tags that self-close
selfClosers :: [TagName]
selfClosers = [
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



type AttrName = ByteString

type AttrValue = ByteString

-- | An attribute of a tag
data Attr = Attr !AttrName !AttrValue
          deriving (Generic, Show, Eq, Ord)

instance NFData Attr
instance ToExpr Attr

-- | normalise an attribution list, removing duplicate AttrNames, and space concatenating class values.
normAttrs :: [Attr] -> [Attr]
normAttrs as = uncurry Attr <$> (Map.toList $ foldl' (\s (Attr n v) -> Map.insertWithKey ( \k a b ->
            case k of
              "class" -> a <> " " <> b
              _ -> b
        ) n v s) Map.empty as)

-- * comments
commentClose :: Parser e ()
commentClose = $(string "-->")

charNotMinus :: Parser e ByteString
charNotMinus = byteStringOf $ satisfy (/= '-')

minusPlusChar :: Parser e ByteString
minusPlusChar = byteStringOf $ $(char '-') *> charNotMinus

-- | comment in xml or html
--
--
-- >>> runParserMaybe xmlComment "<!-- comment -->"
-- Just " comment "
comment :: Parser e Token
comment = Comment <$> byteStringOf (many (charNotMinus <|> minusPlusChar)) <* commentClose

content :: Parser e Token
content = Content <$> byteStringOf (some (satisfy (/= '<')))

-- | (Somewhat) canonical string representation of 'Token'.
detokenize :: Token -> ByteString
detokenize = mconcat . \case
    (TagOpen n [])         -> ["<", n, ">"]
    (TagOpen n attrs)      -> ["<", n, " ", renderAttrs attrs, ">"]
    (TagSelfClose n attrs) -> ["<", n, " ", renderAttrs attrs, " />"]
    (TagClose n)           -> ["</", n, ">"]
    (Content t)            -> [t]
    (Comment t)            -> ["<!--", t, "-->"]
    (Doctype t)            -> ["<!", t, ">"]
    (Decl t)               -> ["<!", t ,"!>"]
    EofToken               -> []

renderAttrs :: [Attr] -> ByteString
renderAttrs = B.unwords . fmap renderAttr

-- Does not escape quotation in attribute values!
renderAttr :: Attr -> ByteString
renderAttr (Attr k v) = mconcat [k, "=\"", v, "\""]

-- | Meld neighboring 'Content' together and drop empties
meldContent :: [Token] -> [Token]
meldContent = filter (/= Content "") . concatBS
  where
    concatBS = \case
      (Content t : Content t' : ts) -> concatBS $ Content (t <> t') : ts
      (t : ts) -> t : concatBS ts
      [] -> []

gather :: [Token] -> ([String], [Tree Token])
gather = (f (Stack [] []))
  where
    f (Stack ss []) [] = ([], reverse ss)
    f pstack [] = (`f` []) =<< (popParentNoCheck pstack)
    f pstack (t : ts)   = case t of
        TagOpen n _     -> if n `elem` selfClosers
                             then f (pushFlatSibling t pstack) ts
                             else f (pushParent t pstack) ts
        TagSelfClose {} -> f (pushFlatSibling t pstack) ts
        TagClose n      -> (`f` ts) =<< popParent n pstack
        Content _   -> f (pushFlatSibling t pstack) ts
        Comment _       -> f (pushFlatSibling t pstack) ts
        Doctype _       -> f (pushFlatSibling t pstack) ts
        Decl _       -> f (pushFlatSibling t pstack) ts
        EofToken -> f pstack ts

gatherEither :: [Token] -> Either String [Tree Token]
gatherEither = gather >>>
  (\(warnings, ts) ->
     bool
     (Left $ unlines $ warnings)
     (Right ts)
     (warnings == []))

data Stack = Stack
    { _siblings :: [ Tree Token ]
    , _parents :: [(Token, [ Tree Token ])]
    }
  deriving (Eq, Show)

pushParent :: Token -> Stack -> Stack
pushParent t (Stack ss ps) = Stack [] ((t, ss) : ps)

popParent :: TagName -> Stack -> ([String], Stack)
popParent n (Stack ss ((p@(TagOpen n' _), ss') : ps)) =
    (bool
      ([unwords ["tag mismatch:", B.unpack n, B.unpack n']])
      []
      (n == n'),
      (Stack (Node p (reverse ss) : ss') ps))
popParent n (Stack ss ((p,ss') : ps)) =
  (["bad parent pop" <> B.unpack n], Stack (Node p (reverse ss) : ss') ps)
popParent n (Stack ss []) =
  (["no parent to pop" <> B.unpack n], Stack ss [])

popParentNoCheck :: Stack -> ([String], Stack)
popParentNoCheck (Stack ss ((p@(TagOpen n _), ss') : ps)) =
      (["unclosed tag: " <> B.unpack n],
      (Stack (Node p (reverse ss) : ss') ps))
popParentNoCheck (Stack ss ((p,ss') : ps)) =
  (["no closure parent pop"], Stack (Node p (reverse ss) : ss') ps)
popParentNoCheck (Stack ss []) =
  (["no parent to pop"], Stack ss [])

pushFlatSibling :: Token -> Stack -> Stack
pushFlatSibling t (Stack ss ps) = Stack (Node t [] : ss) ps

degather :: Standard -> [Tree Token] -> ([String], [Token])
degather s trees = trees & fmap (tokenList s) & mconcat

degatherEither :: Standard -> [Tree Token] -> Either String [Token]
degatherEither s = degather s >>>
  (\(warnings, ts) ->
     bool
     (Left $ unlines $ warnings)
     (Right ts)
     (warnings == []))

tokenList :: Standard -> Tree Token -> ([String], [Token])
tokenList Html (Node o@(TagOpen n _) ts) | n `notElem` selfClosers
  = degather Html ts & (second ((<> [TagClose n]) >>> ([o]<>)))
tokenList Xml (Node o@(TagOpen n _) ts) = degather Xml ts & second ((<> [TagClose n]) >>> ([o]<>))
tokenList _ (Node t []) = ([],[t])
tokenList _ _ = (["Leaf node has children"], [])

tokenXml :: Parser e Token
tokenXml =
  $(switch [| case _ of
      "<!--" -> comment
      "<!" -> doctypeXml
      "</" -> closeTagXml
      "<?" -> declXml
      "<" -> openTagsXml
      _ -> content |])

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
--
-- >>> runParserMaybe xmlName "name"
-- Just "name"
nameXml :: Parser e ByteString
nameXml = byteStringOf (nameStartChar >> many nameChar)

declXml :: Parser e Token
declXml = Decl <$> xmlXMLDecl

-- | XML declaration as per production rule [23]
--
-- >>> runParserMaybe xmlXMLDecl "<?xml version=\"1.0\" standalone=\"yes\" ?>"
-- Just "<?xml version=\"1.0\" standalone=\"yes\" ?>"
xmlXMLDecl :: Parser e ByteString
xmlXMLDecl =
  byteStringOf $
    ($(string "xml")
      >> xmlVersionInfo
      >> optional xmlEncodingDecl
      >> optional xmlStandalone
      >> optional wss) <* $(string "?>")

-- xml production [24]
xmlVersionInfo :: Parser e ByteString
xmlVersionInfo = byteStringOf $ wss >> $(string "version") >> eq >> wrappedQNoGuard xmlVersionNum

-- | xml production [26]
xmlVersionNum :: Parser e ByteString
xmlVersionNum =
  byteStringOf ($(string "1.") >> some (satisfy isDigit))

-- | Doctype declaration as per production rule [28]
--
-- >>> runParserMaybe xmlDoctypedecl "<!DOCTYPE foo [ declarations ]>"
-- Just "<!DOCTYPE foo [ declarations ]>"
doctypeXml :: Parser e Token
doctypeXml = Doctype <$>
  (byteStringOf $
    $(string "DOCTYPE")
      >> wss
      >> nameXml
      >>
      -- optional (wss >> xmlExternalID) >>
      optional wss
      >> optional bracketedSB
      >> optional wss) <* $(char '>')

bracketedSB :: Parser e [Char]
bracketedSB = bracketed $(char '[') $(char ']') (many (satisfy (/= ']')))

-- [32]
xmlStandalone :: Parser e ByteString
xmlStandalone =
  byteStringOf $
    wss *> $(string "standalone") *> eq *> xmlYesNo

xmlYesNo :: Parser e ByteString
xmlYesNo = wrappedQNoGuard (byteStringOf $ $(string "yes") <|> $(string "no"))

-- | xml production [80]
xmlEncodingDecl :: Parser e ByteString
xmlEncodingDecl = wss *> $(string "encoding") *> eq *> wrappedQNoGuard xmlEncName

-- [81]
xmlEncName :: Parser e ByteString
xmlEncName = byteStringOf (satisfyAscii isLatinLetter >> many (satisfyAscii (\x -> isLatinLetter x || isDigit x || elem x ("._-" :: [Char]))))

-- | open xml tag as per xml production rule [40]
-- | self-closing xml tag as per [44]
openTagsXml :: Parser e Token
openTagsXml = do
  n <- nameXml
  as <- many (wss *> attXml)
  _ <- optional wss
  $(switch [| case _ of
      "/>" -> pure (TagSelfClose n as)
      ">" -> pure (TagOpen n as)|])

attXml :: Parser e Attr
attXml = Attr <$> (nameXml <* eq) <*> wrappedQ

-- | closing tag as per [42]
closeTagXml :: Parser e Token
closeTagXml = TagClose <$> (nameXml <* optional wss <* $(char '>'))

-- | Parse a single 'Token'.
tokenHtml :: Parser e Token
tokenHtml = $(switch [| case _ of
      "<!--" -> comment
      "<!" -> doctypeHtml
      "</" -> closeTagHtml
      "<?" -> bogusCommentHtml
      "<" -> openTagsHtml
      _ -> content |])

bogusCommentHtml :: Parser e Token
bogusCommentHtml = Comment <$> byteStringOf (some (satisfy (/= '<')))

doctypeHtml :: Parser e Token
doctypeHtml = Doctype <$>
  (byteStringOf $
    $(string "DOCTYPE")
      >> wss
      >> nameHtml
      >> optional wss) <* $(char '>')

openTagsHtml :: Parser e Token
openTagsHtml = do
  n <- nameHtml
  as <- many (wss *> (attrHtml <|> attrData))
  _ <- optional wss
  $(switch [| case _ of
      "/>" -> pure (TagSelfClose n as)
      ">" -> pure (TagOpen n as)|])

closeTagHtml :: Parser e Token
closeTagHtml = TagClose <$> nameHtml <* optional wss <* $(char '>')

nameHtml :: Parser e ByteString
nameHtml = do
  byteStringOf (nameStartCharHtml >> many (satisfy isNameChar))

nameStartCharHtml :: Parser e Char
nameStartCharHtml = satisfyAscii isLatinLetter

isNameChar :: Char -> Bool
isNameChar x =
  not $
  ((isWhitespace x) ||
  (x=='/') ||
  (x=='<') ||
  (x=='>'))

-- >>> runParserMaybe xmlAtt "style = 'fancy'"
-- Just ("style","fancy")
attrHtml :: Parser e Attr
attrHtml = Attr <$> (attrNameHtml <* eq) <*> (wrappedQ <|> unwrappedV)

attrData :: Parser e Attr
attrData = (\n -> Attr n mempty) <$> (byteStringOf ($(string "data-") >> some (satisfy isAttrName)))

attrNameHtml :: Parser e ByteString
attrNameHtml = byteStringOf $ some (satisfy isAttrName)

isAttrName :: Char -> Bool
isAttrName x = not $
  (isWhitespace x) ||
  (x=='/') ||
  (x=='>') ||
  (x=='=')
