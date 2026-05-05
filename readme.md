# markup-parse

[![img](https://img.shields.io/hackage/v/markup-parse.svg)](https://hackage.haskell.org/package/markup-parse)
[![img](https://github.com/tonyday567/markup-parse/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/tonyday567/markup-parse/actions/workflows/haskell-ci.yml)

`markup-parse` parses and prints a subset of common XML & HTML structured data, from and to strict bytestrings


# Development

    :r
    :set -Wno-type-defaults
    :set -Wno-name-shadowing
    :set -XOverloadedStrings
    :set -XTemplateHaskell
    :set -XQuasiQuotes
    import Control.Monad
    import MarkupParse
    import MarkupParse.Parser
    import Data.ByteString qualified as B
    import Data.ByteString.Char8 qualified as C
    import Data.Map.Strict qualified as Map
    import Data.Function
    import Data.String.Interpolate
    import Control.Monad
    bs <- B.readFile "other/line.svg"
    C.length bs

    [1 of 2] Compiling MarkupParse.Internal.FlatParse ( src/MarkupParse/Internal/FlatParse.hs, interpreted ) [Flags changed]
    Ok, two modules reloaded.
    7554


# Main Pipeline types

    :t tokenize Html
    :t gather Html
    :t normalize
    :t degather
    :t detokenize Html
    :t tokenize Html >=> gather Html >=> (normalize >>> pure) >=> degather Html >=> (fmap (detokenize Html) >>> pure)

    tokenize Html :: ByteString -> Warn [Token]
    gather Html :: [Token] -> Warn Markup
    normalize :: Markup -> Markup
    degather :: Standard -> Markup -> Warn [Token]
    detokenize Html :: Token -> ByteString
    tokenize Html >=> gather Html >=> (normalize >>> pure) >=> degather Html >=> (fmap (detokenize Html) >>> pure)
      :: ByteString -> These [MarkupWarning] [ByteString]


## tests

### Round trip equality

    m = markup_ Xml bs
    m == (markup_ Xml $ markdown_ Compact Xml m)

    True


### wiki diff test debug

    bs <- B.readFile "other/Parsing - Wikipedia.html"
    m = markup_ Html bs
    m == (markup_ Html $ markdown_ Compact Html m)

    True

# Reference

[HTML Standard](https://html.spec.whatwg.org/#toc-syntax)

[Are (non-void) self-closing tags valid in HTML5? - Stack Overflow](https://stackoverflow.com/questions/3558119/are-non-void-self-closing-tags-valid-in-html5)

[Extensible Markup Language (XML) 1.0 (Fifth Edition)](https://www.w3.org/TR/xml/)

## Prior Art

[html-parse](https://hackage.haskell.org/package/html-parse) is an attoparsec-based parser for HTML. The HTML parsing here has referenced this parsing logic for HTML elements.

[blaze-markup](https://hackage.haskell.org/package/blaze-markup) & [lucid](https://hackage.haskell.org/package/lucid) are HTML DSLs and printers, but not parsers.

[xeno](https://hackage.haskell.org/package/xeno) is an &ldquo;event-based&rdquo; XML parser with a more complicated base markup type.

[XMLParser](https://hackage.haskell.org/package/XMLParser) & [hexml](https://hackage.haskell.org/package/hexml) are parsec-based parsers.

