{-# LANGUAGE GHC2024 #-}

-- | Re-export root for the markup-parse library.
--
-- The library is split into named modules:
--
-- * "Data.Markup" — core data types.
-- * "Data.Markup.Parser" — parsing and tree operations.
-- * "Data.Markup.Render" — rendering.
-- * "Data.Markup.Warn" — warnings and warning helpers.
module MarkupParse
  ( module Data.Markup,
    module Data.Markup.Parser,
    module Data.Markup.Render,
    module Data.Markup.Warn,
  )
where

import Data.Markup
import Data.Markup.Parser
import Data.Markup.Render
import Data.Markup.Warn
