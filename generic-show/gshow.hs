{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Text
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Generics.Basics)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.uu.nl/wiki/GenericProgramming/SYB>. The present module
-- provides generic operations for text serialisation of terms.
--
-- https://hackage.haskell.org/package/syb-0.7.2.1/docs/Data-Generics-Text.html#v:gshow
-----------------------------------------------------------------------------

module Data.Generics.Text (

    -- * Generic show
    gshow, gshows,

    -- * Generic read
    -- gread

 ) where

------------------------------------------------------------------------------

#ifdef __HADDOCK__
import Prelude
#endif
import Control.Monad
import Data.Data
import Data.Generics.Aliases
import Text.ParserCombinators.ReadP
import Text.Read.Lex

------------------------------------------------------------------------------

-- | Generic show: an alternative to \"deriving Show\"
gshow :: Data a => a -> String
gshow x = gshows x ""

-- | Generic shows
gshows :: Data a => a -> ShowS

-- This is a prefix-show using surrounding "(" and ")",
-- where we recurse into subterms with gmapQ.
gshows = ( \t ->
                showChar '('
              . (showString . showConstr . toConstr $ t)
              . (foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t)
              . showChar ')'
         ) `extQ` (shows :: String -> ShowS)