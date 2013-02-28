{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Parametric.Parser where

import Prelude (String, (++), flip, ($), undefined, Show(..), Either (..), print, (.), pred)

import System.IO

import Control.Applicative hiding (many)
import Control.Monad

import Data.Either
import Data.Eq
import Data.Foldable

import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.ParserCombinators.Parsec as Parsec

import Parametric.Error
import Parametric.Parser.Error

import Tools.Errors.Class
import Tools.Instances


     ---------------------
    -- PARAMETRIC PARSER --
     ---------------------

-- left-recursion handling
term simple rec pos msg = result where
  result = combine <$> getPosition <*> simple <*> many ((,) <$> rec <*> getPosition)
  combine p = foldl' (\x (f, q) -> setPos pos p (f x) q)

-- right-recursive syntax pattern: "lambda ident : qualifier . body"
abs cons lambda ident colon qualifier dot body
  = cons <$> try (lambda *> ident <* colon) <*> qualifier <*> (dot *> body)

-- left-recursive syntax pattern: "x -> y"
arr cons arrow simple = flip cons <$> (arrow *> simple)

-- left-recursive syntax pattern: "x y"
app cons simple = flip cons <$> simple

-- non-recursive syntax pattern: "ident"
var cons ident = cons <$> ident

-- non-recursive syntax pattern: "constant"
con cons constant = cons <$ constant

withPos f p = setPos f <$> getPosition <*> p <*> getPosition where

setPos f p1 x p2 = f (Position (sourceName p1) (sourceLine p1) (sourceLine p2) (sourceColumn p1) (pred $ sourceColumn p2)) x

