{-# LANGUAGE NoMonomorphismRestriction #-}
module Parametric.Parser.Error 
  ( formatError 
  ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Parametric.Error

  -- ERROR OUTPUT --

formatError :: String -> ParseError -> FOmegaError
formatError src err 
  = Error (Just file) (Just "Syntax Error") (lines msg) (Just (lines src)) where
  
  -- extract information
  messages = errorMessages err
  pos = errorPos err
  name = sourceName pos
  
  (line, column) = convert (sourceLine pos) (sourceColumn pos)
  convert l c = if l > srcLineCount 
                  then (srcLineCount, succ srcLineLength)
                  else (l, min c (length srcLine))
  
  srcLines = lines src
  srcLineCount = length srcLines
  srcLine = srcLines !! (pred line)
  srcLineLength = length srcLine
    
  file = Position name line line column column
  msg = tail $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" messages
