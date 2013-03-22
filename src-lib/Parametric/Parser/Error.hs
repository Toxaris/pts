{-# LANGUAGE NoMonomorphismRestriction #-}
module Parametric.Parser.Error
  ( formatError
  ) where

import Parametric.Error

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error


  -- ERROR OUTPUT --

formatError :: FilePath -> String -> ParseError -> FOmegaError
formatError expectedName src err
  = Error (Just file) (Just "Syntax Error") (lines msg) maybeSrc where

  -- extract information
  messages = errorMessages err
  pos = errorPos err
  name = sourceName pos

  (line, column) = convert (sourceLine pos) (sourceColumn pos)
  convert l c = if l > srcLineCount
                  then (srcLineCount, succ srcLineLength)
                  else (l, min c (length srcLine))

  maybeSrc = if name == expectedName
               then Just (lines src)
               else Nothing

  srcLines = lines src
  srcLineCount = length srcLines
  srcLine = srcLines !! (pred line)
  srcLineLength = length srcLine

  file = Position name line line column column
  msg = tail $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" messages
