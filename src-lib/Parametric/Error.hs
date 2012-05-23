{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances, DeriveDataTypeable #-}
module Parametric.Error
  ( Errors
  , FOmegaError (..)
  , Position (..)
  , annotatePos
  , annotateCode
  , showErrors
  , strMsg
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Error (ErrorList(..))


import Data.Typeable
import Data.Data

import Data.Char
import Data.List (intercalate)

import Tools.Errors.Class

data FOmegaError
  = Error (Maybe Position) (Maybe String) [String] (Maybe [String])

  deriving (Show, Eq)

type Errors = [FOmegaError]

data Position
  = Position String Int Int Int Int
  deriving (Show, Eq, Data, Typeable)

instance ErrorList FOmegaError where
  listMsg msg = pure (Error empty (pure msg) empty empty)

annotateError p' e' m' c' = annotate (map update) where
  update (Error p e m c) = Error (p <|> p') (e <|> e') (m <|> m') (c <|> c')

annotatePos p = annotateError (pure p) empty empty empty
annotateErr e = annotateError empty (pure e) empty empty
annotateMsg m = annotateError empty empty (pure m) empty
annotateCode c = annotateError empty empty empty (pure c)

showErrors :: Errors -> String
showErrors = unlines . map showError

-- prefixes for Tillmann's old jEdit hack
-- probably no longer needed

--   errorPrefix = "[E] "
--   moreLinePrefix = "[-] "

-- prefixes for reasonable error reporting

errorPrefix = ""
moreLinePrefix = "  "


showError :: FOmegaError -> String
showError (Error p e m c) = unlines allLines where
  allLines = firstLine : moreLines
  firstLine = errorPrefix ++ maybePosition ++ head maybeError
  moreLines = map (moreLinePrefix ++) (tail maybeError ++ maybeSource ++ maybeMessages)

  maybePosition = maybe "" showPosition p
  maybeError = lines $ maybe "" (' ' :) e
  maybeSource = maybe [] (" " :) (showSource <$> p <*> c)
  maybeMessages = m

  showSource (Position f r1 r2 c1 c2) src = result where
    result | r1 /= r2 = []
           | otherwise = mark c1 c2 70 (src !! pred r1)

showPosition :: Position -> String
showPosition (Position f r1 r2 c1 c2) = intercalate ":" [f, show r1, ""] --, show r2, show c1, show c2]

mark :: Int -> Int -> Int -> String -> [String]
mark c1 c2 width line = [code, mark] where
  (leaveOut, keep, pre, post, markerLength) = compress c1 c2 width line

  code = pre ++ (take keep . drop leaveOut $ line) ++ post
  mark =    replicate (c1 - leaveOut + length pre - 1) ' '
         ++ replicate markerLength '^'

compress :: Int -> Int -> Int -> String -> (Int, Int, String, String, Int)
compress c1 c2 width text = result where
  prespace = min c1 (length . takeWhile isSpace $ text)
  postspace = length . takeWhile isSpace . reverse . drop c2 $ text
  cropped = drop prespace . take (length text {- - prespace -} - postspace) $ text

  cc1 = c1 - prespace
  cc2 = c2 - prespace

  tlen = length cropped
  clen = c2 - c1 + 1

  left = ((width - clen) + 1) `div` 2
  right = (width - clen) `div` 2

  result | tlen <= width = (prespace, tlen, "", "", clen)
         | cc1 == 1 && clen > width - 3 = (prespace, width - 3, "", "...", width)
         | cc1 > 1 && clen > width - 6 = (c1 - 1, width - 6, "...", "...", width - 3)
         | cc1 - 1 <= left = (prespace, width - 3, "", "...", clen)
         | cc2 <= right = (prespace + tlen - width + 3, width - 3, "...", "", clen)
         | otherwise = (c1 - 1 - left + 3, width - 6, "...", "...", clen)


{-
formatError :: String -> ParseError -> FOmegaError
formatError src err = [file, code, mark, msg] where
  -- extract information
  messages = errorMessages err
  pos = errorPos err
  name = sourceName pos

  (line, column) = convert (sourceLine pos) (sourceColumn pos)
  convert l c = if l > srcLineCount
                  then (min l srcLineCount, succ srcLineLength)
                  else (l, min c (length srcLine))

  srcLines = lines src
  srcLineCount = length srcLines
  srcLine = srcLines !! (pred line)
  srcLineLength = length srcLine

  (leaveOut, howMany, predots, postdots)
    | srcLineLength < 70 = (0, 70, "", "")
    | column < 35 = (0, 67, "", "...")
    | column > srcLineLength - 35 = (srcLineLength - 67, 67, "...", "")
    | otherwise = (column - 33, column + 33, "...", "...")

  file = name ++ ":" ++ show line ++ ":" ++ show column ++ ": Syntax Error"
  code = "  " ++ predots ++ (drop leaveOut . take howMany $ lines src !! pred line) ++ postdots
  mark = "  " ++ replicate (pred column - leaveOut + length predots) ' ' ++ "^"
  msg = unlines . map ("  " ++) . lines . tail $
          showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" messages
 -}
