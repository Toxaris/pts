{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, TypeSynonymInstances, DeriveDataTypeable #-}
module PTS.Error
  ( Errors
  , PTSError (..)
  , Position (..)
  , annotatePos
  , annotateCode
  , showErrors
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Errors.Class
import Control.Monad.Trans.Except

import Data.Char
import Data.Data
import Data.List (intercalate)
import Data.Typeable

import Text.Printf (printf)

data PTSError
  = Error (Maybe Position) (Maybe String) [String] (Maybe [String])

  deriving (Show, Eq)

type Errors = [PTSError]

data Position
  = Position String Int Int Int Int
  deriving (Show, Eq, Data, Typeable)

annotateError p' e' m' c' = annotate (map update) where
  update (Error p e m c) = Error (p <|> p') (e <|> e') (m <|> m') (c <|> c')

annotatePos p = annotateError (pure p) empty empty empty
annotateErr e = annotateError empty (pure e) empty empty
annotateMsg m = annotateError empty empty (pure m) empty
annotateCode c = annotateError empty empty empty (pure c)

showErrors :: Errors -> String
showErrors = unlines . map showError

errorPrefix = ""
moreLinePrefix = "  "

showError :: PTSError -> String
showError (Error p e m c) = unlines allLines where
  allLines = firstLine : moreLines
  firstLine = maybePosition ++ head maybeError
  moreLines = map (moreLinePrefix ++) (tail maybeError ++ maybeSource ++ maybeMessages)

  maybePosition = maybe "" ((++ " ") . showPosition) p
  maybeError = lines $ maybe "Unknown Error" id e
  maybeSource = maybe [] (" " :) (showSource <$> p <*> c)
  maybeMessages = m

  showSource (Position f r1 r2 c1 c2) src = result where
    result | r1 /= r2 = []
           | otherwise = mark c1 c2 70 (src !! pred r1)

showPosition :: Position -> String
showPosition (Position f r1 r2 c1 c2)
  = printf "%s:%d.%d-%d.%d:" f r1 c1 r2 c2

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
