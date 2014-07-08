{-# LANGUAGE DeriveDataTypeable #-}
module PTS.Syntax.Constants
  ( C (C)
  , int
  , star
  , box
  , triangle
  ) where

import Data.Data
import Data.Typeable

-- constants
newtype C = C Int
  deriving (Eq, Data, Typeable)

int :: C
int = C 0

star :: C
star = C 1

box :: C
box = C 2

triangle :: C
triangle = C 3
