{-# LANGUAGE DeriveDataTypeable #-}
module PTS.Constants where

import Data.Typeable
import Data.Data

-- constants
newtype C = C Int
  deriving (Eq, Data, Typeable)

int :: C
int = C 0

star :: C
star = C 1

box :: C
box = C 2
