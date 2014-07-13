module PTS.PrettyToShow where

import PTS.Syntax

instance Show Term where
  show a = showPretty a
